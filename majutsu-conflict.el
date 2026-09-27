;;; majutsu-conflict.el --- Conflict marker parsing for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Portions of conflict refinement/navigation behavior are adapted from:
;; - GNU Emacs `lisp/vc/smerge-mode.el` (commit b37711a25f78a915a10245a6330c3b2b4434b2e5)
;;   Copyright (C) 1999-2026 Free Software Foundation, Inc.

;;; Commentary:

;; This library parses jj conflict markers.  It supports all three styles:
;; - "diff" (jj default): shows diff + snapshot
;; - "snapshot": shows all sides as snapshots
;; - "git": Git's diff3 style (2-sided only)
;;
;; The parser auto-detects the style from file content, matching jj's behavior.

;;; Code:

(require 'cl-lib)
(require 'smerge-mode)

;; Silence byte-compiler warnings for dynamically bound variables
(defvar font-lock-beg)
(defvar font-lock-end)
(defvar diff-refine)
(defvar majutsu-conflict-mode)

;;; Constants

(defconst majutsu-conflict-min-marker-len 7
  "Minimum length of conflict markers.")

;;; Options

(defcustom majutsu-conflict-auto-leave t
  "If non-nil, disable `majutsu-conflict-mode' once every conflict is resolved.
Undoing the resolution re-enables the mode."
  :type 'boolean
  :group 'majutsu)

;;; Marker Regexps
;; Markers must be at least 7 chars, followed by space or EOL

(defconst majutsu-conflict-begin-re
  "^\\(<\\{7,\\}\\)\\(?: \\(.*\\)\\)?$"
  "Regexp matching conflict start marker.")

(defconst majutsu-conflict-end-re
  "^\\(>\\{7,\\}\\)\\(?: \\(.*\\)\\)?$"
  "Regexp matching conflict end marker.")

(defconst majutsu-conflict-add-re
  "^\\(\\+\\{7,\\}\\)\\(?: \\(.*\\)\\)?$"
  "Regexp matching add (side) marker in JJ-style.")

(defconst majutsu-conflict-remove-re
  "^\\(-\\{7,\\}\\)\\(?: \\(.*\\)\\)?$"
  "Regexp matching remove (base) marker in JJ-style.")

(defconst majutsu-conflict-diff-re
  "^\\(%\\{7,\\}\\)\\(?: \\(.*\\)\\)?$"
  "Regexp matching diff marker in JJ-style.")

(defconst majutsu-conflict-note-re
  "^\\(\\\\\\{7,\\}\\)\\(?: \\(.*\\)\\)?$"
  "Regexp matching note (label continuation) marker.")

(defconst majutsu-conflict-git-ancestor-re
  "^\\(|\\{7,\\}\\)\\(?: \\(.*\\)\\)?$"
  "Regexp matching Git-style ancestor (base) marker.")

(defconst majutsu-conflict-git-separator-re
  "^\\(=\\{7,\\}\\)$"
  "Regexp matching Git-style separator marker.")

(defconst majutsu-conflict-label-re
  "^\\([[:alnum:]]+\\) \\([[:xdigit:]]+\\)\\(?: \"\\(.*\\)\"\\)?\\(?: ([^)]*)\\)?$"
  "Regexp matching metadata for a single revision in a JJ conflict label.")

(defconst majutsu-conflict-label-absorbed-re
  "^absorbed changes (from \\([[:alnum:]]+\\) \\([[:xdigit:]]+\\))$"
  "Regexp matching JJ absorb conflict label metadata.")

(defconst majutsu-conflict--marker-chars
  '(?< ?> ?+ ?- ?% ?\\ ?| ?=)
  "Characters used to form conflict marker lines.")

;;; Data Structures

(cl-defstruct (majutsu-conflict (:constructor majutsu-conflict--create))
  "Represents a parsed conflict region."
  begin-pos    ; position of <<<<<<< marker
  end-pos      ; position after >>>>>>> marker
  marker-len   ; length of markers (for regeneration)
  style        ; 'jj-diff, 'jj-snapshot, or 'git
  removes      ; list of (label . content) for bases; base = first remove
  adds)        ; list of (label . content) for sides; jj side #N = nth (1- N)

(defun majutsu-conflict-parse-label (label)
  "Parse JJ LABEL string into a plist.
Return a plist with :change-id, :commit-id, and :description keys.
Return nil for nil or empty LABEL, or when LABEL does not match."
  (when (and (stringp label)
             (not (string= label "")))
    (cond
     ((string-match majutsu-conflict-label-re label)
      (list :change-id (match-string 1 label)
            :commit-id (match-string 2 label)
            :description (match-string 3 label)))
     ((string-match majutsu-conflict-label-absorbed-re label)
      (list :change-id (match-string 1 label)
            :commit-id (match-string 2 label)
            :description "absorbed changes")))))

(defun majutsu-conflict-label-change-id (label)
  "Return the change-id from LABEL, or nil when unavailable."
  (plist-get (majutsu-conflict-parse-label label) :change-id))

(defun majutsu-conflict-label-commit-id (label)
  "Return the commit-id from LABEL, or nil when unavailable."
  (plist-get (majutsu-conflict-parse-label label) :commit-id))

(defun majutsu-conflict-label-description (label)
  "Return the description from LABEL, or nil when unavailable."
  (plist-get (majutsu-conflict-parse-label label) :description))

(defun majutsu-conflict--parse-marker-line (line expected-len)
  "Parse LINE as a conflict marker of at least EXPECTED-LEN.
Return plist with :char, :len and :label keys, or nil if LINE is not a marker."
  (when (and (stringp line)
             (> (length line) 0)
             (memq (aref line 0) majutsu-conflict--marker-chars))
    (let* ((char (aref line 0))
           (line-len (length line))
           (idx 1))
      (while (and (< idx line-len)
                  (= (aref line idx) char))
        (setq idx (1+ idx)))
      (when (>= idx expected-len)
        (if (= idx line-len)
            (list :char char :len idx :label nil)
          (let ((next (aref line idx)))
            (when (memq next '(?\s ?\t))
              (let* ((suffix (substring line idx))
                     (label (replace-regexp-in-string "\\`[ \t]+" "" suffix)))
                (list :char char
                      :len idx
                      :label (unless (string-match-p "\\`[ \t]*\\'" label)
                               label))))))))))

(defun majutsu-conflict--marker-kind (char)
  "Return semantic marker kind for marker CHAR."
  (pcase char
    (?< 'begin)
    (?> 'end)
    (?+ 'add)
    (?- 'remove)
    (?% 'diff)
    (?\\ 'note)
    (?| 'git-ancestor)
    (?= 'git-separator)
    (_ nil)))

(defun majutsu-conflict--line-marker (line expected-len)
  "Return marker information for LINE with EXPECTED-LEN.
The return value is a cons cell of the form (KIND . LABEL), or nil."
  (when-let* ((parsed (majutsu-conflict--parse-marker-line line expected-len))
              (kind (majutsu-conflict--marker-kind (plist-get parsed :char))))
    (cons kind (plist-get parsed :label))))

(defun majutsu-conflict--line-marker-at-point (expected-len)
  "Return marker information for current line using EXPECTED-LEN."
  (majutsu-conflict--line-marker
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))
   expected-len))

(defun majutsu-conflict--normalize-diff-remove-label (label)
  "Strip JJ diff prefix from remove LABEL."
  (let ((label (and label (replace-regexp-in-string "\\`[ \t]+" "" label))))
    (if (and label (string-match "\\`diff from:[ \t]*\\(.*\\)\\'" label))
        (match-string 1 label)
      label)))

(defun majutsu-conflict--normalize-diff-add-label (label)
  "Strip JJ diff prefix from add LABEL."
  (let ((label (and label (replace-regexp-in-string "\\`[ \t]+" "" label))))
    (if (and label (string-match "\\`to:[ \t]*\\(.*\\)\\'" label))
        (match-string 1 label)
      label)))

(defun majutsu-conflict--strip-one-ending-eol (content)
  "Strip one trailing line ending from CONTENT.
If CONTENT ends with LF, drop it. If it ends with CRLF, drop both."
  (if (and (stringp content)
           (> (length content) 0)
           (eq (aref content (1- (length content))) ?\n))
      (let ((without-lf (substring content 0 -1)))
        (if (and (> (length without-lf) 0)
                 (eq (aref without-lf (1- (length without-lf))) ?\r))
            (substring without-lf 0 -1)
          without-lf))
    content))

(defun majutsu-conflict--drop-synthetic-ending-eol (parsed)
  "Undo JJ's synthetic trailing EOL when end marker has no EOL.
PARSED is (STYLE REMOVES ADDS)."
  (dolist (cell (append (cadr parsed) (caddr parsed)))
    (when (consp cell)
      (setcdr cell (majutsu-conflict--strip-one-ending-eol (cdr cell)))))
  parsed)

(defconst majutsu-conflict--jj-marker-kinds '(begin end diff note remove add)
  "Structural marker kinds inside jj-style conflicts.")

(defconst majutsu-conflict--git-marker-kinds '(begin end git-ancestor git-separator)
  "Structural marker kinds inside git-style conflicts.")

(defun majutsu-conflict--scan-terms (conflict fn)
  "Call FN for every line of CONFLICT, in order.
FN receives (TERM KIND LINE-BEG LINE-END) where TERM is a cons of
the term kind (`add' or `remove') and its 0-based index, or nil on
plain marker lines, and KIND is the line's structural marker kind or
nil for content.  Marker lines belonging to the other style count as
content, mirroring jj's per-style hunk parsers.  Terms are counted
in materialization order, so the indices agree with the parsed term
lists for every marker style."
  (let* ((end (majutsu-conflict-end-pos conflict))
         (marker-len (majutsu-conflict-marker-len conflict))
         (git (eq (majutsu-conflict-style conflict) 'git))
         (structural (if git
                         majutsu-conflict--git-marker-kinds
                       majutsu-conflict--jj-marker-kinds))
         (state nil)
         (add-index -1)
         (remove-index -1)
         (done nil))
    (save-excursion
      (goto-char (majutsu-conflict-begin-pos conflict))
      (while (and (not done) (< (point) end))
        (let* ((line-beg (line-beginning-position))
               (line-end (line-end-position))
               (line (buffer-substring-no-properties line-beg line-end))
               (kind (car-safe (majutsu-conflict--line-marker line marker-len)))
               (kind (and (memq kind structural) kind))
               (term
                (pcase kind
                  ('begin
                   ;; A git-style begin marker labels the left side.
                   (when git
                     (setq state 'add
                           add-index 0))
                   (and git '(add . 0)))
                  ('end
                   ;; A git-style end marker labels the right side.
                   (setq done t)
                   (and git '(add . 1)))
                  ('diff
                   ;; A %%%%%%% section reconstructs one base and one side.
                   (setq state 'diff
                         remove-index (1+ remove-index)
                         add-index (1+ add-index))
                   (cons 'remove remove-index))
                  ('note
                   (and (eq state 'diff) (cons 'add add-index)))
                  ('remove
                   (setq state 'remove
                         remove-index (1+ remove-index))
                   (cons 'remove remove-index))
                  ('add
                   (setq state 'add
                         add-index (1+ add-index))
                   (cons 'add add-index))
                  ('git-ancestor
                   (setq state 'remove
                         remove-index 0)
                   (cons 'remove 0))
                  ('git-separator
                   (setq state 'add
                         add-index 1)
                   (cons 'add 1))
                  (_
                   (pcase state
                     ('diff (if (string-prefix-p "-" line)
                                (cons 'remove remove-index)
                              (cons 'add add-index)))
                     ('remove (cons 'remove remove-index))
                     ('add (cons 'add add-index))
                     (_ nil))))))
          (funcall fn term kind line-beg line-end)
          (forward-line 1))))))

(defvar-local majutsu-conflict--term-memo nil
  "Cons of ((TICK . POINT) . TERM) memoizing `majutsu-conflict--term-at-point'.")

(defun majutsu-conflict--term-at-point (conflict)
  "Return the term of CONFLICT at point as a plist, or nil on plain markers.
The plist has :kind (`add' or `remove') and :index (0-based into
`majutsu-conflict-adds' or `majutsu-conflict-removes')."
  (let ((key (cons (buffer-chars-modified-tick) (point))))
    (unless (equal (car majutsu-conflict--term-memo) key)
      (let ((pos (point)))
        (setq majutsu-conflict--term-memo
              (cons key
                    (catch 'hit
                      (majutsu-conflict--scan-terms
                       conflict
                       (lambda (term _kind line-beg line-end)
                         (when (and (<= line-beg pos) (<= pos line-end))
                           (throw 'hit
                                  (and term
                                       (list :kind (car term)
                                             :index (cdr term)))))))
                      nil)))))
    (cdr majutsu-conflict--term-memo)))

(defun majutsu-conflict--goto-term (conflict term)
  "Move point to the first line of CONFLICT belonging to TERM.
With nil TERM, or when TERM cannot be found, move to the conflict's
begin marker."
  (goto-char (majutsu-conflict-begin-pos conflict))
  (when term
    (let ((target (cons (plist-get term :kind) (plist-get term :index))))
      ;; The scan runs inside save-excursion, so collect the position
      ;; first and move afterwards.
      (when-let* ((pos (catch 'hit
                         (majutsu-conflict--scan-terms
                          conflict
                          (lambda (tm _kind line-beg _line-end)
                            (when (equal tm target)
                              (throw 'hit line-beg))))
                         nil)))
        (goto-char pos)))))

(defun majutsu-conflict-term-cell (conflict term)
  "Return the (LABEL . CONTENT) cell of CONFLICT described by TERM."
  (when term
    (nth (plist-get term :index)
         (if (eq (plist-get term :kind) 'remove)
             (majutsu-conflict-removes conflict)
           (majutsu-conflict-adds conflict)))))

(defun majutsu-conflict-display-parts (conflict)
  "Return ordered snapshots and before/after changes encoded by CONFLICT.
Keep the actual pairing of jj diff markers, including a snapshot in any
position.  Adjacent negative and positive snapshots form a change too.
Term references use the plists accepted by `majutsu-conflict-term-cell'.
Native diff bodies retain their source bounds; no merge is recomputed."
  (let ((next-add 0) parts)
    (cl-labels
        ((reference (term)
           (list :kind (car term) :index (cdr term)))
         (snapshot (term)
           (when term
             (let ((ref (reference term)))
               (if (and (eq (car term) 'add)
                        (eq (plist-get (car parts) :kind) 'snapshot)
                        (eq (plist-get (plist-get (car parts) :term) :kind) 'remove))
                   (setcar parts (list :kind 'change
                                       :before (plist-get (car parts) :term)
                                       :after ref))
                 (push (list :kind 'snapshot :term ref) parts))
               (when (eq (car term) 'add)
                 (setq next-add (1+ (cdr term))))))))
      (majutsu-conflict--scan-terms
       conflict
       (lambda (term kind line-beg line-end)
         (pcase kind
           ('diff
            (push (list :kind 'change :before (reference term)
                        :after (list :kind 'add :index next-add) :diff t)
                  parts)
            (setq next-add (1+ next-add)))
           ((or 'begin 'add 'remove 'git-ancestor 'git-separator)
            (snapshot term))
           ('nil
            (when (plist-get (car parts) :diff)
              (unless (plist-get (car parts) :diff-begin)
                (setcar parts (plist-put (car parts) :diff-begin line-beg)))
              (setcar parts (plist-put (car parts) :diff-end (1+ line-end)))))))))
    (nreverse parts)))

(defun majutsu-conflict-revision-at-point ()
  "Return conflict metadata at point as a plist.
Returns a plist with :change-id, :commit-id, and :side, or nil when
point is not inside a labeled term.  :side is `add' for a side,
`remove' for a jj base, and `base' for a git-style ancestor."
  (when-let* ((conflict (majutsu-conflict-at-point))
              (term (majutsu-conflict--term-at-point conflict))
              (cell (majutsu-conflict-term-cell conflict term))
              (parsed (majutsu-conflict-parse-label (car cell))))
    (list :change-id (plist-get parsed :change-id)
          :commit-id (plist-get parsed :commit-id)
          :side (if (eq (plist-get term :kind) 'remove)
                    (if (eq (majutsu-conflict-style conflict) 'git) 'base 'remove)
                  'add))))

;;; Style Detection

(defun majutsu-conflict--detect-style (hunk-start marker-len)
  "Detect conflict style from first line after HUNK-START.
Returns \='jj-diff, \='jj-snapshot, or \='git."
  (save-excursion
    (goto-char hunk-start)
    (forward-line 1)
    (let* ((line (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)))
           (marker (majutsu-conflict--line-marker line marker-len)))
      (pcase (car-safe marker)
        ('diff 'jj-diff)
        ((or 'remove 'add) 'jj-snapshot)
        ('git-ancestor 'git)
        ;; Default to git (content starts directly)
        (_ 'git)))))

;;; JJ-Style Parser

(defun majutsu-conflict--parse-jj-hunk (begin end marker-len)
  "Parse JJ-style conflict hunk between BEGIN and END.
MARKER-LEN is the expected marker length.
Returns (STYLE REMOVES ADDS) or nil if invalid.
ADDS are the conflict sides in jj's side order, whether shown as
+++++++ snapshots or reconstructed from %%%%%%% diffs.  REMOVES are
the bases."
  (save-excursion
    (goto-char begin)
    (forward-line 1)  ; skip <<<<<<< line
    (let ((state 'unknown)
          (style 'jj-snapshot)
          (removes nil)
          (adds nil)
          (current-remove nil)
          (current-add nil)
          (done nil))
      (catch 'invalid
        (cl-labels
            ((append-cell (cell text)
               (setcdr cell (concat (cdr cell) text))))
          (while (and (not done) (< (point) end))
            (let* ((line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
                   (marker (majutsu-conflict--line-marker line marker-len))
                   (kind (car-safe marker))
                   (label (cdr-safe marker)))
              (pcase kind
                ('end
                 (setq done t))
                ('diff
                 (setq style 'jj-diff
                       state 'diff)
                 (push (cons (majutsu-conflict--normalize-diff-remove-label label) "") removes)
                 (push (cons nil "") adds)
                 (setq current-remove (car removes)
                       current-add (car adds)))
                ('remove
                 (setq state 'remove)
                 (push (cons label "") removes)
                 (setq current-remove (car removes)))
                ('add
                 (setq state 'add)
                 (push (cons label "") adds)
                 (setq current-add (car adds)))
                ('note
                 (when (and (eq state 'diff)
                            current-add
                            (null (car current-add)))
                   (setcar current-add
                           (majutsu-conflict--normalize-diff-add-label label))))
                (_
                 (pcase state
                   ('diff
                    (cond
                     ((string-prefix-p "-" line)
                      (append-cell current-remove (concat (substring line 1) "\n")))
                     ((string-prefix-p "+" line)
                      (append-cell current-add (concat (substring line 1) "\n")))
                     ((string-prefix-p " " line)
                      (let ((rest (concat (substring line 1) "\n")))
                        (append-cell current-remove rest)
                        (append-cell current-add rest)))
                     ((string= line "")
                      (append-cell current-remove "\n")
                      (append-cell current-add "\n"))
                     (t
                      (throw 'invalid nil))))
                   ('remove
                    (append-cell current-remove (concat line "\n")))
                   ('add
                    (append-cell current-add (concat line "\n")))
                   (_
                    (throw 'invalid nil))))))
            (unless done
              (forward-line 1))))
        (let ((removes (nreverse removes))
              (adds (nreverse adds)))
          ;; A valid jj conflict materializes N sides and N-1 bases.
          (and (= (length adds) (1+ (length removes)))
               (list style removes adds)))))))

;;; Git-Style Parser

(defun majutsu-conflict--parse-git-hunk (begin end marker-len)
  "Parse Git-style conflict hunk between BEGIN and END.
MARKER-LEN is the expected marker length.
Returns (\='git REMOVES ADDS) or nil if invalid."
  (save-excursion
    (goto-char begin)
    (forward-line 1)  ; skip <<<<<<< line
    (let ((state 'left)
          (left "")
          (base "")
          (right "")
          (left-label nil)
          (base-label nil)
          (right-label nil)
          (done nil))
      ;; Get left label from <<<<<<< line
      (save-excursion
        (goto-char begin)
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
               (marker (majutsu-conflict--line-marker line marker-len)))
          (when (eq (car-safe marker) 'begin)
            (setq left-label (cdr marker)))))

      (catch 'invalid
        (while (and (not done) (< (point) end))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
                 (marker (majutsu-conflict--line-marker line marker-len))
                 (kind (car-safe marker))
                 (label (cdr-safe marker)))
            (cond
             ((eq kind 'end)
              (setq right-label label
                    done t))
             ((eq kind 'git-ancestor)
              (if (eq state 'left)
                  (setq state 'base
                        base-label label)
                (throw 'invalid nil)))
             ((eq kind 'git-separator)
              (if (eq state 'base)
                  (setq state 'right)
                (throw 'invalid nil)))
             (t
              (let ((content (concat line "\n")))
                (pcase state
                  ('left (setq left (concat left content)))
                  ('base (setq base (concat base content)))
                  ('right (setq right (concat right content)))
                  (_ (throw 'invalid nil)))))))
          (unless done
            (forward-line 1)))

        ;; Validate: must have reached right side before end marker
        (if (eq state 'right)
            (list 'git
                  (list (cons base-label base))
                  (list (cons left-label left)
                        (cons right-label right)))
          nil)))))

;;; Main Parser

(defvar-local majutsu-conflict--parse-cache nil
  "Cons of (KEY . CONFLICTS) caching `majutsu-conflict-parse-buffer'.")

(defun majutsu-conflict-parse-buffer ()
  "Parse all conflicts in current buffer.
Returns list of `majutsu-conflict' structs.  The result is cached
until the buffer text or the visible restriction changes."
  (let ((key (list (buffer-chars-modified-tick) (point-min) (point-max))))
    (unless (equal (car majutsu-conflict--parse-cache) key)
      (setq majutsu-conflict--parse-cache
            (cons key (majutsu-conflict--parse-buffer))))
    (cdr majutsu-conflict--parse-cache)))

(defun majutsu-conflict--parse-buffer ()
  "Parse all conflicts in current buffer without caching."
  (save-excursion
    (goto-char (point-min))
    (let (conflicts)
      (while (re-search-forward majutsu-conflict-begin-re nil t)
        (let* ((begin (match-beginning 0))
               (marker-len (length (match-string 1)))
               (end-re (format "^>\\{%d,\\}\\(?: .*\\)?$" marker-len)))
          ;; Find matching end marker
          (when (re-search-forward end-re nil t)
            (let* ((match-end (match-end 0))
                   (marker-has-eol (and (< match-end (point-max))
                                        (eq (char-after match-end) ?\n)))
                   (end (if marker-has-eol
                            (1+ match-end)
                          match-end))
                   (style (majutsu-conflict--detect-style begin marker-len))
                   (parsed (if (eq style 'git)
                               (majutsu-conflict--parse-git-hunk begin end marker-len)
                             (majutsu-conflict--parse-jj-hunk begin end marker-len))))
              (when (and parsed (not marker-has-eol))
                ;; jj compensates missing trailing newlines by omitting EOL on the
                ;; end marker; strip one synthetic trailing EOL from each term.
                (setq parsed (majutsu-conflict--drop-synthetic-ending-eol parsed)))
              (when parsed
                (push (majutsu-conflict--create
                       :begin-pos begin
                       :end-pos end
                       :marker-len marker-len
                       :style (car parsed)
                       :removes (cadr parsed)
                       :adds (caddr parsed))
                      conflicts))))))
      (nreverse conflicts))))

(defun majutsu-conflict-at-point ()
  "Return the conflict at point, or nil."
  (let ((pos (point))
        (conflicts (majutsu-conflict-parse-buffer)))
    (cl-find-if (lambda (c)
                  (and (<= (majutsu-conflict-begin-pos c) pos)
                       (< pos (majutsu-conflict-end-pos c))))
                conflicts)))

(defun majutsu-conflict-goto-nearest ()
  "Move point to the nearest conflict marker in the buffer.
Prefer the current conflict when point is inside one."
  (interactive)
  (let ((conflict (majutsu-conflict-at-point)))
    (cond
     (conflict
      (goto-char (majutsu-conflict-begin-pos conflict)))
     ((re-search-forward majutsu-conflict-begin-re nil t)
      (goto-char (match-beginning 0)))
     ((re-search-backward majutsu-conflict-begin-re nil t)
      (goto-char (match-beginning 0)))
     (t
      (user-error "No conflict markers in buffer")))))

;;; Navigation

(defun majutsu-conflict-next ()
  "Move to the next conflict marker."
  (interactive)
  (let ((pos (point)))
    (when (looking-at majutsu-conflict-begin-re)
      (forward-char))
    (if (re-search-forward majutsu-conflict-begin-re nil t)
        (progn
          (goto-char (match-beginning 0))
          (when diff-refine
            (ignore-errors (majutsu-conflict-refine))))
      (goto-char pos)
      (user-error "No more conflicts"))))

(defun majutsu-conflict-prev ()
  "Move to the previous conflict marker."
  (interactive)
  (if (re-search-backward majutsu-conflict-begin-re nil t)
      (when diff-refine
        (ignore-errors (majutsu-conflict-refine)))
    (user-error "No previous conflict")))

;;; Resolution Helpers

(defun majutsu-conflict-resolve-with (conflict content)
  "Replace CONFLICT region with CONTENT."
  (save-excursion
    (delete-region (majutsu-conflict-begin-pos conflict)
                   (majutsu-conflict-end-pos conflict))
    (goto-char (majutsu-conflict-begin-pos conflict))
    (insert content))
  (majutsu-conflict--auto-leave))

(defun majutsu-conflict--auto-leave ()
  "Disable `majutsu-conflict-mode' when no conflict remains.
Enabled by `majutsu-conflict-auto-leave'; undo re-enables the mode.
The whole buffer is checked, not just the current restriction."
  (when (and majutsu-conflict-auto-leave
             majutsu-conflict-mode
             (save-restriction
               (widen)
               (null (majutsu-conflict-parse-buffer))))
    (when (listp buffer-undo-list)
      (push (list 'apply 'majutsu-conflict-mode 1) buffer-undo-list))
    (majutsu-conflict-mode -1)))

(defun majutsu-conflict-keep-side (n &optional before)
  "Keep side N (1-indexed) of the conflict at point.
Without prefix, keep jj side #N.  With prefix arg (BEFORE non-nil),
keep base N instead: bases are numbered in materialization order and
are not paired with the side numbers."
  (interactive "p\nP")
  (let ((conflict (majutsu-conflict-at-point)))
    (unless conflict
      (user-error "No conflict at point"))
    (let* ((adds (majutsu-conflict-adds conflict))
           (removes (majutsu-conflict-removes conflict))
           (content (if before
                        (cdr (nth (1- n) removes))
                      (cdr (nth (1- n) adds)))))
      (unless content
        (user-error (if before "No base %d" "No side %d") n))
      (majutsu-conflict-resolve-with conflict content))))

(defun majutsu-conflict-keep-base ()
  "Keep the base of the conflict at point.
The base is the first remove term (jj's merge base, or the
ancestor section of a git-style conflict)."
  (interactive)
  (let ((conflict (majutsu-conflict-at-point)))
    (unless conflict
      (user-error "No conflict at point"))
    (let ((base (car (majutsu-conflict-removes conflict))))
      (unless base
        (user-error "No base in this conflict"))
      (majutsu-conflict-resolve-with conflict (cdr base)))))

(defun majutsu-conflict-keep-current ()
  "Keep the conflict term at point, side or base."
  (interactive)
  (let ((conflict (majutsu-conflict-at-point)))
    (unless conflict
      (user-error "No conflict at point"))
    (let ((cell (majutsu-conflict-term-cell
                 conflict (majutsu-conflict--term-at-point conflict))))
      (unless cell
        (user-error "Point is not inside a conflict term"))
      (majutsu-conflict-resolve-with conflict (cdr cell))
      (when (car cell)
        (message "Kept %s" (car cell))))))

(defun majutsu-conflict-keep-all (&optional confirm)
  "Concatenate every side of the conflict at point, in side order.
Each side is a complete snapshot, so shared context is duplicated.  When
CONFIRM is non-nil, confirm before concatenating a conflict with a nonempty
base.  Interactive calls set CONFIRM.  A side without a trailing newline
marks the end of the file, so a separating newline is added only when more
content follows."
  (interactive (list t))
  (let ((conflict (majutsu-conflict-at-point)))
    (unless conflict
      (user-error "No conflict at point"))
    (when (and confirm
               (cl-some (lambda (cell)
                          (not (string-empty-p (cdr cell))))
                        (majutsu-conflict-removes conflict))
               (not (yes-or-no-p
                     "Sides are complete snapshots; concatenate them anyway? ")))
      (user-error "Side concatenation cancelled"))
    (majutsu-conflict-resolve-with
     conflict
     (let ((parts nil)
           (content-follows nil))
       (dolist (cell (reverse (majutsu-conflict-adds conflict)))
         (let ((content (cdr cell)))
           (unless (string-empty-p content)
             (push (if (and content-follows
                            (not (eq (aref content (1- (length content))) ?\n)))
                       (concat content "\n")
                     content)
                   parts)
             (setq content-follows t))))
       (apply #'concat parts)))))

;;; Marker Style Conversion

(defun majutsu-conflict--marker-line (conflict char &optional label)
  "Return a marker line for CONFLICT built from CHAR and LABEL."
  (concat (make-string (majutsu-conflict-marker-len conflict) char)
          (and label (not (string-empty-p label)) (concat " " label))
          "\n"))

(defun majutsu-conflict--info-labels (conflict)
  "Return (BEGIN . END) marker labels for CONFLICT's jj-style rendering.
Reuse the buffer's begin/end marker labels when they are jj-style;
git-style conflicts carry side labels there instead, so fall back to
a generic label."
  (if (eq (majutsu-conflict-style conflict) 'git)
      (cons "conflict" "conflict ends")
    (let ((marker-len (majutsu-conflict-marker-len conflict)))
      (save-excursion
        (goto-char (majutsu-conflict-begin-pos conflict))
        (let ((begin-label (cdr (majutsu-conflict--line-marker-at-point marker-len))))
          (goto-char (1- (majutsu-conflict-end-pos conflict)))
          (cons begin-label
                (cdr (majutsu-conflict--line-marker-at-point marker-len))))))))

(defun majutsu-conflict--materialize (conflict style)
  "Return CONFLICT rendered with STYLE markers, including begin/end lines.
STYLE is `jj-snapshot' or `git'.  Follows jj's materialization
semantics: when any term lacks a trailing newline, every term gains
one as a separator and the end marker loses its own."
  (let* ((adds (majutsu-conflict-adds conflict))
         (removes (majutsu-conflict-removes conflict))
         (all-eol (cl-every (lambda (cell)
                              (let ((content (cdr cell)))
                                (or (string-empty-p content)
                                    (eq (aref content (1- (length content))) ?\n))))
                            (append removes adds)))
         (term (lambda (cell)
                 (if all-eol (cdr cell) (concat (cdr cell) "\n"))))
         (text
          (pcase-exhaustive style
            ('jj-snapshot
             (pcase-let ((`(,begin-label . ,end-label)
                          (majutsu-conflict--info-labels conflict)))
               (concat
                (majutsu-conflict--marker-line conflict ?< begin-label)
                (majutsu-conflict--marker-line conflict ?+ (caar adds))
                (funcall term (car adds))
                (mapconcat (lambda (pair)
                             (concat
                              (majutsu-conflict--marker-line conflict ?- (car (car pair)))
                              (funcall term (car pair))
                              (majutsu-conflict--marker-line conflict ?+ (car (cdr pair)))
                              (funcall term (cdr pair))))
                           (cl-mapcar #'cons removes (cdr adds))
                           "")
                (majutsu-conflict--marker-line conflict ?> end-label))))
            ('git
             (unless (and (= 2 (length adds)) (= 1 (length removes)))
               (user-error "Git-style markers require a 2-sided conflict"))
             (concat
              (majutsu-conflict--marker-line conflict ?< (caar adds))
              (funcall term (car adds))
              (majutsu-conflict--marker-line conflict ?| (caar removes))
              (funcall term (car removes))
              (majutsu-conflict--marker-line conflict ?=)
              (funcall term (cadr adds))
              (majutsu-conflict--marker-line conflict ?> (car (cadr adds))))))))
    (if all-eol text (substring text 0 -1))))

(defun majutsu-conflict--content-collision (conflict style)
  "Return a term line of CONFLICT that clashes with STYLE's markers.
A content line that would itself parse as one of STYLE's structural
markers at this conflict's marker length cannot be re-rendered in
STYLE: jj (and Majutsu) would mis-parse the result."
  (let ((kinds (if (eq style 'git)
                   majutsu-conflict--git-marker-kinds
                 majutsu-conflict--jj-marker-kinds))
        (marker-len (majutsu-conflict-marker-len conflict)))
    (catch 'hit
      (dolist (cell (append (majutsu-conflict-removes conflict)
                            (majutsu-conflict-adds conflict)))
        (dolist (line (split-string (cdr cell) "\n"))
          (when (memq (car-safe (majutsu-conflict--line-marker line marker-len))
                      kinds)
            (throw 'hit line))))
      nil)))

(defun majutsu-conflict--region-refined-p (beg end)
  "Return non-nil when refine overlays exist between BEG and END."
  (seq-some (lambda (ov) (overlay-get ov 'majutsu-conflict-refine))
            (overlays-in beg end)))

(defun majutsu-conflict-restyle ()
  "Re-render the conflict at point with different conflict markers.
Converts jj's diff style to snapshot style, and toggles between
snapshot and git styles for 2-sided conflicts.  jj parses every
marker style regardless of `ui.conflict-marker-style' (the style is
detected per conflict from its first line), so this only changes the
presentation; the recorded conflict stays the same."
  (interactive)
  (let ((conflict (majutsu-conflict-at-point)))
    (unless conflict
      (user-error "No conflict at point"))
    (let* ((sides (length (majutsu-conflict-adds conflict)))
           (target (pcase (majutsu-conflict-style conflict)
                     ('jj-diff 'jj-snapshot)
                     ('git 'jj-snapshot)
                     ('jj-snapshot
                      (if (= sides 2)
                          'git
                        (user-error
                         "Snapshot is the only marker style for a %d-sided conflict"
                         sides))))))
      (when-let* ((collision (majutsu-conflict--content-collision conflict target)))
        (user-error "A term contains a %s marker line (%s); refusing to restyle"
                    (if (eq target 'git) "git-style" "jj-style")
                    (truncate-string-to-width collision 20 nil nil "…")))
      (let ((term (majutsu-conflict--term-at-point conflict))
            (begin (majutsu-conflict-begin-pos conflict))
            (refined (majutsu-conflict--region-refined-p
                      (majutsu-conflict-begin-pos conflict)
                      (majutsu-conflict-end-pos conflict)))
            (text (majutsu-conflict--materialize conflict target)))
        (save-excursion
          (goto-char begin)
          (delete-region begin (majutsu-conflict-end-pos conflict))
          (insert text))
        (goto-char begin)
        (when-let* ((renewed (majutsu-conflict-at-point)))
          (majutsu-conflict--goto-term renewed term))
        (when refined
          (ignore-errors (majutsu-conflict-refine)))
        (majutsu-conflict-ensure-mode)
        (message "Conflict re-rendered with %s markers"
                 (if (eq target 'git) "git" "snapshot"))))))

;;; Minor Mode

(defvar-local majutsu-conflict--overlays nil
  "List of overlays for conflict highlighting (refine only).")

(defvar-local majutsu-conflict--in-diff nil
  "Non-nil when inside a diff section during font-lock.")

(defvar-local majutsu-conflict--in-add nil
  "Non-nil when inside a ++++++ section.")

(defvar-local majutsu-conflict--in-remove nil
  "Non-nil when inside a ------ section.")

(defvar-local majutsu-conflict--side-count 0
  "Number of side sections seen so far in the current conflict.")

(defvar-local majutsu-conflict--style nil
  "Marker style of the conflict the font-lock state machine is inside.")

(defvar-local majutsu-conflict--marker-len nil
  "Current conflict marker length used by font-lock state machine.")

(defun majutsu-conflict--bind-side-keys (map prefix before)
  "Bind PREFIX 1-9 in MAP to conflict side commands.
When BEFORE is non-nil, keep the before side."
  (dotimes (i 9)
    (let ((n (1+ i)))
      (keymap-set map (format "%s%d" prefix n)
                  (lambda () (interactive)
                    (majutsu-conflict-keep-side n before))))))

(defvar-keymap majutsu-conflict-mode-map
  :doc "Keymap for `majutsu-conflict-mode'.
\\<majutsu-conflict-mode-map>
\\[majutsu-conflict-next] - next conflict
\\[majutsu-conflict-prev] - previous conflict
\\[majutsu-conflict-keep-current] - keep the term at point
\\[majutsu-conflict-keep-base] - keep base (the merge ancestor)
\\[majutsu-conflict-keep-all] - concatenate all side snapshots
\\[majutsu-conflict-restyle] - re-render with different markers
\\[majutsu-conflict-refine] - add word-level refinement
C-c ^ 1-9 - keep side N
C-c ^ M-1 to M-9 - keep base N"
  "C-c ^ n" #'majutsu-conflict-next
  "C-c ^ p" #'majutsu-conflict-prev
  "C-c ^ RET" #'majutsu-conflict-keep-current
  "C-c ^ b" #'majutsu-conflict-keep-base
  "C-c ^ a" #'majutsu-conflict-keep-all
  "C-c ^ s" #'majutsu-conflict-restyle
  "C-c ^ R" #'majutsu-conflict-refine)

(majutsu-conflict--bind-side-keys majutsu-conflict-mode-map "C-c ^ " nil)
(majutsu-conflict--bind-side-keys majutsu-conflict-mode-map "C-c ^ M-" t)

(defun majutsu-conflict--git-style-only-p ()
  "Return non-nil if buffer only contains Git-style conflicts."
  (let ((conflicts (majutsu-conflict-parse-buffer)))
    (and conflicts
         (cl-every (lambda (c) (eq (majutsu-conflict-style c) 'git))
                   conflicts))))

(defun majutsu-conflict--clear-overlays ()
  "Remove all conflict overlays."
  (mapc #'delete-overlay majutsu-conflict--overlays)
  (setq majutsu-conflict--overlays nil)
  ;; Remove refine overlays created by smerge-refine-regions
  (remove-overlays (point-min) (point-max) 'majutsu-conflict-refine t))

(defface majutsu-conflict-marker-face
  '((((background light))
     (:background "grey85" :extend t))
    (((background dark))
     (:background "grey30" :extend t)))
  "Face for conflict marker lines."
  :group 'majutsu)

(defface majutsu-conflict-base-face
  '((default :inherit smerge-base :extend t))
  "Face for base content in conflicts."
  :group 'majutsu)

(defface majutsu-conflict-context-face
  '((((background light))
     (:background "grey95" :extend t))
    (((background dark))
     (:background "grey25" :extend t)))
  "Face for context lines in conflict diffs."
  :group 'majutsu)

(defface majutsu-conflict-added-face
  '((((class color) (min-colors 88) (background light))
     :background "#ddffdd" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#335533" :extend t)
    (((class color))
     :foreground "green" :extend t))
  "Face for added lines in conflict diffs."
  :group 'majutsu)

(defface majutsu-conflict-removed-face
  '((((class color) (min-colors 88) (background light))
     :background "#ffdddd" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#553333" :extend t)
    (((class color))
     :foreground "red" :extend t))
  "Face for removed lines in conflict diffs."
  :group 'majutsu)

(defface majutsu-conflict-refined-added
  '((default :inherit diff-refine-added)
    (((class color) (min-colors 88) (background light))
     :background "#aaffaa")
    (((class color) (min-colors 88) (background dark))
     :background "#22aa22"))
  "Face for refined added regions in conflict diffs."
  :group 'majutsu)

(defface majutsu-conflict-refined-removed
  '((default :inherit diff-refine-removed)
    (((class color) (min-colors 88) (background light))
     :background "#ffbbbb")
    (((class color) (min-colors 88) (background dark))
     :background "#aa2222"))
  "Face for refined removed regions in conflict diffs."
  :group 'majutsu)

(defface majutsu-conflict-side-1-face
  '((default :inherit smerge-upper :extend t))
  "Face for the first (and every third further) conflict side."
  :group 'majutsu)

(defface majutsu-conflict-side-2-face
  '((default :inherit smerge-lower :extend t))
  "Face for the second (and every third further) conflict side."
  :group 'majutsu)

(defface majutsu-conflict-side-3-face
  '((((class color) (min-colors 88) (background light))
     :background "#ddddff" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#333366" :extend t)
    (((class color))
     :foreground "blue" :extend t))
  "Face for the third (and every third further) conflict side."
  :group 'majutsu)

(defvar majutsu-conflict-side-faces
  '(majutsu-conflict-side-1-face
    majutsu-conflict-side-2-face
    majutsu-conflict-side-3-face)
  "Faces for conflict side snapshots, cycled by side number.")

(defun majutsu-conflict--side-face (n)
  "Return the face for 0-based side number N."
  (nth (mod n (length majutsu-conflict-side-faces))
       majutsu-conflict-side-faces))

;;; Font-Lock Support

(defvar majutsu-conflict--region-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-3] #'majutsu-conflict-popup-context-menu)
    map)
  "Keymap active on conflict regions via their region overlay.")

(defun majutsu-conflict--place-region-overlay (begin end)
  "Ensure one region overlay with menu and help props spans BEGIN..END.
Overlays, unlike text properties, disappear together with the text
they cover and never linger on killed or yanked copies."
  (dolist (overlay (overlays-in begin end))
    (when (overlay-get overlay 'majutsu-conflict-region)
      (delete-overlay overlay)))
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'majutsu-conflict-region t)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'keymap majutsu-conflict--region-map)
    (overlay-put overlay 'help-echo "jj conflict: mouse-3 shows a resolution menu")
    overlay))

(defun majutsu-conflict--remove-region-overlays (&optional beg end)
  "Remove conflict region overlays between BEG and END (whole buffer)."
  (save-restriction
    (widen)
    (remove-overlays (or beg (point-min)) (or end (point-max))
                     'majutsu-conflict-region t)))

(defun majutsu-conflict--find-conflict (&optional limit)
  "Match the next parseable conflict.  Intended as a font-lock MATCHER.
Matches are driven by `majutsu-conflict-parse-buffer', so regions the
parser rejects are never highlighted or given a context menu.
Returns non-nil if a match is found between point and LIMIT.
Sets match-data with group 0 = entire conflict."
  (let* ((limit (or limit (point-max)))
         (conflict (cl-find-if
                    (lambda (c)
                      (and (>= (majutsu-conflict-begin-pos c) (point))
                           (< (majutsu-conflict-begin-pos c) limit)))
                    (majutsu-conflict-parse-buffer))))
    (when conflict
      (let ((begin (majutsu-conflict-begin-pos conflict))
            (end (majutsu-conflict-end-pos conflict)))
        (set-match-data (list begin end))
        (goto-char end)
        (with-silent-modifications
          (put-text-property begin end 'font-lock-multiline t))
        (majutsu-conflict--place-region-overlay begin end)
        t))))

(defun majutsu-conflict--match-line (limit)
  "Match any line within JJ conflict.  Font-lock ANCHORED-MATCHER.
Sets match-data and updates state variables."
  (when (< (point) limit)
    (let* ((line-beg (line-beginning-position))
           (line-end (line-end-position))
           (line (buffer-substring-no-properties line-beg line-end))
           (expected-len (or majutsu-conflict--marker-len
                             majutsu-conflict-min-marker-len))
           (parsed (majutsu-conflict--parse-marker-line line expected-len))
           (kind (and parsed
                      (majutsu-conflict--marker-kind (plist-get parsed :char))))
           ;; Marker lines of the other style are content, mirroring
           ;; jj's per-style hunk parsers.
           (kind (and (memq kind (if (eq majutsu-conflict--style 'git)
                                     majutsu-conflict--git-marker-kinds
                                   majutsu-conflict--jj-marker-kinds))
                      kind)))
      (pcase kind
        ;; A git-style begin marker is directly followed by side #1's
        ;; content; jj-style conflicts see their first section marker
        ;; before any content line appears.
        ('begin
         (setq majutsu-conflict--marker-len (plist-get parsed :len)
               majutsu-conflict--style (majutsu-conflict--detect-style
                                        line-beg (plist-get parsed :len))
               majutsu-conflict--in-diff nil
               majutsu-conflict--in-remove nil
               majutsu-conflict--in-add (eq majutsu-conflict--style 'git)
               majutsu-conflict--side-count
               (if (eq majutsu-conflict--style 'git) 1 0)))
        ('end
         (setq majutsu-conflict--in-diff nil
               majutsu-conflict--in-add nil
               majutsu-conflict--in-remove nil
               majutsu-conflict--side-count 0
               majutsu-conflict--style nil
               majutsu-conflict--marker-len nil))
        ('diff
         ;; A %%%%%%% section reconstructs one side.
         (setq majutsu-conflict--in-diff t
               majutsu-conflict--in-add nil
               majutsu-conflict--in-remove nil
               majutsu-conflict--side-count (1+ majutsu-conflict--side-count)))
        ('add
         (setq majutsu-conflict--in-diff nil
               majutsu-conflict--in-add t
               majutsu-conflict--in-remove nil
               majutsu-conflict--side-count (1+ majutsu-conflict--side-count)))
        ('remove
         (setq majutsu-conflict--in-diff nil
               majutsu-conflict--in-add nil
               majutsu-conflict--in-remove t))
        ('git-ancestor
         (setq majutsu-conflict--in-diff nil
               majutsu-conflict--in-add nil
               majutsu-conflict--in-remove t))
        ('git-separator
         (setq majutsu-conflict--in-diff nil
               majutsu-conflict--in-add t
               majutsu-conflict--in-remove nil
               majutsu-conflict--side-count 2)))
      (set-match-data (list line-beg (min (1+ line-end) (point-max))))
      (goto-char (min (1+ line-end) (point-max)))
      t)))

(defun majutsu-conflict--line-face ()
  "Return face for current line based on state and content."
  (save-excursion
    (goto-char (match-beginning 0))
    (let ((marker (majutsu-conflict--line-marker-at-point
                   (or majutsu-conflict--marker-len
                       majutsu-conflict-min-marker-len))))
      (cond
       ;; Marker lines of the conflict's own style
       ((memq (car-safe marker)
              (if (eq majutsu-conflict--style 'git)
                  majutsu-conflict--git-marker-kinds
                majutsu-conflict--jj-marker-kinds))
        'majutsu-conflict-marker-face)
       ;; JJ diff style content
       (majutsu-conflict--in-diff
        (cond
         ((looking-at "^\\+") 'majutsu-conflict-added-face)
         ((looking-at "^-") 'majutsu-conflict-removed-face)
         ((looking-at "^ ") 'majutsu-conflict-context-face)
         (t nil)))
       ;; Side snapshot content: one face per side number
       (majutsu-conflict--in-add
        (majutsu-conflict--side-face
         (1- (max 1 majutsu-conflict--side-count))))
       ;; Base snapshot content
       (majutsu-conflict--in-remove
        'majutsu-conflict-base-face)
       (t nil)))))

(defconst majutsu-conflict-font-lock-keywords
  `((majutsu-conflict--find-conflict
     (majutsu-conflict--match-line
      ;; PRE-FORM: reset state and return conflict end as search limit
      (progn
        (setq majutsu-conflict--in-diff nil
              majutsu-conflict--in-add nil
              majutsu-conflict--in-remove nil
              majutsu-conflict--side-count 0
              majutsu-conflict--style nil
              majutsu-conflict--marker-len nil)
        (goto-char (match-beginning 0))
        (match-end 0))  ; Return conflict end position as limit
      nil
      (0 (majutsu-conflict--line-face) prepend t))))
  "Font lock keywords for `majutsu-conflict-mode'.")

;;; Context Menu and Eldoc

(defun majutsu-conflict--menu-item-label (prefix cell)
  "Return a menu label with PREFIX for term CELL."
  (concat prefix
          (when (car cell)
            (concat ": " (truncate-string-to-width (car cell) 50 nil nil "…")))))

(defun majutsu-conflict--context-menu (conflict)
  "Return a context menu for resolving CONFLICT."
  (easy-menu-create-menu
   "jj Conflict"
   (append
    (list ["Keep This" majutsu-conflict-keep-current t])
    (cl-loop for cell in (majutsu-conflict-adds conflict)
             for n from 1
             collect (let ((n n))
                       (vector (majutsu-conflict--menu-item-label
                                (format "Keep Side #%d" n) cell)
                               (lambda ()
                                 (interactive)
                                 (majutsu-conflict-keep-side n))
                               t)))
    (cl-loop for cell in (majutsu-conflict-removes conflict)
             for n from 1
             collect (let ((n n))
                       (vector (majutsu-conflict--menu-item-label
                                (format "Keep Base %d" n) cell)
                               (lambda ()
                                 (interactive)
                                 (majutsu-conflict-keep-side n t))
                               t)))
    (list ["Concatenate All Sides" majutsu-conflict-keep-all t]
          "--"
          ["Re-render Markers" majutsu-conflict-restyle t]
          ["Refine Highlighting" majutsu-conflict-refine t]))))

(defun majutsu-conflict-popup-context-menu (event)
  "Pop up a resolution menu for the conflict under EVENT.
When `context-menu-mode' is enabled, show the composed standard
context menu instead; the conflict entries join it through
`context-menu-functions'."
  (interactive "e")
  (posn-set-point (event-end event))
  (let ((conflict (majutsu-conflict-at-point)))
    (cond
     ((not conflict)
      ;; Not a conflict any more: drop the stale overlay under point
      ;; only, leaving every other conflict's menu alone.
      (dolist (overlay (overlays-at (point)))
        (when (overlay-get overlay 'majutsu-conflict-region)
          (delete-overlay overlay)))
      (push event unread-command-events))
     ((and (bound-and-true-p context-menu-mode)
           (fboundp 'context-menu-map))
      (popup-menu (context-menu-map event)))
     (t
      (popup-menu (majutsu-conflict--context-menu conflict))))))

(defun majutsu-conflict-context-menu (menu click)
  "Add conflict resolution entries to MENU when CLICK hits a conflict.
Intended for `context-menu-functions'."
  (save-excursion
    (posn-set-point (event-start click))
    (when-let* ((conflict (majutsu-conflict-at-point)))
      (define-key menu [majutsu-conflict-separator] '(menu-item "--"))
      (define-key menu [majutsu-conflict]
                  (list 'menu-item "jj Conflict"
                        (majutsu-conflict--context-menu conflict)))))
  menu)

(defun majutsu-conflict--eldoc (&rest _)
  "Describe the conflict term at point for eldoc."
  (when-let* ((conflict (majutsu-conflict-at-point))
              (term (majutsu-conflict--term-at-point conflict))
              (cell (majutsu-conflict-term-cell conflict term)))
    (let ((removep (eq (plist-get term :kind) 'remove)))
      (concat (format "jj conflict %s %d/%d"
                      (if removep "base" "side")
                      (1+ (plist-get term :index))
                      (length (if removep
                                  (majutsu-conflict-removes conflict)
                                (majutsu-conflict-adds conflict))))
              (when (car cell)
                (concat ": " (car cell)))))))

(defun majutsu-conflict--add-overlay (beg end face)
  "Add refine overlay from BEG to END with FACE."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'majutsu-conflict t)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'priority 100)
    (push ov majutsu-conflict--overlays)))

(defun majutsu-conflict--clear-refine-overlays (beg end)
  "Remove refinement overlays between BEG and END."
  (remove-overlays beg end 'majutsu-conflict-refine t))

(defun majutsu-conflict--refine-pair (remove-beg remove-end add-beg add-end)
  "Refine a removed/added region pair.
REMOVE-BEG/REMOVE-END and ADD-BEG/ADD-END are content-only ranges (no +/-)."
  (when (and remove-beg remove-end add-beg add-end
             (< remove-beg remove-end) (< add-beg add-end))
    (let ((props-r `((majutsu-conflict-refine . t)
                     (face . majutsu-conflict-refined-removed)))
          (props-a `((majutsu-conflict-refine . t)
                     (face . majutsu-conflict-refined-added)))
          (smerge-refine-ignore-whitespace t)
          (write-region-inhibit-fsync t))
      (smerge-refine-regions remove-beg remove-end add-beg add-end
                             nil nil props-r props-a)
      (dolist (ov (overlays-in remove-beg add-end))
        (when (overlay-get ov 'majutsu-conflict-refine)
          (overlay-put ov 'priority 100))))))

(defun majutsu-conflict--refine-diff-region (beg end)
  "Apply word-level refinement to diff lines between BEG and END."
  (save-excursion
    (majutsu-conflict--clear-refine-overlays beg end)
    (goto-char beg)
    (let ((remove-beg nil)
          (remove-end nil)
          (add-beg nil)
          (add-end nil))
      (cl-labels
          ((flush ()
             (majutsu-conflict--refine-pair
              remove-beg remove-end add-beg add-end)
             (setq remove-beg nil remove-end nil
                   add-beg nil add-end nil)))
        (while (< (point) end)
          (cond
           ((looking-at "^-")
            (when add-beg (flush))
            (let ((beg (1+ (line-beginning-position)))
                  (end (min end (1+ (line-end-position)))))
              (if remove-beg
                  (setq remove-end end)
                (setq remove-beg beg
                      remove-end end))))
           ((looking-at "^+")
            (let ((beg (1+ (line-beginning-position)))
                  (end (min end (1+ (line-end-position)))))
              (if add-beg
                  (setq add-end end)
                (setq add-beg beg
                      add-end end))))
           (t
            (flush)))
          (forward-line 1))
        (flush)))))

(defun majutsu-conflict--refine-diffs ()
  "Add word-level refinement to JJ diff sections."
  (dolist (conflict (majutsu-conflict-parse-buffer))
    (when (eq (majutsu-conflict-style conflict) 'jj-diff)
      (save-excursion
        (let ((end (majutsu-conflict-end-pos conflict))
              (marker-len (majutsu-conflict-marker-len conflict)))
          (goto-char (majutsu-conflict-begin-pos conflict))
          (forward-line 1)
          (while (< (point) end)
            (pcase (car-safe (majutsu-conflict--line-marker-at-point marker-len))
              ('diff
               ;; Skip marker line and optional note line.
               (forward-line 1)
               (when (and (< (point) end)
                          (eq (car-safe (majutsu-conflict--line-marker-at-point marker-len))
                              'note))
                 (forward-line 1))
               (let ((content-start (point)))
                 (while (and (< (point) end)
                             (not (majutsu-conflict--line-marker-at-point marker-len)))
                   (forward-line 1))
                 (let ((content-end (point)))
                   (when (> content-end content-start)
                     (majutsu-conflict--refine-diff-region content-start content-end)))))
              (_
               (forward-line 1)))))))))

(defun majutsu-conflict--refine-snapshots ()
  "Add word-level refinement to JJ snapshot-style conflicts.
Compares each ------- section with the following +++++++ section."
  (dolist (conflict (majutsu-conflict-parse-buffer))
    (when (eq (majutsu-conflict-style conflict) 'jj-snapshot)
      (save-excursion
        (let ((end (majutsu-conflict-end-pos conflict))
              (marker-len (majutsu-conflict-marker-len conflict)))
          (goto-char (majutsu-conflict-begin-pos conflict))
          (forward-line 1)
          (while (< (point) end)
            (if (eq (car-safe (majutsu-conflict--line-marker-at-point marker-len))
                    'remove)
                (progn
                  (forward-line 1)
                  (let ((remove-start (point)))
                    (while (and (< (point) end)
                                (not (majutsu-conflict--line-marker-at-point marker-len)))
                      (forward-line 1))
                    (let ((remove-end (point)))
                      (when (and (< (point) end)
                                 (eq (car-safe (majutsu-conflict--line-marker-at-point marker-len))
                                     'add))
                        (forward-line 1)
                        (let ((add-start (point)))
                          (while (and (< (point) end)
                                      (not (majutsu-conflict--line-marker-at-point marker-len)))
                            (forward-line 1))
                          (let ((add-end (point)))
                            (when (and (< remove-start remove-end)
                                       (< add-start add-end))
                              (majutsu-conflict--refine-pair
                               remove-start remove-end add-start add-end))))))))
              (forward-line 1))))))))

(defun majutsu-conflict--fontify-region (beg end)
  "Force font-lock to fontify BEG to END."
  (with-demoted-errors "%S"
    (font-lock-fontify-region beg end nil)))

(defun majutsu-conflict--fontify-conflicts ()
  "Fontify all conflict regions in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (majutsu-conflict--find-conflict)
      (majutsu-conflict--fontify-region (match-beginning 0) (match-end 0)))))

(defun majutsu-conflict--extend-font-lock-region ()
  "Extend font-lock region to cover the whole conflict.
Return non-nil when the region was expanded."
  (let ((orig-beg font-lock-beg)
        (orig-end font-lock-end)
        (expanded nil))
    (save-excursion
      (goto-char orig-beg)
      (when (re-search-backward majutsu-conflict-begin-re nil t)
        (let* ((begin (match-beginning 0))
               (marker-len (length (match-string 1)))
               (end-re (format "^>\\{%d,\\}\\(?: .*\\)?$" marker-len)))
          (when (re-search-forward end-re nil t)
            (let ((end (match-end 0)))
              (when (or (and (<= begin orig-beg) (< orig-beg end))
                        (and (<= begin orig-end) (< orig-end end)))
                (when (or (< begin orig-beg) (> end orig-end))
                  (setq font-lock-beg begin
                        font-lock-end end
                        expanded t))))))))
    expanded))

;;;###autoload
(defun majutsu-conflict-refine ()
  "Add word-level refinement to conflict regions.
Call this command to highlight fine-grained differences within conflicts."
  (interactive)
  (majutsu-conflict--clear-refine-overlays (point-min) (point-max))
  (majutsu-conflict--refine-diffs)
  (majutsu-conflict--refine-snapshots))

(defun majutsu-conflict--after-change (beg end _len)
  "Refontify conflicts after edits in BEG..END.
Region overlays touched by the edit are dropped; refontification
re-creates them for regions that still parse as conflicts, so a
destroyed conflict loses its menu and help echo immediately."
  (when majutsu-conflict-mode
    ;; Sweep one character beyond the change so overlays that start or
    ;; end exactly at an empty deletion point are caught too.
    (dolist (overlay (overlays-in (max (point-min) (1- beg))
                                  (min (point-max) (1+ end))))
      (when (overlay-get overlay 'majutsu-conflict-region)
        (delete-overlay overlay)))
    (when font-lock-mode
      (font-lock-flush beg end)
      (font-lock-ensure beg end))))

(defvar-local majutsu-conflict--enabled-eldoc nil
  "Non-nil when `majutsu-conflict-mode' turned on `eldoc-mode' itself.")

(defun majutsu-conflict--enable ()
  "Enable conflict highlighting for JJ-style conflicts."
  ;; The two conflict minor modes paint and bind the same regions;
  ;; keep them mutually exclusive from this side too.
  (when (bound-and-true-p smerge-mode)
    (smerge-mode -1))
  ;; Add font-lock keywords for line-level highlighting
  (font-lock-add-keywords nil majutsu-conflict-font-lock-keywords 'append)
  (add-hook 'font-lock-extend-region-functions
            #'majutsu-conflict--extend-font-lock-region nil t)
  (add-hook 'after-change-functions #'majutsu-conflict--after-change nil t)
  (add-hook 'eldoc-documentation-functions #'majutsu-conflict--eldoc nil t)
  (add-hook 'context-menu-functions #'majutsu-conflict-context-menu 5 t)
  ;; global-eldoc-mode gives up on buffers whose major mode has no
  ;; eldoc support before this minor mode gets a chance to register;
  ;; honor its intent by enabling eldoc here.
  (when (and (bound-and-true-p global-eldoc-mode)
             (not (bound-and-true-p eldoc-mode)))
    (setq majutsu-conflict--enabled-eldoc t)
    (eldoc-mode 1))
  (when font-lock-mode
    (font-lock-flush)
    (majutsu-conflict--fontify-conflicts)))

(defun majutsu-conflict--disable ()
  "Disable conflict highlighting."
  (font-lock-remove-keywords nil majutsu-conflict-font-lock-keywords)
  (remove-hook 'font-lock-extend-region-functions
               #'majutsu-conflict--extend-font-lock-region t)
  (remove-hook 'after-change-functions #'majutsu-conflict--after-change t)
  (remove-hook 'eldoc-documentation-functions #'majutsu-conflict--eldoc t)
  (remove-hook 'context-menu-functions #'majutsu-conflict-context-menu t)
  (when majutsu-conflict--enabled-eldoc
    (setq majutsu-conflict--enabled-eldoc nil)
    (eldoc-mode -1))
  (majutsu-conflict--remove-region-overlays)
  (when font-lock-mode
    (font-lock-flush))
  (majutsu-conflict--clear-overlays))

;;;###autoload
(define-minor-mode majutsu-conflict-mode
  "Minor mode for jj conflict markers.
Provides highlighting and navigation for conflict regions."
  :lighter " Conflict"
  :keymap majutsu-conflict-mode-map
  (if majutsu-conflict-mode
      (majutsu-conflict--enable)
    (majutsu-conflict--disable)))

(defun majutsu-conflict--scan-styles ()
  "Return a list of conflict styles found in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (styles)
      (while (re-search-forward majutsu-conflict-begin-re nil t)
        (push (majutsu-conflict--detect-style
               (match-beginning 0)
               (length (match-string 1)))
              styles))
      styles)))

;;;###autoload
(defun majutsu-conflict-ensure-mode ()
  "Enable conflict mode based on marker style in the buffer.

Prefer `majutsu-conflict-mode' for JJ-style conflicts.  If only Git-style
markers are present, enable `smerge-mode'."
  (let* ((styles (majutsu-conflict--scan-styles))
         (jj-style (cl-some (lambda (style)
                              (memq style '(jj-diff jj-snapshot)))
                            styles))
         (git-style (cl-some (lambda (style)
                               (eq style 'git))
                             styles)))
    (cond
     (jj-style
      (when (bound-and-true-p smerge-mode)
        (smerge-mode -1))
      (majutsu-conflict-mode 1))
     (git-style
      (when (bound-and-true-p majutsu-conflict-mode)
        (majutsu-conflict-mode -1))
      (smerge-mode 1)))))

;;;###autoload
(defun majutsu-conflict-check-enable ()
  "Enable conflict mode if buffer has conflict markers."
  (majutsu-conflict-ensure-mode))

;;; _
(provide 'majutsu-conflict)
;;; majutsu-conflict.el ends here
