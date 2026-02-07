;;; majutsu-conflict.el --- Conflict marker parsing for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

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

;;; Constants

(defconst majutsu-conflict-min-marker-len 7
  "Minimum length of conflict markers.")

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
  "^\\([[:alnum:]]+\\) \\([[:xdigit:]]+\\) \"\\([^\"]+\\)\"\\(?: ([^)]*)\\)?$"
  "Regexp matching JJ conflict label metadata.")

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
  removes      ; list of (label . content) for bases
  adds         ; list of (label . content) for sides
  base)        ; (label . content) for the snapshot base side

(defun majutsu-conflict-parse-label (label)
  "Parse JJ LABEL string into a plist.
Return a plist with :change-id, :commit-id, and :description keys.
Return nil for nil or empty LABEL, or when LABEL does not match."
  (when (and (stringp label)
             (not (string= label ""))
             (string-match majutsu-conflict-label-re label))
    (list :change-id (match-string 1 label)
          :commit-id (match-string 2 label)
          :description (match-string 3 label))))

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
  (if (and label (string-match "\\`diff from:[ \t]*\\(.*\\)\\'" label))
      (match-string 1 label)
    label))

(defun majutsu-conflict--normalize-diff-add-label (label)
  "Strip JJ diff prefix from add LABEL."
  (if (and label (string-match "\\`to:[ \t]*\\(.*\\)\\'" label))
      (match-string 1 label)
    label))

(defun majutsu-conflict-revision-at-point ()
  "Return conflict metadata at point as a plist.
Returns a plist with :change-id, :commit-id, and :side (add/remove/base),
or nil when point is not inside a labeled section."
  (let ((conflict (majutsu-conflict-at-point)))
    (when conflict
      (let ((pos (point))
            (begin (majutsu-conflict-begin-pos conflict))
            (end (majutsu-conflict-end-pos conflict))
            (style (majutsu-conflict-style conflict)))
        (save-excursion
          (goto-char begin)
          (let ((done nil)
                (state nil)
                (add-count 0)
                (current-label nil)
                (diff-remove-label nil)
                (diff-add-label nil)
                (base-label nil)
                (side nil)
                (label nil))
            (while (and (not done) (< (point) end))
              (let* ((line-beg (line-beginning-position))
                     (line-end (line-end-position))
                     (line (buffer-substring-no-properties line-beg line-end))
                     (on-line (and (<= line-beg pos) (<= pos line-end))))
                (cond
                 ((string-match majutsu-conflict-end-re line)
                  (setq state nil)
                  (when on-line
                    (setq done t)))
                 ((string-match majutsu-conflict-diff-re line)
                  (setq state 'diff
                        diff-remove-label (match-string 2 line)
                        diff-add-label nil)
                  (when on-line
                    (setq side 'remove
                          label diff-remove-label
                          done t)))
                 ((string-match majutsu-conflict-begin-re line)
                  (when (eq style 'git)
                    (setq state 'remove
                          current-label (match-string 2 line))
                    (when on-line
                      (setq side 'remove
                            label current-label
                            done t))))
                 ((string-match majutsu-conflict-note-re line)
                  (when (eq state 'diff)
                    (setq diff-add-label (match-string 2 line)))
                  (when on-line
                    (setq side 'add
                          label diff-add-label
                          done t)))
                 ((string-match majutsu-conflict-remove-re line)
                  (setq state 'remove
                        current-label (match-string 2 line))
                  (when on-line
                    (setq side 'remove
                          label current-label
                          done t)))
                 ((string-match majutsu-conflict-add-re line)
                  (cl-incf add-count)
                  (setq state (if (or (eq style 'jj-diff)
                                      (and (eq style 'jj-snapshot) (= add-count 1)))
                                  'base
                                'add)
                        current-label (match-string 2 line))
                  (when on-line
                    (setq side state
                          label current-label
                          done t)))
                 ((string-match majutsu-conflict-git-ancestor-re line)
                  (setq state 'base
                        base-label (match-string 2 line)
                        current-label base-label)
                  (when on-line
                    (setq side 'base
                          label base-label
                          done t)))
                 ((string-match majutsu-conflict-git-separator-re line)
                  (setq state 'add)
                  (when on-line
                    (setq side 'add
                          label nil
                          done t)))
                 (t
                  (when on-line
                    (cond
                     ((eq state 'diff)
                      (cond
                       ((string-prefix-p "-" line)
                        (setq side 'remove
                              label diff-remove-label))
                       ((string-prefix-p "+" line)
                        (setq side 'add
                              label diff-add-label))))
                     ((eq state 'remove)
                      (setq side 'remove
                            label current-label))
                     ((eq state 'add)
                      (setq side 'add
                            label current-label))
                     ((eq state 'base)
                      (setq side 'base
                            label current-label)))
                    (setq done t))))
                (forward-line 1)))
            (let ((parsed (majutsu-conflict-parse-label label)))
              (when (and parsed side)
                (list :change-id (plist-get parsed :change-id)
                      :commit-id (plist-get parsed :commit-id)
                      :side side)))))))))

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
Returns (STYLE REMOVES ADDS BASE) or nil if invalid.
BASE is the snapshot term shown as a standalone +++++++ section."
  (save-excursion
    (goto-char begin)
    (forward-line 1)  ; skip <<<<<<< line
    (let ((state 'unknown)
          (style 'jj-snapshot)
          (seen-diff nil)
          (removes nil)
          (adds nil)
          (current-remove nil)
          (current-add nil)
          (snapshot-cell nil)
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
                       seen-diff t
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
                 (setq current-add (car adds))
                 (when (and (null snapshot-cell)
                            (or seen-diff
                                (and (null removes)
                                     (= (length adds) 1))))
                   (setq snapshot-cell current-add)))
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
        (let* ((removes (nreverse removes))
               (all-adds (nreverse adds))
               (base nil)
               (side-adds nil))
          (dolist (cell all-adds)
            (if (eq cell snapshot-cell)
                (setq base cell)
              (push cell side-adds)))
          (setq side-adds (nreverse side-adds))
          (if (and base
                   (= (length side-adds) (length removes)))
              (list style removes side-adds base)
            nil))))))

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
            (let ((removes (list (cons base-label base)))
                  (adds (list (cons left-label left)
                              (cons right-label right))))
              (list 'git removes adds (car removes)))
          nil)))))

;;; Main Parser

(defun majutsu-conflict-parse-buffer ()
  "Parse all conflicts in current buffer.
Returns list of `majutsu-conflict' structs."
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
                   (end (if (and (< match-end (point-max))
                                 (eq (char-after match-end) ?\n))
                            (1+ match-end)
                          match-end))
                   (style (majutsu-conflict--detect-style begin marker-len))
                   (parsed (if (eq style 'git)
                               (majutsu-conflict--parse-git-hunk begin end marker-len)
                             (majutsu-conflict--parse-jj-hunk begin end marker-len))))
              (when parsed
                (push (majutsu-conflict--create
                       :begin-pos begin
                       :end-pos end
                       :marker-len marker-len
                       :style (car parsed)
                       :removes (cadr parsed)
                       :adds (caddr parsed)
                       :base (cadddr parsed))
                      conflicts))))))
      (nreverse conflicts))))

(defun majutsu-conflict-at-point ()
  "Return the conflict at point, or nil."
  (let ((pos (point))
        (conflicts (majutsu-conflict-parse-buffer)))
    (cl-find-if (lambda (c)
                  (and (<= (majutsu-conflict-begin-pos c) pos)
                       (<= pos (majutsu-conflict-end-pos c))))
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
    (insert content)))

(defun majutsu-conflict-keep-side (n &optional before)
  "Keep side N (1-indexed) of the conflict at point.
With prefix arg (BEFORE non-nil), keep remove term N.
Without prefix, keep add term N (jj side #N)."
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
        (user-error "No side %d%s" n (if before " (before)" "")))
      (majutsu-conflict-resolve-with conflict content))))

(defun majutsu-conflict-keep-base ()
  "Keep the snapshot base of the conflict at point."
  (interactive)
  (let ((conflict (majutsu-conflict-at-point)))
    (unless conflict
      (user-error "No conflict at point"))
    (let ((base (or (majutsu-conflict-base conflict)
                    (car (majutsu-conflict-adds conflict)))))
      (unless base
        (user-error "No base in this conflict"))
      (majutsu-conflict-resolve-with conflict (cdr base)))))

;;; Minor Mode

(defvar-local majutsu-conflict--overlays nil
  "List of overlays for conflict highlighting (refine only).")

(defvar-local majutsu-conflict--in-diff nil
  "Non-nil when inside a diff section during font-lock.")

(defvar-local majutsu-conflict--in-add nil
  "Non-nil when inside a ++++++ section.")

(defvar-local majutsu-conflict--in-remove nil
  "Non-nil when inside a ------ section.")

(defvar-local majutsu-conflict--style nil
  "Current conflict style: `jj-diff' or `jj-snapshot'.")

(defvar-local majutsu-conflict--is-first-add nil
  "Non-nil when current ++++++ section is the first one (base).")

(defvar-local majutsu-conflict--add-count 0
  "Count of ++++++ sections seen in current conflict.")

(defvar-local majutsu-conflict--marker-len nil
  "Current conflict marker length used by font-lock state machine.")

(defvar majutsu-conflict-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ^ n") #'majutsu-conflict-next)
    (define-key map (kbd "C-c ^ p") #'majutsu-conflict-prev)
    (define-key map (kbd "C-c ^ b") #'majutsu-conflict-keep-base)
    (define-key map (kbd "C-c ^ R") #'majutsu-conflict-refine)
    ;; 1-9 for sides (after), M-1 to M-9 for before
    (dotimes (i 9)
      (let ((n (1+ i)))
        (define-key map (kbd (format "C-c ^ %d" n))
                    (lambda () (interactive)
                      (majutsu-conflict-keep-side n nil)))
        (define-key map (kbd (format "C-c ^ M-%d" n))
                    (lambda () (interactive)
                      (majutsu-conflict-keep-side n t)))))
    map)
  "Keymap for `majutsu-conflict-mode'.
\\<majutsu-conflict-mode-map>
\\[majutsu-conflict-next] - next conflict
\\[majutsu-conflict-prev] - previous conflict
\\[majutsu-conflict-keep-base] - keep base (rebase destination)
\\[majutsu-conflict-refine] - add word-level refinement
C-c ^ 1-9 - keep side N (after diff)
C-c ^ M-1 to M-9 - keep side N (before diff)")

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

;;; Font-Lock Support

(defun majutsu-conflict--find-conflict (&optional limit)
  "Find and match a JJ-style conflict region.  Intended as a font-lock MATCHER.
Skips git-style conflicts (left to `smerge-mode').
Returns non-nil if a match is found between point and LIMIT.
Sets match-data with group 0 = entire conflict."
  (let ((found nil))
    (while (and (not found) (re-search-forward majutsu-conflict-begin-re limit t))
      (let* ((begin (match-beginning 0))
             (marker-len (length (match-string 1)))
             (end-re (format "^>\\{%d,\\}\\(?: .*\\)?$" marker-len))
             (style (majutsu-conflict--detect-style begin marker-len)))
        (when (and (memq style '(jj-diff jj-snapshot))
                   (re-search-forward end-re limit t))
          (set-match-data (list begin (match-end 0)))
          (with-silent-modifications
            (put-text-property begin (match-end 0) 'font-lock-multiline t))
          (setq found t))))
    found))

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
                      (majutsu-conflict--marker-kind (plist-get parsed :char)))))
      (cond
       ;; Marker line
       ((memq kind '(begin end diff add remove))
        (pcase kind
          ('begin
           ;; Detect style from first body line using this conflict's marker length.
           (setq majutsu-conflict--marker-len (plist-get parsed :len)
                 majutsu-conflict--in-diff nil
                 majutsu-conflict--in-add nil
                 majutsu-conflict--in-remove nil
                 majutsu-conflict--add-count 0)
           (setq majutsu-conflict--style
                 (majutsu-conflict--detect-style line-beg majutsu-conflict--marker-len)))
          ('end
           (setq majutsu-conflict--in-diff nil
                 majutsu-conflict--in-add nil
                 majutsu-conflict--in-remove nil
                 majutsu-conflict--marker-len nil))
          ('diff
           (setq majutsu-conflict--in-diff t
                 majutsu-conflict--in-add nil
                 majutsu-conflict--in-remove nil))
          ('add
           (setq majutsu-conflict--in-diff nil
                 majutsu-conflict--in-add t
                 majutsu-conflict--in-remove nil)
           (cl-incf majutsu-conflict--add-count))
          ('remove
           (setq majutsu-conflict--in-diff nil
                 majutsu-conflict--in-add nil
                 majutsu-conflict--in-remove t)))
        (set-match-data (list line-beg (min (1+ line-end) (point-max))))
        (goto-char (min (1+ line-end) (point-max)))
        t)
       ;; Note marker
       ((eq kind 'note)
        (set-match-data (list line-beg (min (1+ line-end) (point-max))))
        (goto-char (min (1+ line-end) (point-max)))
        t)
       ;; Content line
       (t
        (set-match-data (list line-beg (min (1+ line-end) (point-max))))
        (goto-char (min (1+ line-end) (point-max)))
        t)))))

(defun majutsu-conflict--line-face ()
  "Return face for current line based on state and content."
  (save-excursion
    (goto-char (match-beginning 0))
    (let ((marker (majutsu-conflict--line-marker-at-point
                   (or majutsu-conflict--marker-len
                       majutsu-conflict-min-marker-len))))
      (cond
       ;; Marker lines
       ((memq (car-safe marker) '(begin end add remove diff note))
        'majutsu-conflict-marker-face)
       ;; JJ diff style content
       (majutsu-conflict--in-diff
        (cond
         ((looking-at "^\\+") 'majutsu-conflict-added-face)
         ((looking-at "^-") 'majutsu-conflict-removed-face)
         ((looking-at "^ ") 'majutsu-conflict-context-face)
         (t nil)))
       ;; In ++++++ section
       (majutsu-conflict--in-add
        (cond
         ;; In jj-diff, ++++++ is always the base
         ((eq majutsu-conflict--style 'jj-diff)
          'majutsu-conflict-base-face)
         ;; In jj-snapshot, first ++++++ (count=1) is base
         ((= majutsu-conflict--add-count 1)
          'majutsu-conflict-base-face)
         (t
          ;; Subsequent ++++++ sections are "to" sides
          'majutsu-conflict-added-face)))
       ;; In ------ section ("from" side in snapshot style)
       (majutsu-conflict--in-remove
        'majutsu-conflict-removed-face)
       (t nil)))))

(defconst majutsu-conflict-font-lock-keywords
  `((majutsu-conflict--find-conflict
     (majutsu-conflict--match-line
      ;; PRE-FORM: reset state and return conflict end as search limit
      (progn
        (setq majutsu-conflict--in-diff nil
              majutsu-conflict--in-add nil
              majutsu-conflict--in-remove nil
              majutsu-conflict--add-count 0
              majutsu-conflict--marker-len nil)
        (goto-char (match-beginning 0))
        (match-end 0))  ; Return conflict end position as limit
      nil
      (0 (majutsu-conflict--line-face) prepend t))))
  "Font lock keywords for `majutsu-conflict-mode'.")

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
  "Refontify conflicts after edits in BEG..END."
  (when (and majutsu-conflict-mode font-lock-mode)
    (font-lock-flush beg end)
    (font-lock-ensure beg end)))

(defun majutsu-conflict--enable ()
  "Enable conflict highlighting for JJ-style conflicts."
  ;; Add font-lock keywords for line-level highlighting
  (font-lock-add-keywords nil majutsu-conflict-font-lock-keywords 'append)
  (add-hook 'font-lock-extend-region-functions
            #'majutsu-conflict--extend-font-lock-region nil t)
  (add-hook 'after-change-functions #'majutsu-conflict--after-change nil t)
  (when font-lock-mode
    (font-lock-flush)
    (majutsu-conflict--fontify-conflicts)))

(defun majutsu-conflict--disable ()
  "Disable conflict highlighting."
  (font-lock-remove-keywords nil majutsu-conflict-font-lock-keywords)
  (remove-hook 'font-lock-extend-region-functions
               #'majutsu-conflict--extend-font-lock-region t)
  (remove-hook 'after-change-functions #'majutsu-conflict--after-change t)
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
     (jj-style (majutsu-conflict-mode 1))
     (git-style (smerge-mode 1)))))

;;;###autoload
(defun majutsu-conflict-check-enable ()
  "Enable conflict mode if buffer has conflict markers."
  (majutsu-conflict-ensure-mode))

;;; _
(provide 'majutsu-conflict)
;;; majutsu-conflict.el ends here
