;;; majutsu-diff.el --- Diff viewing and editing for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025-2026 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Portions of diff washing/highlighting behavior are adapted from:
;; - Magit `lisp/magit-diff.el` (commit c800f79c2061621fde847f6a53129eca0e8da728)
;;   Copyright (C) 2008-2026 The Magit Project Contributors
;; - GNU Emacs `lisp/vc/smerge-mode.el` (commit b37711a25f78a915a10245a6330c3b2b4434b2e5)
;;   Copyright (C) 1999-2026 Free Software Foundation, Inc.

;;; Commentary:

;; This library implements jj diff buffers and related transients,
;; including context management, refinement, and diffedit helpers.

;;; Code:

(require 'majutsu-core)
(require 'majutsu-mode)
(require 'majutsu-process)
(require 'majutsu-config)
(require 'majutsu-selection)
(require 'majutsu-template)
(require 'json)
(require 'magit-diff)      ; for faces/font-lock keywords
(require 'diff-mode)
(require 'smerge-mode)

(declare-function majutsu-read-revset "majutsu-jj" (prompt &optional default completion-args))
(declare-function majutsu-find-file "majutsu-file" (revset path))
(declare-function majutsu-find-file-noselect "majutsu-file" (rev file &optional revert))
(declare-function majutsu-read-files "majutsu-file" (prompt initial-input history &optional list-fn))
(declare-function majutsu-color-words-line-info-at-point "majutsu-color-words" ())
(declare-function majutsu-color-words-side-at-point "majutsu-color-words" (&optional pos))
(declare-function majutsu-color-words-column-at-point "majutsu-color-words" (goto-from &optional pos info))
(declare-function majutsu-color-words-wash-diffs "majutsu-color-words" (args))
(declare-function majutsu-color-words--collect-change-spans "majutsu-color-words" (beg end))
(declare-function majutsu-color-words--collect-debug-change-spans "majutsu-color-words" (beg end))
(declare-function majutsu-color-words--collect-debug-token-spans "majutsu-color-words" (beg end))
(declare-function majutsu-color-words--group-change-pairs "majutsu-color-words" (spans))

;;; Options
;;;; Diff Mode

(defcustom majutsu-diff-sections-hook
  (list #'majutsu-insert-diff-revision-headers
        #'majutsu-insert-diff-revision-message
        #'majutsu-insert-diff
        ;; #'majutsu-insert-xref-buttons
        )
  "Hook run to insert sections into a `majutsu-diff-mode' buffer."
  :group 'majutsu-diff
  :type 'hook)

(defcustom majutsu-diff-refine-hunk nil
  "Whether to show word-granularity differences inside hunks.

`nil'  Never show refinement.
`all'  Refine all hunks immediately.
`t'    Refine each hunk once it becomes the current section and keep the
       refinement when another section is selected.  This variant exists
       for performance reasons."
  :group 'majutsu
  :type '(choice (const :tag "No refinement" nil)
          (const :tag "Immediately refine all hunks" all)
          (const :tag "Refine currently selected hunk" t)))

(put 'majutsu-diff-refine-hunk 'permanent-local t)

(defcustom majutsu-diff-fontify-hunk nil
  "Whether to apply syntax highlighting to diff hunks.

`nil'  Never fontify hunks.
`all'  Fontify all hunks immediately.
`t'    Fontify each hunk once it becomes the current section and keep the
       fontification when another section is selected.  This variant exists
       for performance reasons.

This is experimental and can be slow because fontification is synchronous."
  :group 'majutsu
  :type '(choice (const :tag "No syntax highlighting" nil)
          (const :tag "Immediately highlight all hunks" all)
          (const :tag "Highlight currently selected hunk" t)))

(put 'majutsu-diff-fontify-hunk 'permanent-local t)

(defcustom majutsu-diff-refine-ignore-whitespace smerge-refine-ignore-whitespace
  "Whether to ignore whitespace while refining hunks."
  :group 'majutsu
  :type 'boolean)

(defcustom majutsu-diff-paint-whitespace t
  "Whether to highlight whitespace issues inside diff hunks."
  :group 'majutsu
  :type 'boolean)

(defcustom majutsu-diff-highlight-trailing t
  "Whether to mark trailing whitespace in diff hunks."
  :group 'majutsu
  :type 'boolean)

(defcustom majutsu-diff-adjust-tab-width nil
  "Whether to adjust displayed tab width based on the file's setting.
When non-nil, try to read `tab-width' from a live buffer visiting the file;
otherwise fall back to the current buffer's `tab-width'."
  :group 'majutsu
  :type '(choice (const :tag "Never adjust" nil)
          (const :tag "Use live file buffer value" t)))

;;; Faces

(defface majutsu-diffstat-binary
  '((t :inherit font-lock-constant-face :foreground "#81c8be"))
  "Face for the (binary) label in diffstat entries."
  :group 'majutsu)

(defface majutsu-diff-color-words-focus
  '((((class color) (background light))
     :extend t
     :background "grey95")
    (((class color) (background dark))
     :extend t
     :background "grey20"))
  "Background-only face for the focused hunk in color-words diffs.
This face intentionally omits `:foreground' so that per-word ANSI
colors (red/green) show through unaffected."
  :group 'majutsu)

;;; Line Offset Calculation

(defun majutsu-diff--offset-in-buffer (line)
  "Return LINE offset after applying diff hunks in current buffer.
Assumes current buffer contains a unified diff."
  (let ((offset 0))
    (goto-char (point-min))
    (catch 'found
      (while (re-search-forward
              "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@.*\\n"
              nil t)
        (let* ((from-beg (string-to-number (match-string 1)))
               (from-len (if (match-string 2)
                             (string-to-number (match-string 2))
                           1))
               (to-len (if (match-string 4)
                           (string-to-number (match-string 4))
                         1)))
          (if (<= from-beg line)
              (if (<= (+ from-beg from-len) line)
                  (setq offset (+ offset (- to-len from-len)))
                (let ((rest (- line from-beg)))
                  (while (> rest 0)
                    (pcase (char-after)
                      (?\s (setq rest (1- rest)))
                      (?- (setq offset (1- offset))
                          (setq rest (1- rest)))
                      (?+ (setq offset (1+ offset))))
                    (forward-line 1))))
            (throw 'found nil)))))
    (+ line offset)))

(defun majutsu-diff-visit--offset (root file from-rev to-rev line)
  "Compute line offset for FILE between FROM-REV and TO-REV.
ROOT is the repository root.  Returns the adjusted line number."
  (let ((default-directory root))
    (with-temp-buffer
      (majutsu-jj-insert "diff" "--from" from-rev "--to" to-rev "--"
                         (majutsu-jj-fileset-quote file))
      (if (= (point-min) (point-max))
          line
        (majutsu-diff--offset-in-buffer line)))))

;;;

(defvar majutsu-diff--tab-width-cache nil
  "Alist mapping file names to cached tab widths.")

(defconst majutsu-diff--formatting-args
  '("--stat"
    "--summary"
    "-s"
    "--types"
    "--name-only"
    "--git"
    "--ignore-all-space"
    "-w"
    "--ignore-space-change"
    "-b"
    "--color-words")
  "Arguments that are considered jj diff \"Diff Formatting Options\".

These are the only arguments that are remembered per diff buffer.")

(defvar-local majutsu-diff-backend 'git
  "Backend used to render the current diff buffer.")

(defvar-local majutsu-diff--revision-metadata-cache nil
  "Cached revision metadata for the current diff arguments.
The value is (KEY . METADATA).  A nil METADATA value is cached too, so
zero-match, multi-match, and failed queries are not repeated by section
inserters during the same refresh.")

(defun majutsu-diff--backend-from-args (args)
  "Return diff backend inferred from ARGS."
  (if (member "--color-words" args)
      'color-words
    'git))

(defun majutsu-diff--sync-backend (&optional args)
  "Sync `majutsu-diff-backend' from ARGS or current buffer args.
Return the resulting backend symbol."
  (setq-local majutsu-diff-backend
              (majutsu-diff--backend-from-args
               (or args majutsu-buffer-diff-args)))
  (when (eq majutsu-diff-backend 'color-words)
    (require 'majutsu-color-words))
  majutsu-diff-backend)

(defun majutsu-diff--set-left-margin (width)
  "Set left margin WIDTH for the current buffer's windows."
  (setq-local left-margin-width width)
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (set-window-margins window width (cdr (window-margins window)))))

(defun majutsu-diff--backend-washer (backend)
  "Return the wash function for BACKEND."
  (if (eq backend 'color-words)
      #'majutsu-color-words-wash-diffs
    #'majutsu-diff-wash-diffs))

(defun majutsu-diff--color-words-line-info ()
  "Return color-words line info at point, or nil if unavailable."
  (when (eq (majutsu-diff--sync-backend) 'color-words)
    (majutsu-color-words-line-info-at-point)))

(defun majutsu-diff--color-words-column (info goto-from)
  "Return source column for INFO on GOTO-FROM side.
Color-words may split one source line into multiple rendered lines, so this
uses side-affine accumulation from related lines instead of raw
`current-column'."
  (when (plist-get info :content-column)
    (majutsu-color-words-column-at-point goto-from nil info)))

(defcustom majutsu-diff-whitespace-max-chars 12000
  "Skip whitespace painting for hunks larger than this many chars.
Set to nil to always paint whitespace inside hunks."
  :group 'majutsu
  :type '(choice (const :tag "No limit" nil)
          (integer :tag "Max characters")))

(put 'majutsu-diff-mode 'majutsu-diff-default-arguments
     '("--git" "--stat"))

(defun majutsu-diff--delete-line ()
  "Delete current line, including trailing newline if present."
  (delete-region (line-beginning-position)
                 (min (point-max) (1+ (line-end-position)))))

(defun majutsu-diff--remembered-args (args)
  "Return the subset of ARGS that should be remembered by diff buffers.

This intentionally keeps only jj diff \"Diff Formatting Options\"."
  (let (out)
    (while args
      (let ((arg (pop args)))
        (when (stringp arg)
          (cond
           ((member arg majutsu-diff--formatting-args)
            (push arg out))
           ((string-prefix-p "--context=" arg)
            (push arg out))
           ((member arg '("--context" "--template"))
            (push arg out)
            (when (and args (stringp (car args)))
              (push (pop args) out)))
           ((seq-some (lambda (prefix)
                        (string-prefix-p prefix arg))
                      '("--context=" "--template="))
            (push arg out))))))
    (nreverse out)))

(defun majutsu-diff--extract-range-args (args)
  "Return the subset of ARGS that restrict `jj diff' range."
  (seq-filter (lambda (arg)
                (or (transient-arg-value "--revisions=" (list arg))
                    (transient-arg-value "--from=" (list arg))
                    (transient-arg-value "--to=" (list arg))))
              args))

;;; Arguments
;;;; Prefix Classes

(defclass majutsu-diff-prefix (majutsu-jj-transient-prefix)
  ((history-key :initform 'majutsu-diff)
   (major-mode :initform 'majutsu-diff-mode)))

;;;; Infix Classes

(defclass majutsu-diff-range-option (majutsu-selection-option) ())

(cl-defmethod transient-init-value ((obj majutsu-diff-prefix))
  (pcase-let ((`(,args ,range ,filesets)
               (majutsu-diff--get-value (oref obj major-mode) 'prefix)))
    (oset obj value
          (majutsu-filesets-build-transient-value
           (append range args)
           filesets))))

(cl-defmethod transient-prefix-value ((obj majutsu-diff-prefix))
  "Return (ARGS RANGE FILESETS) for the Majutsu diff transient.

ARGS are remembered diff formatting arguments.  RANGE is a list of jj
diff range arguments derived from `-r' or `--from/--to'.  FILESETS is a
list of filesets (path filters)."
  (pcase-let* ((`(,raw ,filesets)
                (majutsu-filesets-split-transient-value
                 (cl-call-next-method obj)))
               (args (majutsu-diff--remembered-args raw))
               (range (majutsu-diff--extract-range-args raw)))
    (list args range filesets)))

(cl-defmethod transient-set-value ((obj majutsu-diff-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode)))
    (pcase-let ((`(,args ,range ,filesets) (transient-args (oref obj command))))
      (majutsu-diff--set-value mode args range filesets)
      (transient--history-push obj))))

(cl-defmethod transient-save-value ((obj majutsu-diff-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode)))
    (pcase-let ((`(,args ,range ,filesets) (transient-args (oref obj command))))
      (majutsu-diff--set-value mode args range filesets t)
      (transient--history-push obj))))

(cl-defmethod majutsu-transient--save-repository-defaults ((obj majutsu-diff-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode)))
    (pcase-let ((`(,args ,range ,filesets) (transient-args (oref obj command))))
      (majutsu-transient-save-repository-value 'majutsu-diff mode args)
      (when (eq major-mode mode)
        (majutsu-diff--set-value mode args range filesets))
      (transient--history-push obj)
      (message "Saved diff arguments as repository defaults"))))

;;;; Argument Access

(defun majutsu-diff--get-value (mode &optional use-buffer-args)
  "Get diff arguments for MODE.

  Returns (args range filesets) triple.  USE-BUFFER-ARGS follows
`majutsu-prefix-use-buffer-arguments' or
`majutsu-direct-use-buffer-arguments'."
  (setq use-buffer-args
        (pcase-exhaustive use-buffer-args
          ('prefix majutsu-prefix-use-buffer-arguments)
          ('direct majutsu-direct-use-buffer-arguments)
          ('nil majutsu-direct-use-buffer-arguments)
          ((or 'always 'selected 'current 'never)
           use-buffer-args)))
  (let* ((use-current (and (memq use-buffer-args '(always selected current))
                           (eq major-mode mode)))
         (args (cond
                (use-current
                 majutsu-buffer-diff-args)
                ((and (memq use-buffer-args '(always selected))
                      (when-let* ((buf (majutsu--get-mode-buffer
                                        mode (eq use-buffer-args 'selected))))
                        (buffer-local-value 'majutsu-buffer-diff-args buf))))
                (t
                 (majutsu-transient-default-value
                  'majutsu-diff mode
                  'majutsu-diff-current-arguments
                  'majutsu-diff-default-arguments))))
         (range (and use-current majutsu-buffer-diff-range))
         (filesets (and use-current majutsu-buffer-diff-filesets)))
    (list args range filesets)))

(defun majutsu-diff--set-value (mode args range filesets &optional save)
  "Set current diff values for MODE.

When SAVE is non-nil, also persist ARGS using `transient-values'."
  (setq args (or (majutsu-diff--remembered-args args)
                 (get mode 'majutsu-diff-default-arguments)))
  (if-let* ((config-id (majutsu-repository-config-id)))
      (majutsu-transient-put-repository-current-value
       'majutsu-diff mode args config-id)
    (put mode 'majutsu-diff-current-arguments args))
  (when save
    (setf (alist-get (majutsu-transient-global-default-key 'majutsu-diff mode)
                     transient-values)
          args)
    (transient-save-values))
  (when (eq major-mode mode)
    (setq-local majutsu-buffer-diff-args args)
    (setq-local majutsu-buffer-diff-range range)
    (setq-local majutsu-buffer-diff-filesets filesets)
    (majutsu-diff--sync-backend majutsu-buffer-diff-args))
  nil)

(defun majutsu-diff--set-buffer-args (args)
  "Set current buffer's remembered diff formatting ARGS."
  (majutsu-diff--set-value 'majutsu-diff-mode
                           args
                           majutsu-buffer-diff-range
                           majutsu-buffer-diff-filesets))

(defun majutsu-diff-arguments (&optional mode)
  "Return the current diff arguments.

The returned value is a (ARGS RANGE FILESETS) triple."
  (if (eq transient-current-command 'majutsu-diff)
      (transient-args 'majutsu-diff)
    (majutsu-diff--get-value (or mode 'majutsu-diff-mode) 'direct)))

;;; Diff Parsing & Display

(defconst majutsu-diff-statline-re
  (concat "^ ?"
          "\\(.*\\)"     ; file
          "\\( +| +\\)"  ; separator
          "\\("
          "\\(?:[0-9]+\\)"
          "\\|"
          "(binary)\\(?: +[+-][0-9]+ bytes\\)?"
          "\\)"
          "\\(?: +\\(\\+*\\)\\(-*\\)\\)?$") ; add/del graph (optional)
  "Regexp matching `jj diff --stat` entries, modeled after Magit's statline.")

(defconst majutsu-diff--file-metadata-template
  (majutsu-template-compile
   ["[" [:json [:status]]
    "," [:json [:source :path]]
    "," [:json [:target :path]] "]\n"]
   'TreeDiffEntry)
  "Template for unambiguous machine-readable diff entry metadata.")

(defvar majutsu-diff--active-file-metadata nil
  "Structured file metadata matching the Git diff currently being washed.")

(defvar majutsu-diff--file-metadata-queue nil
  "Remaining structured entries while Git file sections are being washed.")

(defvar-local majutsu-diff--file-metadata-by-section nil
  "Hash table mapping Git-format patch sections to structured metadata.")

(defun majutsu-diff--parse-file-metadata-line (line)
  "Parse one JSON diff metadata LINE into a plist, or return nil."
  (condition-case nil
      (pcase (json-parse-string line
                                :array-type 'list
                                :null-object nil
                                :false-object nil)
        (`(,status ,source ,target)
         (when (and (stringp status)
                    (stringp source)
                    (stringp target))
           (list :status status
                 :source source
                 :target target))))
    (error nil)))

(defun majutsu-diff--parse-file-metadata-output (output)
  "Parse newline-delimited JSON diff metadata OUTPUT.
Ignore malformed records."
  (delq nil
        (mapcar #'majutsu-diff--parse-file-metadata-line
                (split-string (or output "") "\n" t))))

(defun majutsu-diff--query-file-metadata (range filesets)
  "Return structured metadata for diff RANGE and FILESETS, or nil on failure.
The caller must already have snapshotted the main diff; this query uses
`--ignore-working-copy' to read that same state."
  (let ((args (majutsu-jj-append-filesets
               (append (list "--ignore-working-copy"
                             "diff" "-T" majutsu-diff--file-metadata-template)
                       range)
               filesets)))
    (condition-case nil
        (majutsu--with-no-color
          (with-temp-buffer
            (when (zerop (apply #'majutsu-jj-insert args))
              (majutsu-diff--parse-file-metadata-output (buffer-string)))))
      (error nil))))

(defun majutsu-diff--raw-git-file-headers ()
  "Return raw Git file header blocks in the current narrowed buffer."
  (save-excursion
    (goto-char (point-min))
    (let (headers)
      (while (re-search-forward "^diff --git " nil t)
        (let ((beg (match-beginning 0)))
          (forward-line 1)
          (let ((end (or (save-excursion
                           (when (re-search-forward
                                  "^\\(?:diff --git \\|@@ \\)" nil t)
                             (match-beginning 0)))
                         (point-max))))
            (push (buffer-substring-no-properties beg end) headers)
            (goto-char end))))
      (nreverse headers))))

(defun majutsu-diff--set-section-path (section path)
  "Set real file SECTION and its hunk identities to exact PATH."
  (oset section value path)
  (dolist (child (oref section children))
    (when (magit-section-match 'jj-hunk child)
      (let ((value (oref child value)))
        (when (consp value)
          (oset child value (cons path (cdr value))))))))

(defun majutsu-diff--read-git-quoted-token (token)
  "Decode one C-quoted Git path at the start of TOKEN.
Return (VALUE . END), where END is the first position after the closing quote,
or nil for malformed input."
  (when (string-prefix-p "\"" token)
    (catch 'done
      (let ((index 1)
            bytes)
        (cl-labels
            ((push-char
              (char)
              (dolist (byte (string-to-list
                             (encode-coding-string (string char) 'utf-8 t)))
                (push byte bytes))))
          (while (< index (length token))
            (let ((char (aref token index)))
              (setq index (1+ index))
              (cond
               ((= char ?\")
                (let ((raw (apply #'unibyte-string (nreverse bytes))))
                  (throw 'done
                         (cons (decode-coding-string raw 'utf-8 t) index))))
               ((/= char ?\\)
                (push-char char))
               ((>= index (length token))
                (throw 'done nil))
               (t
                (let ((escaped (aref token index)))
                  (cond
                   ((and (>= escaped ?0) (<= escaped ?7))
                    (let ((start index)
                          (count 0))
                      (while (and (< index (length token))
                                  (< count 3)
                                  (>= (aref token index) ?0)
                                  (<= (aref token index) ?7))
                        (setq index (1+ index)
                              count (1+ count)))
                      (let ((byte (string-to-number
                                   (substring token start index) 8)))
                        (when (> byte 255)
                          (throw 'done nil))
                        (push byte bytes))))
                   (t
                    (setq index (1+ index))
                    (let ((byte (cdr (assq escaped
                                           '((?a . 7) (?b . 8) (?t . 9)
                                             (?n . 10) (?v . 11) (?f . 12)
                                             (?r . 13) (?\" . 34) (?\\ . 92))))))
                      (unless byte
                        (throw 'done nil))
                      (push byte bytes))))))))))
        nil))))

(defun majutsu-diff--git-quoted-token-value (token)
  "Decode a complete quoted Git path TOKEN, or return nil."
  (pcase (majutsu-diff--read-git-quoted-token token)
    (`(,value . ,end)
     (and (= end (length token)) value))))

(defun majutsu-diff--git-token-matches-p (token expected)
  "Return non-nil when Git path TOKEN represents EXPECTED."
  (or (equal token expected)
      (equal (majutsu-diff--git-quoted-token-value token) expected)))

(defun majutsu-diff--git-header-paths-match-p (header entry)
  "Return non-nil when Git diff HEADER paths match structured ENTRY.
ENTRY remains the source of truth; this only rejects a stale or reordered
metadata result."
  (let* ((header (or header ""))
         (left (concat "a/" (plist-get entry :source)))
         (right (concat "b/" (plist-get entry :target)))
         (raw (concat left " " right)))
    (or (string-prefix-p (concat "diff --git " raw "\n") header)
        (when-let* ((line (car (split-string header "\n")))
                    ((string-prefix-p "diff --git " line)))
          (let ((payload (string-remove-prefix "diff --git " line)))
            (or (equal payload raw)
          ;; If the left token is raw, its exact structured value tells us
          ;; where the otherwise ambiguous separator must be.
                (and (string-prefix-p (concat left " ") payload)
                     (majutsu-diff--git-token-matches-p
                      (substring payload (1+ (length left))) right))
          ;; A quoted left token is self-delimiting; decode it and validate the
          ;; complete remaining token independently.
                (and (string-prefix-p "\"" payload)
                     (pcase (majutsu-diff--read-git-quoted-token payload)
                       (`(,value . ,end)
                        (and (equal value left)
                             (< end (length payload))
                             (= (aref payload end) ?\s)
                             (majutsu-diff--git-token-matches-p
                              (substring payload (1+ end)) right)))))))))))

(defun majutsu-diff--git-header-status-match-p (header status)
  "Return non-nil when extended Git HEADER agrees with structured STATUS."
  (let ((added (or (string-match-p "^new file mode " header)
                   (string-match-p "^--- /dev/null$" header)))
        (removed (or (string-match-p "^deleted file mode " header)
                     (string-match-p "^+++ /dev/null$" header)))
        (renamed (and (string-match-p "^rename from " header)
                      (string-match-p "^rename to " header)))
        (copied (and (string-match-p "^copy from " header)
                     (string-match-p "^copy to " header))))
    (pcase status
      ("added" added)
      ("removed" removed)
      ("renamed" renamed)
      ("copied" copied)
      ("modified" (not (or added removed renamed copied)))
      (_ nil))))

(defun majutsu-diff--file-metadata-matches-section-p (entry section)
  "Return non-nil when structured ENTRY is consistent with file SECTION."
  (let ((header (oref section header)))
    (and (majutsu-diff--git-header-paths-match-p header entry)
         (majutsu-diff--git-header-status-match-p
         header (plist-get entry :status)))))

(defun majutsu-diff--raw-file-metadata-consistent-p (metadata)
  "Return non-nil when ordered METADATA agrees with the raw Git file headers.
The check covers file count, order, paths, and operation status."
  (let ((headers (majutsu-diff--raw-git-file-headers)))
    (and metadata
         (= (length headers) (length metadata))
         (cl-every
          #'identity
          (cl-mapcar
           (lambda (entry header)
             (and (majutsu-diff--git-header-paths-match-p header entry)
                  (majutsu-diff--git-header-status-match-p
                   header (plist-get entry :status))))
           metadata headers)))))

(defun majutsu-diff--attach-file-metadata (metadata)
  "Attach ordered structured METADATA to washed file sections.
Fail closed unless section count, order, paths, and operation statuses all
agree with the structured metadata."
  (setq majutsu-diff--file-metadata-by-section (make-hash-table :test #'eq))
  (let ((root (or (bound-and-true-p magit-insert-section--current)
                  magit-root-section))
        real-files stat-files)
    (when root
      (magit-map-sections
       (lambda (section)
         (when (magit-section-match 'jj-file section)
           (if (oref section header)
               (push section real-files)
             (push section stat-files))))
       root))
    (setq real-files (nreverse real-files)
          stat-files (nreverse stat-files))
    (when (and metadata
               (= (length real-files) (length metadata))
               (cl-every #'identity
                         (cl-mapcar #'majutsu-diff--file-metadata-matches-section-p
                                    metadata real-files)))
      (cl-mapc
       (lambda (section entry)
         (majutsu-diff--set-section-path section (plist-get entry :target))
         (puthash section entry majutsu-diff--file-metadata-by-section))
       real-files metadata)
      (when (= (length stat-files) (length metadata))
        (cl-mapc (lambda (section entry)
                   (oset section value (plist-get entry :target)))
                 stat-files metadata)))))

(defun majutsu-diff-file-metadata (section)
  "Return structured metadata attached to file SECTION, or nil."
  (and majutsu-diff--file-metadata-by-section
       (gethash section majutsu-diff--file-metadata-by-section)))

(defun majutsu-diff--collect-diff-files ()
  "Return file paths in the order they appear in the current diff.
Prefer active structured metadata; otherwise fall back to simple
unquoted `diff --git' headers."
  (or (mapcar (lambda (entry) (plist-get entry :target))
              majutsu-diff--active-file-metadata)
      (save-excursion
        (goto-char (point-min))
        (let (files)
          (while (re-search-forward "^diff --git a/\\(.*\\) b/\\(.*\\)$" nil t)
            (push (match-string-no-properties 2) files))
          (nreverse files)))))

(defun majutsu-jump-to-diffstat-or-diff (&optional expand)
  "Jump to the diffstat or diff.
When point is on a file inside the diffstat section, then jump
to the respective diff section, otherwise jump to the diffstat
section or a child thereof."
  (interactive "P")
  (cond-let
    ([section
      (magit-get-section
       (append
        (magit-section-case
          ([jj-file diffstat] `((jj-file . ,(oref it value)) (diff-root)))
          (jj-file `((jj-file . ,(oref it value)) (diffstat) (diff-root)))
          (jj-hunk `((jj-file . ,(magit-section-parent-value it))
                     (diffstat) (diff-root)))
          (t '((diffstat) (diff-root))))
        (magit-section-ident magit-root-section)))]
     (goto-char (oref section start))
     (when expand
       (with-local-quit (magit-section-show section))
       (recenter 0)))
    ((user-error "No diffstat in this buffer"))))

(defun majutsu-diff--tab-width (file)
  "Return the tab width to use for FILE, with simple caching."
  (cond
   ((not majutsu-diff-adjust-tab-width) tab-width)
   ((and (assoc file majutsu-diff--tab-width-cache)
         (cdr (assoc file majutsu-diff--tab-width-cache))))
   ((and (find-buffer-visiting file)
         (setf (alist-get file majutsu-diff--tab-width-cache nil nil #'equal)
               (buffer-local-value 'tab-width (get-file-buffer file)))))
   (t
    (setf (alist-get file majutsu-diff--tab-width-cache nil nil #'equal) tab-width)
    tab-width)))

(defun majutsu-diff--paint-hunk-whitespace (start end file)
  "Paint tabs and trailing whitespace between START and END for FILE."
  (when (and majutsu-diff-paint-whitespace
             (or (not majutsu-diff-whitespace-max-chars)
                 (<= (- end start) majutsu-diff-whitespace-max-chars)))
    (let ((tabw (majutsu-diff--tab-width file)))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (let ((bol (line-beginning-position))
                (eol (line-end-position)))
            ;; Skip the diff marker column when checking trailing whitespace so
            ;; blank diff lines like "+" or " " don't light up.
            (when (and majutsu-diff-highlight-trailing
                       (< bol eol))
              (save-excursion
                (goto-char (min eol (1+ bol)))
                (when (re-search-forward "[ \t]+$" eol t)
                  (let ((ov (make-overlay (match-beginning 0) (match-end 0) nil t)))
                    (overlay-put ov 'font-lock-face 'magit-diff-whitespace-warning)
                    (overlay-put ov 'evaporate t)
                    (overlay-put ov 'priority 2)))))
            ;; Paint tabs by giving them display width.
            (goto-char bol)
            (while (search-forward "\t" eol t)
              (put-text-property (1- (point)) (point)
                                 'display (list (list 'space :width tabw)))))
          ;; Paint tabs by giving them display width.
          (forward-line 1))))))

(defun majutsu--insert-diff-hunks (diff-output &optional args)
  "Insert DIFF-OUTPUT and wash it into navigable hunk sections.
ARGS are the diff arguments used to produce DIFF-OUTPUT."
  (let ((start (point)))
    (when diff-output
      (insert diff-output)
      (unless (or (string-empty-p diff-output)
                  (string-suffix-p "\n" diff-output))
        (insert "\n")))
    (save-restriction
      (narrow-to-region start (point))
      (goto-char (point-min))
      (majutsu-diff-wash-diffs args))))

(defconst majutsu-diff--revision-metadata-field-count 11
  "Number of tokens emitted in each revision metadata record.")

(defconst majutsu-diff--revision-metadata-separator (string 0)
  "NUL separator between encoded revision metadata tokens.")

(defconst majutsu-diff--revision-metadata-template
  (majutsu-template-compile
   `[:concat
     [:join ,majutsu-diff--revision-metadata-separator
            [:label "change_id" [:json [:change_id]]]
            [:label "local_bookmarks map join name"
                    [:json [:local_bookmarks :map [:|ref| [:name]]]]]
            [:label "remote_bookmarks map join name remote"
                    [:concat
                     "["
                     [:remote_bookmarks
                      :map [:|ref|
                            [:concat "[" [:json [:name]] "," [:json [:remote]] "]"]]
                      :join ","]
                     "]"]]
            [:label "commit_id" [:json [:commit_id]]]
            [:label "author name" [:json [:author :name]]]
            [:label "author email" [:json [:author :email]]]
            [:label "author timestamp format"
                    [:json [:author :timestamp :format "%a %b %e %T %Y %z"]]]
            [:label "committer name" [:json [:committer :name]]]
            [:label "committer email" [:json [:committer :email]]]
            [:label "committer timestamp format"
                    [:json [:committer :timestamp :format "%a %b %e %T %Y %z"]]]
            [:label "description" [:json [:description]]]]
     ,majutsu-diff--revision-metadata-separator]
   'Commit)
  "Template for labelled, unambiguous revision metadata tokens.")

(defun majutsu-diff--range-option (long &optional short)
  "Return LONG (or SHORT) option value from the current diff range.
Both `--option=VALUE' and the two-argument `--option VALUE' forms are
accepted.  Return the symbol `missing' when the option is absent, and an
empty string when it is present without a value."
  (let ((args majutsu-buffer-diff-range)
        (value 'missing))
    (while (and args (eq value 'missing))
      (let ((arg (pop args)))
        (cond
         ((or (equal arg long) (and short (equal arg short)))
          (setq value (if (and args (stringp (car args)))
                          (pop args)
                        "")))
         ((and (stringp arg)
               (string-prefix-p (concat long "=") arg))
          (setq value (substring arg (1+ (length long))))))))
    value))

(defun majutsu-diff--metadata-revision ()
  "Return the revision eligible for display metadata, or nil.
Metadata is only appropriate for a single-revision change diff: the
project default (working-copy revision `@') or an explicit `-r' /
`--revisions' revset.  Explicit `--from' or `--to' ranges never qualify;
the metadata query later rejects revsets resolving to zero or many changes."
  (let ((from (majutsu-diff--range-option "--from"))
        (to (majutsu-diff--range-option "--to"))
        (revisions (majutsu-diff--range-option "--revisions" "-r")))
    (cond
     ((or (not (eq from 'missing)) (not (eq to 'missing))) nil)
     ((null majutsu-buffer-diff-range) "@")
     ((and (stringp revisions) (not (string-empty-p revisions))) revisions)
     (t nil))))

(defun majutsu-diff--presentation-properties (value)
  "Return face properties at the beginning of string VALUE."
  (when (and (stringp value) (not (string-empty-p value)))
    (let ((face (get-text-property 0 'face value))
          (font-lock-face (get-text-property 0 'font-lock-face value)))
      (append (and face (list 'face face))
              (and font-lock-face (list 'font-lock-face font-lock-face))))))

(defun majutsu-diff--apply-presentation-properties (value properties)
  "Apply PROPERTIES recursively to strings in JSON VALUE."
  (cond
   ((stringp value)
    (when (and properties (not (string-empty-p value)))
      (add-text-properties 0 (length value) properties value))
    value)
   ((vectorp value)
    (dotimes (index (length value))
      (aset value index
            (majutsu-diff--apply-presentation-properties
             (aref value index) properties)))
    value)
   (t value)))

(defun majutsu-diff--decode-metadata-token (token)
  "Decode one labelled JSON TOKEN and transfer its presentation properties."
  (let ((properties (majutsu-diff--presentation-properties token))
        (decoded (json-parse-string (substring-no-properties token)
                                    :array-type 'array
                                    :null-object :null)))
    (majutsu-diff--apply-presentation-properties decoded properties)))

(defun majutsu-diff--parse-revision-metadata (output)
  "Parse one NUL-framed revision metadata record from OUTPUT.
Return a plist, or nil for malformed output and revsets resolving to zero
or multiple changes.  Each field is JSON, so NUL, terminal escapes, and other
control characters remain data while externally applied jj labels survive as
Emacs presentation properties."
  (condition-case nil
      (let* ((ansi-color-apply-face-function
              #'ansi-color-apply-text-property-face)
             (colored (ansi-color-apply (or output "")))
             (tokens (majutsu--split-fields
                      colored majutsu-diff--revision-metadata-separator)))
        (when (and (= (length tokens)
                      (1+ majutsu-diff--revision-metadata-field-count))
                   (string-empty-p (car (last tokens))))
          (let ((record
                 (vconcat
                  (mapcar #'majutsu-diff--decode-metadata-token
                          (butlast tokens)))))
            (pcase-let ((`[,change-id ,local ,remote ,commit-id
                           ,author-name ,author-email ,author-date
                           ,committer-name ,committer-email ,committer-date
                           ,description]
                         record))
              (when (and (seq-every-p
                          #'stringp
                          (list change-id commit-id
                                author-name author-email author-date
                                committer-name committer-email committer-date
                                description))
                         (vectorp local)
                         (seq-every-p #'stringp local)
                         (vectorp remote)
                         (seq-every-p
                          (lambda (pair)
                            (and (vectorp pair)
                                 (= (length pair) 2)
                                 (stringp (aref pair 0))
                                 (stringp (aref pair 1))))
                          remote))
                (list :change-id change-id
                      :local-bookmarks (append local nil)
                      :remote-bookmarks
                      (mapcar (lambda (pair) (append pair nil)) remote)
                      :commit-id commit-id
                      :author-name author-name
                      :author-email author-email
                      :author-date author-date
                      :committer-name committer-name
                      :committer-email committer-email
                      :committer-date committer-date
                      :description description))))))
    (error nil)))

(defun majutsu-diff--query-revision-metadata (rev)
  "Query all display metadata for REV with one `jj log' invocation.
Return nil if the command fails or REV does not resolve to exactly one
change."
  (let ((majutsu-jj-global-arguments
         (cons "--color=always"
               (seq-remove (lambda (arg) (string-prefix-p "--color" arg))
                           majutsu-jj-global-arguments))))
    (condition-case nil
        (with-temp-buffer
          (when (zerop (majutsu-jj-insert
                        "log" "--no-graph" "--limit" "2"
                        "-r" rev "-T"
                        majutsu-diff--revision-metadata-template))
            (majutsu-diff--parse-revision-metadata (buffer-string))))
      (error nil))))

(defun majutsu-diff--revision-metadata ()
  "Return cached metadata for the current single-revision diff.
The cache key includes formatting arguments, range arguments, and the
selected revision.  Consequently changing any diff argument or revision
invalidates the cache while the two metadata section inserters share one
query."
  (let* ((rev (majutsu-diff--metadata-revision))
         (key (list (copy-sequence majutsu-buffer-diff-args)
                    (copy-sequence majutsu-buffer-diff-range)
                    rev)))
    (if (and majutsu-diff--revision-metadata-cache
             (equal key (car majutsu-diff--revision-metadata-cache)))
        (cdr majutsu-diff--revision-metadata-cache)
      (let ((metadata (and rev (majutsu-diff--query-revision-metadata rev))))
        (setq majutsu-diff--revision-metadata-cache (cons key metadata))
        metadata))))

(defun majutsu-diff--propertize-ref (ref face)
  "Return REF propertized like a Magit ref label using FACE."
  (let* ((properties (majutsu-diff--presentation-properties ref))
         (display
          (apply #'concat
                 (mapcar
                  (lambda (char)
                    (pcase char
                      (?\\ "\\\\")
                      (?\n "\\n")
                      (?\r "\\r")
                      (?\t "\\t")
                      ((pred (lambda (value)
                               (or (< value 32) (= value 127))))
                       (format "\\x%02X" char))
                      (_ (char-to-string char))))
                  (string-to-list ref)))))
    (when (not (string-empty-p display))
      (add-text-properties
       0 (length display)
       (append (or properties (list 'font-lock-face face))
               (list 'help-echo (substring-no-properties ref)))
       display))
    display))

(defun majutsu-diff--with-fallback-face (value face)
  "Return VALUE with FACE unless it already has presentation properties."
  (if (or (not (stringp value))
          (string-empty-p value)
          (majutsu-diff--presentation-properties value))
      value
    (propertize value 'font-lock-face face)))

(defun majutsu-diff--format-refs (fields)
  "Return Magit-style local and remote ref labels from FIELDS.
The synthetic `git' remote used by jj's Git backend is omitted."
  (string-join
   (delete-dups
    (append
     (mapcar (lambda (ref)
               (majutsu-diff--propertize-ref ref 'magit-branch-local))
             (plist-get fields :local-bookmarks))
     (delq nil
           (mapcar
            (lambda (ref)
              (pcase-let ((`(,name ,remote) ref))
                (unless (string= remote "git")
                  (majutsu-diff--propertize-ref
                   (format "%s/%s" remote name)
                   'magit-branch-remote))))
            (plist-get fields :remote-bookmarks)))))
   " "))

(defun majutsu-insert-diff-revision-headers ()
  "Insert collapsible revision headers for a single-change diff."
  (when-let* ((fields (majutsu-diff--revision-metadata)))
    (magit-insert-section (commit-headers)
      (let ((refs (majutsu-diff--format-refs fields)))
        (magit-insert-heading
          (unless (string-empty-p refs) (concat refs " "))
          (majutsu-diff--with-fallback-face
           (plist-get fields :commit-id) 'magit-hash)))
      (insert "Author:     " (plist-get fields :author-name)
              " <" (plist-get fields :author-email) ">\n"
              "AuthorDate: " (plist-get fields :author-date) "\n"
              "Commit:     " (plist-get fields :committer-name)
              " <" (plist-get fields :committer-email) ">\n"
              "CommitDate: " (plist-get fields :committer-date) "\n\n"))))

(defun majutsu-insert-diff-revision-message ()
  "Insert a collapsible revision message for a single-change diff."
  (when-let* ((fields (majutsu-diff--revision-metadata)))
    (let ((message (plist-get fields :description)))
      (magit-insert-section
          (commit-message nil nil
            :heading-highlight-face 'magit-diff-revision-summary-highlight)
        (if (string-empty-p message)
            (progn
              (magit-insert-heading "(no description)")
              (insert "\n"))
          (save-excursion
            (insert message)
            (unless (bolp) (insert "\n")))
          (magit--add-face-text-property
           (point) (progn (forward-line) (point))
           'magit-diff-revision-summary t)
          (magit-insert-heading)
          (goto-char (point-max)))
        (insert "\n")))))

(defun majutsu-insert-diff ()
  "Insert a diff section and wash it."
  (let* ((args (majutsu-jj-append-filesets
                (append (list "diff")
                        majutsu-buffer-diff-args
                        majutsu-buffer-diff-range)
                majutsu-buffer-diff-filesets))
         (backend (majutsu-diff--sync-backend args))
         (washer (majutsu-diff--backend-washer backend)))
    (magit-insert-section (diff-root)
      (magit-insert-heading (format "jj %s" (string-join args " ")))
      (majutsu-jj-wash
          (lambda (wash-args)
            ;; The Git command has already snapshotted the working copy.
            ;; Query the same range without snapshotting again, then
            ;; attach exact paths/statuses only if the ordered raw headers and
            ;; operation kinds agree.
            (let* ((metadata (and (eq backend 'git)
                                  (majutsu-diff--query-file-metadata
                                   majutsu-buffer-diff-range
                                   majutsu-buffer-diff-filesets)))
                   (matching-git-metadata
                    (and (majutsu-diff--raw-file-metadata-consistent-p metadata)
                         metadata))
                   (majutsu-diff--active-file-metadata matching-git-metadata)
                   (majutsu-diff--file-metadata-queue
                    (copy-sequence matching-git-metadata)))
              (funcall washer wash-args)
              (majutsu-diff--attach-file-metadata metadata)))
        'wash-anyway args))))

;;; Diff wash

(defun majutsu-diff-wash-diffstat ()
  "Wash the diffstat produced by `jj diff --stat'.

Assumes point is at the start of the diff output and that the output was
generated using `--git --stat', meaning the diffstat appears before the
first \"diff --git\" header."
  (let ((files (majutsu-diff--collect-diff-files))
        heading
        (beg (point)))
    ;; Like `magit-diff-wash-diffstat': find the summary line first, delete it,
    ;; then rewrite each stat line as a `diffstat' section.
    (when (re-search-forward "^ ?\\([0-9]+ +files? change[^\n]*\n\\)" nil t)
      (setq heading (match-string 1))
      (delete-region (match-beginning 0) (match-end 0))
      (goto-char beg)
      (magit-insert-section (diffstat)
        (magit-insert-heading
          (propertize heading 'font-lock-face 'magit-diff-file-heading))
        (while (looking-at majutsu-diff-statline-re)
          (let* ((file (match-string-no-properties 1))
                 (sep  (match-string 2))
                 (cnt  (match-string 3))
                 (add  (match-string 4))
                 (del  (match-string 5)))
            (majutsu-diff--delete-line)
            (when (string-match " +$" file)
              (setq sep (concat (match-string 0 file) sep))
              (setq file (substring file 0 (match-beginning 0))))
            (setq file (string-trim-right file))
            (magit-insert-section (jj-file (or (pop files) file))
              (insert (magit-format-file 'stat file 'magit-filename))
              (insert sep)
              (cond
               ((string-match "\\`(binary)\\(?: +\\([+-][0-9]+\\) bytes\\)?\\'" cnt)
                (insert (propertize "(binary)" 'font-lock-face
                                    'majutsu-diffstat-binary))
                (when-let* ((delta (match-string 1 cnt)))
                  (insert " "
                          (propertize delta 'font-lock-face
                                      (if (string-prefix-p "-" delta)
                                          'magit-diffstat-removed
                                        'magit-diffstat-added))
                          " bytes")))
               (t
                (insert cnt)))
              (insert " ")
              (when add
                (insert (propertize add 'font-lock-face
                                    'magit-diffstat-added)))
              (when del
                (insert (propertize del 'font-lock-face
                                    'magit-diffstat-removed)))
              (insert "\n"))))
        (if (looking-at "^$") (forward-line) (insert "\n"))))))

(defun majutsu-diff-wash-diffs (args)
  "Parse a jj diff already inserted into the current buffer.
Assumes point is at the start of the diff output."
  (goto-char (point-min))
  (when (member "--stat" args)
    (majutsu-diff-wash-diffstat))
  (goto-char (point-min))
  (when (re-search-forward "^diff --git " nil t)
    (goto-char (match-beginning 0)))
  (when (looking-at "^diff --git ")
    (while (and (not (eobp))
                (looking-at "^diff --git "))
      (let ((before (point)))
        (majutsu-diff-wash-file)
        ;; A malformed or future Git header must never trap refresh in a
        ;; non-advancing parser loop.
        (when (= before (point))
          (forward-line 1))))
    (unless (bolp) (insert "\n"))))

(defun majutsu-diff-wash-file ()
  "Parse a single file section at point and wrap it in Magit sections."
  (when (looking-at "^diff --git ")
    (let* ((diff-line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))
           (metadata (pop majutsu-diff--file-metadata-queue))
           (fallback
            (if (string-match "^diff --git a/\\(.*\\) b/\\(.*\\)$" diff-line)
                (or (match-string 2 diff-line) (match-string 1 diff-line))
              (string-remove-prefix "diff --git " diff-line)))
           (file (or (plist-get metadata :target) fallback))
           (headers nil)
           (diff-header (buffer-substring-no-properties
                         (line-beginning-position)
                         (min (point-max) (1+ (line-end-position))))))
      ;; Drop the diff header line; keep the rest of the text in place.
      (majutsu-diff--delete-line)
      ;; Collect extended headers
      (while (and (not (eobp))
                  (not (looking-at "^diff --git "))
                  (not (looking-at "^@@ ")))
        (push (buffer-substring-no-properties (line-beginning-position)
                                              (min (point-max)
                                                   (1+ (line-end-position))))
              headers)
        (majutsu-diff--delete-line))
      (setq headers (nreverse headers))
      (magit-insert-section
          (jj-file file nil
                   :header (concat diff-header (string-join headers "")))
        (magit-insert-heading
          (propertize (majutsu-diff--file-heading file headers)
                      'font-lock-face 'magit-diff-file-heading))
        ;; Hunk bodies remain in the buffer; just wrap them.
        (while (and (not (eobp)) (looking-at "^@@ "))
          (majutsu-diff-wash-hunk file))
        (insert "\n"))))
  t)

(defun majutsu-diff-wash-hunk (file)
  "Wrap the current hunk in a section for FILE without copying its body."
  (let* ((header (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position)))
         (ranges nil)
         (about nil)
         (from-range nil)
         (to-range nil))
    (when (string-match "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?$" header)
      (setq about (match-string 2 header))
      (setq ranges (mapcar (lambda (str)
                             (let ((nums (mapcar #'string-to-number
                                                 (split-string (substring str 1) ","))))
                               (if (= (length nums) 1)
                                   (append nums (list 1))
                                 nums)))
                           (split-string (match-string 1 header))))
      (when (= (length ranges) 2)
        (setq from-range (car ranges))
        (setq to-range (cadr ranges))))
    ;; Remove original header and insert a propertized one.
    (majutsu-diff--delete-line)
    ;; Use (file . from-range) as unique hunk identity to avoid collisions.
    (magit-insert-section
        (jj-hunk (cons file from-range) nil
                 :from-range from-range
                 :to-range to-range
                 :about about)
      (magit-insert-heading
        (propertize header 'font-lock-face 'magit-diff-hunk-heading))
      (let ((body-start (point)))
        ;; Advance over hunk lines already present in the buffer.
        (while (and (not (eobp))
                    (not (looking-at "^@@ "))
                    (not (looking-at "^diff --git ")))
          (let ((bol (point)))
            (forward-line 1)
            (let ((face (cond
                         ((eq (char-after bol) ?+) 'magit-diff-added)
                         ((eq (char-after bol) ?-) 'magit-diff-removed)
                         (t 'magit-diff-context))))
              (put-text-property bol (point) 'font-lock-face face))))
        (when majutsu-diff-paint-whitespace
          (majutsu-diff--paint-hunk-whitespace body-start (point) file))))))

(defun majutsu-diff--line-matching-p (regexp lines)
  "Return non-nil if any string in LINES matches REGEXP."
  (seq-some (lambda (line) (and line (string-match-p regexp line))) lines))

(defun majutsu--diff-file-status (lines)
  "Infer the jj diff status for a file based on LINES."
  (cond
   ((majutsu-diff--line-matching-p "^new file" lines) "new file")
   ((majutsu-diff--line-matching-p "^deleted file" lines) "deleted")
   ((majutsu-diff--line-matching-p "^rename \\(from\\|to\\)" lines) "renamed")
   ((majutsu-diff--line-matching-p "^copy \\(from\\|to\\)" lines) "copied")
   (t "modified")))

(defun majutsu-diff--file-heading (file lines)
  "Return a formatted heading string for FILE using parsed LINES."
  (format "%-11s %s" (majutsu--diff-file-status lines) file))

;;; Syntax highlighting

(defun majutsu-diff--update-hunk-syntax (&optional section)
  "Apply syntax highlighting overlays to hunk SECTION.
When SECTION is nil, walk all hunk sections."
  (if section
      (when (and (not (oref section hidden))
                 (not (oref section fontified))
                 (fboundp 'diff-syntax-fontify-props)
                 (fboundp 'diff-hunk-text)
                 (not (eq majutsu-diff-backend 'color-words)))
        (oset section fontified t)
        (save-excursion
          (goto-char (oref section content))
          (let* ((file (magit-section-parent-value section))
                 (revs (majutsu-diff--revisions))
                 (old (majutsu-diff--get-hunk-syntax section (car revs) file t))
                 (new (majutsu-diff--get-hunk-syntax
                       section
                       (if (majutsu-diff--visit-workspace-p) nil (cdr revs))
                       file nil)))
            (while (< (point) (oref section end))
              (pcase-dolist (`(,beg ,end ,face)
                             (pcase (char-after)
                               (?- (pop old))
                               (?+ (pop new))
                               (?\s (pop old)
                                    (pop new))))
                (let ((ov (make-overlay (+ (point) 1 beg)
                                        (+ (point) 1 end)
                                        nil t)))
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'face face)
                  (overlay-put ov 'majutsu-diff-syntax t)))
              (forward-line 1)))))
    (cl-labels ((walk (node)
                  (if (magit-section-match 'jj-hunk node)
                      (majutsu-diff--update-hunk-syntax node)
                    (dolist (child (oref node children))
                      (walk child)))))
      (walk magit-root-section))))

(defun majutsu-diff--get-hunk-syntax (section rev file from)
  "Return syntax fontification props for SECTION side REV FILE.
When FROM is non-nil, fontify the old side; otherwise fontify the new side."
  (with-demoted-errors "Error getting hunk syntax highlighting: %S"
    (let ((args (majutsu-diff--get-hunk-text section from)))
      (when (and args (not (listp (car (cadr args)))))
        (require 'majutsu-file)
        (with-current-buffer (majutsu-find-file-noselect rev file)
          (save-excursion
            (apply #'diff-syntax-fontify-props nil args)))))))

(defun majutsu-diff--get-hunk-text (section from)
  "Return text and line range for SECTION side.
When FROM is non-nil, return removed/context text; otherwise return
added/context text."
  (let* ((range (if from (oref section from-range) (oref section to-range)))
         (line (car range))
         (lines (cadr range)))
    (when range
      (list (string-trim-right
             (diff-hunk-text (buffer-substring-no-properties
                              (oref section start) (oref section end))
                             (not from) nil))
            (list line lines)))))

;;; Refinement

(defun majutsu-diff--update-hunk-refinement (&optional section allow-remove)
  "Apply or remove word-level refinement overlays.
When SECTION is nil, walk all hunk sections."
  (if section
      (let ((current (eq section (magit-current-section))))
        (if (and allow-remove
                 (or (null majutsu-diff-refine-hunk)
                     (and (eq majutsu-diff-refine-hunk t)
                          (oref section refined)
                          (not current))))
            (progn
              (oset section refined nil)
              (remove-overlays (oref section start)
                               (oref section end)
                               'diff-mode 'fine))
          (unless (oref section hidden)
            (pcase (list majutsu-diff-refine-hunk
                         (oref section refined)
                         current)
              ((or `(all nil ,_) '(t nil t))
               (oset section refined t)
               (if (eq majutsu-diff-backend 'color-words)
                   (majutsu-diff--color-words-refine-hunk section)
                 (save-excursion
                   (goto-char (oref section start))
                   (let ((smerge-refine-ignore-whitespace
                          majutsu-diff-refine-ignore-whitespace)
                         (write-region-inhibit-fsync t))
                     (diff-refine-hunk)))))))))
    (cl-labels ((walk (node)
                  (if (magit-section-match 'jj-hunk node)
                      (majutsu-diff--update-hunk-refinement node t)
                    (dolist (child (oref node children))
                      (walk child)))))
      (walk magit-root-section))))

(defun majutsu-diff--color-words--overlay-at (pos predicate)
  "Return overlay at POS satisfying PREDICATE, or nil."
  (let ((ols (overlays-at pos)))
    (while (and ols (not (funcall predicate (car ols))))
      (pop ols))
    (car ols)))

(defun majutsu-diff--color-words--region-overlay-at (pos)
  "Return region overlay at POS (or just before)."
  (let ((pred (lambda (ov) (overlay-get ov 'smerge--refine-region))))
    (or (majutsu-diff--color-words--overlay-at pos pred)
        (when (> pos (point-min))
          (majutsu-diff--color-words--overlay-at (1- pos) pred)))))

(defun majutsu-diff--color-words--span-stream-length (spans)
  "Return total length of SPANS stream."
  (let ((len 0))
    (dolist (span spans)
      (setq len (+ len (- (cdr span) (car span)))))
    len))

(defun majutsu-diff--color-words--span-stream-offset (pos spans)
  "Return POS offset in SPANS stream.
Counts only SPANS before POS.  If POS is inside a span, include the
partial offset.  Return nil when SPANS is nil." 
  (when spans
    (let ((offset 0))
      (cl-block nil
        (dolist (span spans)
          (let ((s (car span))
                (e (cdr span)))
            (cond
             ((<= pos s)
              (cl-return offset))
             ((< pos e)
              (setq offset (+ offset (- pos s)))
              (cl-return offset))
             (t
              (setq offset (+ offset (- e s)))))))
        offset))))

(defun majutsu-diff--color-words--span-stream-pos (offset spans)
  "Return buffer position for OFFSET into SPANS stream." 
  (let* ((total (majutsu-diff--color-words--span-stream-length spans))
         (remaining (max 0 (min offset (max 0 (1- total))))))
    (or
     (cl-block nil
       (dolist (span spans)
         (let* ((s (car span))
                (e (cdr span))
                (len (- e s)))
           (when (< remaining len)
             (cl-return (+ s remaining)))
           (setq remaining (- remaining len)))))
     (when spans
       (max (caar (last spans)) (1- (cdar (last spans))))))))

(defun majutsu-diff--color-words--token-anchors (token-spans non-token-spans)
  "Return alist mapping non-token offsets to TOKEN-SPANS.
Each entry is (OFFSET . (SPAN...)) with SPAN = (START . END) in buffer order."
  (let (anchors)
    (dolist (span token-spans)
      (let* ((start (car span))
             (offset (or (majutsu-diff--color-words--span-stream-offset
                          start non-token-spans)
                         0))
             (cell (assoc offset anchors)))
        (if cell
            (setcdr cell (cons span (cdr cell)))
          (push (cons offset (list span)) anchors))))
    (dolist (cell anchors)
      (setcdr cell (nreverse (cdr cell))))
    (nreverse anchors)))

(defun majutsu-diff--color-words-shadow-pos (cursor)
  "Compute shadow cursor position for color-words geometry at CURSOR.
Return buffer position, or nil if no mapping is possible.

Map using non-token offsets to align shared context.  When CURSOR sits
inside a token span, prefer a token span anchored at the same offset on
the other side; otherwise fall back to the non-token stream." 
  (let ((region-ov (majutsu-diff--color-words--region-overlay-at cursor)))
    (when region-ov
      (let* ((region-other (overlay-get region-ov 'majutsu-color-words-region-other))
             (from-spans (overlay-get region-ov 'majutsu-color-words-non-token))
             (to-spans (and region-other
                            (overlay-get region-other 'majutsu-color-words-non-token)))
             (from-tokens (overlay-get region-ov 'majutsu-color-words-token-anchors))
             (to-tokens (and region-other
                             (overlay-get region-other 'majutsu-color-words-token-anchors)))
             (offset (or (majutsu-diff--color-words--span-stream-offset
                          cursor from-spans)
                         (and from-tokens 0)))
             (scaled (cond
                      ((and to-spans offset)
                       (min offset (1- (majutsu-diff--color-words--span-stream-length
                                        to-spans))))
                      (offset offset))))
        (when (and offset scaled)
          (let* ((from-bucket (cdr (assoc offset from-tokens)))
                 (to-bucket (cdr (assoc scaled to-tokens)))
                 (token-hit (and from-bucket
                                 (seq-find (lambda (span)
                                             (and (<= (car span) cursor)
                                                  (< cursor (cdr span))))
                                           from-bucket))))
            (cond
             ((and token-hit to-bucket)
              (car (car to-bucket)))
             (to-spans
              (majutsu-diff--color-words--span-stream-pos scaled to-spans)))))))))

(defun majutsu-diff--color-words-shadow-cursor (window _oldpos dir)
  "Shadow cursor callback for color-words diffs.
Unlike `smerge--refine-shadow-cursor', this maps through inline pairs
and falls back to region mapping for shared context."
  (let ((ol (window-parameter window 'smerge--refine-shadow-cursor)))
    (if (not (and (bound-and-true-p smerge-refine-shadow-cursor)
                  (memq dir '(entered moved))))
        (when ol (delete-overlay ol))
      (with-current-buffer (window-buffer window)
        (let ((other-beg (ignore-errors
                           (majutsu-diff--color-words-shadow-pos
                            (window-point window)))))
          (if (not other-beg)
              (when ol (delete-overlay ol))
            (let ((other-end (min (point-max) (1+ other-beg))))
              ;; Handle wide chars (TAB/LF) — show as pseudo-space.
              (when (memq (char-after other-beg) '(?\n ?\t))
                (setq other-end other-beg))
              (if ol (move-overlay ol other-beg other-end)
                (setq ol (make-overlay other-beg other-end nil t nil))
                (setf (window-parameter window 'smerge--refine-shadow-cursor)
                      ol)
                (overlay-put ol 'window window)
                (overlay-put ol 'face 'smerge-refine-shadow-cursor))
              (overlay-put ol 'before-string
                           (when (= other-beg other-end)
                             (propertize
                              " " 'face 'smerge-refine-shadow-cursor))))))))))

(defun majutsu-diff--color-words-refine-hunk (section)
  "Apply word-level refinement to a color-words SECTION.
Walk the hunk body to find colored word spans (identified by debug
labels when available, otherwise ANSI-derived `font-lock-face') and
create three kinds of overlays:

1. `smerge--refine-region' overlays covering the full extent of each
   removed/added block.  These carry `cursor-sensor-functions' with
   `majutsu-diff--color-words-shadow-cursor' (NOT the smerge version,
   which assumes separated geometry).

2. `diff-mode fine' overlays on non-token spans, cross-linked via
   `smerge--refine-other' as anchors for precise shadow-cursor mapping.

3. Token overlays on underlined spans with `diff-refine-removed'/
   `diff-refine-added' faces.  When available, token spans come from
   jj `--color=debug' labels (exactly matching jj's inline tokenization);
   otherwise fall back to ANSI face spans.

This trusts jj's own word-level diff: underlined = unique to one
side, non-underlined colored = shared context within a change."
  (require 'majutsu-color-words)
  (require 'smerge-mode)
  (let* ((beg (oref section content))
         (end (oref section end))
         ;; Prefer debug-labeled spans when available; otherwise use ANSI faces.
         (debug-spans (majutsu-color-words--collect-debug-change-spans beg end))
         (ansi-spans (majutsu-color-words--collect-change-spans beg end))
         (change-spans (or debug-spans ansi-spans))
         (region-pairs (majutsu-color-words--group-change-pairs change-spans))
         ;; For token overlays, prefer explicit debug token labels because
         ;; they mirror jj's rendered token stream exactly.
         (debug-token-spans (majutsu-color-words--collect-debug-token-spans beg end))
         (token-pairs (if debug-token-spans
                          (majutsu-color-words--group-change-pairs debug-token-spans)
                        region-pairs)))

    (dolist (pair region-pairs)
      (pcase-let ((`(,rbeg ,rend ,abeg ,aend ,r-ul ,a-ul ,r-non ,a-non) pair))
        ;; 1. Region overlays for shadow cursor activation.
        (let (r-region a-region)
          (when rbeg
            (setq r-region (make-overlay rbeg rend nil 'front-advance))
            (overlay-put r-region 'evaporate t)
            (overlay-put r-region 'diff-mode 'fine)
            (overlay-put r-region 'smerge--refine-region t)
            (overlay-put r-region 'majutsu-color-words-non-token r-non)
            (overlay-put r-region 'majutsu-color-words-token-anchors
                         (majutsu-diff--color-words--token-anchors r-ul r-non))
            (overlay-put r-region 'cursor-sensor-functions
                         '(majutsu-diff--color-words-shadow-cursor)))
          (when abeg
            (setq a-region (make-overlay abeg aend nil 'front-advance))
            (overlay-put a-region 'evaporate t)
            (overlay-put a-region 'diff-mode 'fine)
            (overlay-put a-region 'smerge--refine-region t)
            (overlay-put a-region 'majutsu-color-words-non-token a-non)
            (overlay-put a-region 'majutsu-color-words-token-anchors
                         (majutsu-diff--color-words--token-anchors a-ul a-non))
            (overlay-put a-region 'cursor-sensor-functions
                         '(majutsu-diff--color-words-shadow-cursor)))
          (when (and r-region a-region)
            (overlay-put r-region 'majutsu-color-words-region-other a-region)
            (overlay-put a-region 'majutsu-color-words-region-other r-region)))))

    (dolist (pair token-pairs)
      (pcase-let ((`(,_rbeg ,_rend ,_abeg ,_aend ,r-ul ,a-ul ,_r-non ,_a-non) pair))
        ;; 2. Token overlays on underlined spans — visual refinement only.
        (dolist (span r-ul)
          (let ((ol (make-overlay (car span) (cdr span))))
            (overlay-put ol 'diff-mode 'fine)
            (overlay-put ol 'evaporate t)
            (overlay-put ol 'face 'diff-refine-removed)
            (overlay-put ol 'majutsu-color-words-token t)))
        (dolist (span a-ul)
          (let ((ol (make-overlay (car span) (cdr span))))
            (overlay-put ol 'diff-mode 'fine)
            (overlay-put ol 'evaporate t)
            (overlay-put ol 'face 'diff-refine-added)
            (overlay-put ol 'majutsu-color-words-token t)))))
    ;; Enable cursor-sensor-mode for shadow cursor.
    (when (bound-and-true-p smerge-refine-shadow-cursor)
      (cursor-sensor-mode 1))))

(cl-defmethod magit-section--refine ((section majutsu-hunk-section))
  ;; For both backends, delegate to the unified refinement handler.
  ;; Color-words uses `majutsu-diff--color-words-refine-hunk' internally;
  ;; the git backend uses `diff-refine-hunk'.
  (when (eq majutsu-diff-fontify-hunk t)
    (majutsu-diff--update-hunk-syntax section))
  (when (eq majutsu-diff-refine-hunk t)
    (majutsu-diff--update-hunk-refinement section)))

(defun majutsu-diff--color-words-paint (section highlight)
  "Apply or remove a focus background overlay for a color-words SECTION.
When HIGHLIGHT is non-nil, create a single overlay with
`magit-diff-context-highlight' covering the hunk body; otherwise
remove it.  A single low-priority overlay is used so that per-word
ANSI `font-lock-face' foreground colors show through."
  (let ((start (oref section start))
        (end   (oref section end)))
    (dolist (ov (overlays-in start end))
      (when (overlay-get ov 'majutsu-color-words-highlight)
        (delete-overlay ov)))
    (when highlight
      (save-excursion
        (goto-char start)
        (forward-line)                  ; skip hunk heading
        (let ((ov (make-overlay (point) end nil t)))
          (overlay-put ov 'majutsu-color-words-highlight t)
          (overlay-put ov 'face 'majutsu-diff-color-words-focus)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'priority -1))))))

(cl-defmethod magit-section-paint ((section majutsu-hunk-section) highlight)
  "Paint a hunk so focus highlighting behaves like Magit.

This mirrors `magit-section-paint' for `magit-hunk-section' but
works with the simplified jj diff we render here.

For the color-words backend, overlays with low priority are used
so that per-word ANSI `font-lock-face' foreground colors show
through while the background changes to indicate focus."
  (if (eq majutsu-diff-backend 'color-words)
      (majutsu-diff--color-words-paint section highlight)
    (let* ((highlight-body (if (boundp 'magit-diff-highlight-hunk-body)
                               magit-diff-highlight-hunk-body
                             t))
           (do-highlight (and highlight highlight-body))
           (end (oref section end)))
      (save-excursion
        ;; Skip the hunk header.
        (goto-char (oref section start))
        (forward-line)
        (while (< (point) end)
          (let* ((line-start (point))
                 (line-end (line-end-position))
                 (face (cond
                        ((looking-at "^\\+")
                         (if do-highlight
                             'magit-diff-added-highlight
                           'magit-diff-added))
                        ((looking-at "^-")
                         (if do-highlight
                             'magit-diff-removed-highlight
                           'magit-diff-removed))
                        (t
                         (if do-highlight
                             'magit-diff-context-highlight
                           'magit-diff-context)))))
            (put-text-property line-start (1+ line-end)
                               'font-lock-face face))
          (forward-line)))))
  (oset section painted (if highlight 'highlight 'plain)))

;;; Navigation

(defun majutsu-goto-diff-line ()
  "Jump to the line in the file corresponding to the diff line at point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (_ (magit-section-match 'jj-hunk section))
              (file (magit-section-parent-value section)))
    (let* ((to-range (oref section to-range))
           (start-line (and to-range (car to-range))))
      (unless start-line
        (let ((header (save-excursion
                        (goto-char (oref section start))
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))))
          (when (string-match "^@@.*\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?.*@@" header)
            (setq start-line (string-to-number (match-string 1 header))))))
      (when start-line
        (let* ((start-line start-line)
               ;; Calculate which line within the hunk we're on
               (hunk-start (oref section start))
               (current-pos (point))
               (line-offset 0)
               (full-file-path (expand-file-name file default-directory)))
          ;; Count lines from hunk start to current position
          (save-excursion
            (goto-char hunk-start)
            (forward-line 1) ; Skip hunk header
            (while (< (point) current-pos)
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                ;; Only count context and added lines for line numbering
                (unless (string-prefix-p "-" line)
                  (setq line-offset (1+ line-offset))))
              (forward-line 1)))
          ;; Open file and jump to calculated line
          (let ((target-line (+ start-line line-offset -1))) ; -1 because we start counting from the header
            (find-file full-file-path)
            (goto-char (point-min))
            (forward-line (max 0 target-line))
            (message "Jumped to line %d in %s" (1+ target-line) file)))))))

(defun majutsu-visit-file ()
  "Visit the file at point."
  (interactive)
  (when-let* ((file (majutsu-file-at-point)))
    (find-file (expand-file-name file default-directory))))

(defun majutsu-diff--on-removed-line-p ()
  "Return non-nil if point is on a removed diff line."
  (if-let* ((info (majutsu-diff--color-words-line-info)))
      (majutsu-diff--color-words-goto-from info)
    (eq (char-after (line-beginning-position)) ?-)))

(defun majutsu-diff--color-words-goto-from (info)
  "Return non-nil when color-words navigation should target the old side.
INFO is the plist from `majutsu-color-words-line-info-at-point'."
  (let ((side (majutsu-color-words-side-at-point)))
    (cond
     ((eq side 'removed) t)
     ((eq side 'added) nil)
     (t
      ;; If the line itself is side-specific, keep old behavior.
      (and (plist-get info :from-line)
           (not (plist-get info :to-line)))))))

(defun majutsu-diff--revisions ()
  "Return (FROM-REV . TO-REV) for the current diff buffer.
FROM-REV is the old/left side, TO-REV is the new/right side."
  (let* ((range majutsu-buffer-diff-range)
         (from (transient-arg-value "--from=" range))
         (to (transient-arg-value "--to=" range))
         (revisions (transient-arg-value "--revisions=" range)))
    (cond
     ;; -r REV: diff of a single revision's changes
     ((and range (equal (car range) "-r") (cadr range))
      (let ((rev (cadr range)))
        (cons (concat rev "-") rev)))
     ;; --revisions=REV: same as -r
     (revisions
      (cons (concat revisions "-") revisions))
     ;; --from=X --to=Y: explicit range
     ((or from to)
      (cons (or from "@-") (or to "@")))
     ;; Default: working copy changes (parent to @)
     (t (cons "@-" "@")))))

(defun majutsu-diff--default-revset ()
  "Return the revset implied by the current diff buffer.
If on a removed line, return the from-rev; otherwise return the to-rev."
  (let* ((revs (majutsu-diff--revisions))
         (removed (majutsu-diff--on-removed-line-p)))
    (if removed (car revs) (cdr revs))))

(defun majutsu-diff--hunk-line (section goto-from)
  "Return the line number in SECTION for GOTO-FROM side."
  (save-excursion
    (goto-char (line-beginning-position))
    (with-slots (content from-range to-range) section
      (when (or from-range to-range)
        (when (< (point) content)
          (goto-char content)
          (re-search-forward "^[-+]"))
        (+ (car (if goto-from from-range to-range))
           (let ((target (point))
                 (offset 0))
             (goto-char content)
             (while (< (point) target)
               (let ((ch (char-after)))
                 (cond
                  ((eq ch ?+) (unless goto-from (cl-incf offset)))
                  ((eq ch ?-) (when goto-from (cl-incf offset)))
                  (t (cl-incf offset))))
               (forward-line))
             offset))))))

(defun majutsu-diff--hunk-column (section goto-from)
  "Return the column for SECTION based on GOTO-FROM side."
  (let ((bol (line-beginning-position)))
    (if (or (< (point) (oref section content))
            (and (not goto-from) (eq (char-after bol) ?-)))
        0
      ;; All hunk content lines have a 1-char prefix (+, -, or space)
      (max 0 (1- (current-column))))))

(defun majutsu-diff--goto-line-col (buffer line col)
  "Move point in BUFFER to LINE and COL."
  (with-current-buffer buffer
    (widen)
    (goto-char (point-min))
    (forward-line (max 0 (1- line)))
    (move-to-column col)))

(defun majutsu-diff--visit-workspace-p ()
  "Return non-nil if the current diff should visit the workspace file.
This is true when diffing the working copy (@) on the new/right side."
  (let* ((range majutsu-buffer-diff-range)
         (to (transient-arg-value "--to=" range))
         (revisions (transient-arg-value "--revisions=" range)))
    (cond
     ;; Explicit --to=@ means we're looking at working copy changes
     ((equal to "@") t)
     ;; No range specified defaults to -r @ (working copy)
     ((null range) t)
     ;; Single revision diff (-r @) shows working copy
     ((and revisions (equal revisions "@")) t)
     ;; Otherwise we're looking at committed changes
     (t nil))))

;;;###autoload
(defun majutsu-diff-visit-file (&optional force-workspace)
  "From a diff, visit the appropriate version of the file at point.

If point is on an added or context line, visit the new/right side.
If point is on a removed line, visit the old/left side.

For diffs of the working copy (@), this visits the actual file in
the workspace.  For diffs of committed changes, this visits the
blob from the appropriate revision.

With prefix argument FORCE-WORKSPACE, always visit the workspace file
regardless of what the diff is about."
  (interactive "P")
  (let* ((section (magit-current-section))
         (file (majutsu-file-at-point))
         (backend (majutsu-diff--sync-backend))
         (line-info (and (eq backend 'color-words)
                         (majutsu-diff--color-words-line-info))))
    (unless file
      (user-error "No file at point"))
    (let* ((revs (majutsu-diff--revisions))
           (goto-from (if line-info
                          (majutsu-diff--color-words-goto-from line-info)
                        (and section (magit-section-match 'jj-hunk section)
                             (majutsu-diff--on-removed-line-p))))
           (target-rev (if goto-from (car revs) (cdr revs)))
           (goto-workspace (or force-workspace
                               (and (majutsu-diff--visit-workspace-p)
                                    (not goto-from))))
           (line (cond
                  (line-info
                   (or (and goto-from (plist-get line-info :from-line))
                       (plist-get line-info :to-line)
                       (plist-get line-info :from-line)))
                  ((and section (magit-section-match 'jj-hunk section))
                   (majutsu-diff--hunk-line section goto-from))))
           (col (cond
                 (line-info (majutsu-diff--color-words-column line-info goto-from))
                 ((and section (magit-section-match 'jj-hunk section))
                  (majutsu-diff--hunk-column section goto-from)))))
      (if goto-workspace
          ;; Visit workspace file
          (let ((full-path (expand-file-name file default-directory)))
            (if (file-exists-p full-path)
                (progn
                  (find-file full-path)
                  (when (and line col)
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (move-to-column col)))
              (user-error "File does not exist in workspace: %s" file)))
        ;; Visit blob from appropriate revision
        (let ((buf (majutsu-find-file target-rev file)))
          (when (and buf line col)
            (majutsu-diff--goto-line-col buf line col)))))))

;;; Section Keymaps

(defvar-keymap majutsu-diff-section-map
  :doc "Keymap for diff sections."
  "<remap> <majutsu-visit-thing>" #'majutsu-diff-visit-file
  "C-j" #'majutsu-diff-visit-workspace-file
  "C-<return>" #'majutsu-diff-visit-workspace-file)

;;;###autoload
(defun majutsu-diff-visit-workspace-file ()
  "From a diff, visit the workspace version of the file at point.
Always visits the actual file in the working tree, regardless of
what the diff is about."
  (interactive)
  (majutsu-diff-visit-file t))

(defvar-keymap majutsu-file-section-map
  :doc "Keymap for `jj-file' sections."
  :parent majutsu-diff-section-map)

(defvar-keymap majutsu-hunk-section-map
  :doc "Keymap for `jj-hunk' sections."
  :parent majutsu-diff-section-map)

;;; Diff Commands

(defun majutsu-diff-less-context (&optional count)
  "Decrease the context for diff hunks by COUNT lines."
  (interactive "p")
  (majutsu-diff-set-context (##max 0 (- (or % 0) count))))

(defun majutsu-diff-more-context (&optional count)
  "Increase the context for diff hunks by COUNT lines."
  (interactive "p")
  (majutsu-diff-set-context (##+ (or % 0) count)))

(defun majutsu-diff-default-context ()
  "Reset context for diff hunks to the default height."
  (interactive)
  (majutsu-diff-set-context #'ignore))

(defun majutsu-diff-set-context (fn)
  (let* ((def (if-let* ((context (majutsu-get "diff.git.context")))
                  (string-to-number context)
                3))
         (val majutsu-buffer-diff-args)
         (arg (seq-find (##string-match "^--context=" %) val))
         (num (if arg
                  (string-to-number (substring arg 10))
                def))
         (val (delq arg val))
         (num (funcall fn num))
         (arg (and num (not (= num def)) (format "--context=%d" num)))
         (val (if arg (cons arg val) val)))
    (majutsu-diff--set-buffer-args val)
    (majutsu-refresh)))

(defun majutsu-diff-toggle-refine-hunk (&optional style)
  "Toggle word-level refinement within hunks.
With prefix STYLE, cycle between `all' and `t'."
  (interactive "P")
  (setq-local majutsu-diff-refine-hunk
              (if style
                  (if (eq majutsu-diff-refine-hunk 'all) t 'all)
                (not majutsu-diff-refine-hunk)))
  (majutsu-diff--update-hunk-refinement))

(defun majutsu-diff-toggle-fontify-hunk (&optional style)
  "Toggle syntax highlighting within hunks.
With prefix STYLE, cycle between `all' and `t'."
  (interactive "P")
  (setq-local majutsu-diff-fontify-hunk
              (if style
                  (if (eq majutsu-diff-fontify-hunk 'all) t 'all)
                (not majutsu-diff-fontify-hunk)))
  (majutsu-diff--update-hunk-syntax))

(defvar-keymap majutsu-diff-mode-map
  :doc "Keymap for `majutsu-diff-mode'."
  :parent majutsu-mode-map
  "t" #'majutsu-diff-toggle-refine-hunk
  "T" #'majutsu-diff-toggle-fontify-hunk
  "+" #'majutsu-diff-more-context
  "-" #'majutsu-diff-less-context
  "0" #'majutsu-diff-default-context
  "j" #'majutsu-jump-to-diffstat-or-diff)

(define-derived-mode majutsu-diff-mode majutsu-mode "Majutsu Diff"
  "Major mode for viewing jj diffs."
  :group 'majutsu
  (setq-local line-number-mode nil)
  ;; Majutsu owns refinement state.  Otherwise `diff-font-lock-keywords'
  ;; consults the independent `diff-refine' option and recreates fine overlays
  ;; even when `majutsu-diff-refine-hunk' is nil.
  ;; `magit-section-mode' disables syntactic font locking.  The diff washer
  ;; attaches faces to actual hunk lines as text properties, so revision
  ;; messages cannot be mistaken for patches.
  (setq-local diff-refine nil))

(cl-defmethod majutsu-buffer-value (&context (major-mode majutsu-diff-mode))
  (list majutsu-buffer-diff-args
        majutsu-buffer-diff-range
        majutsu-buffer-diff-filesets))

(defun majutsu-diff-refresh-buffer ()
  "Refresh the current diff buffer."
  (interactive)
  (when majutsu-buffer-diff-args
    ;; Metadata is refreshed once per buffer refresh, then shared by the
    ;; header and message inserters through the argument-keyed cache.
    (setq majutsu-diff--revision-metadata-cache nil)
    (let* ((backend (majutsu-diff--sync-backend))
           (majutsu-jj-global-arguments
            (cons (if (eq backend 'color-words) "--color=debug" "--color=never")
                  (seq-remove (lambda (arg) (string-prefix-p "--color" arg))
                              majutsu-jj-global-arguments)))
           ;; Diff buffers are used as patch source by interactive commands.
           ;; For the regular git backend we force `--color=never', so applying
           ;; `ansi-color' here is unnecessary and corrupts file contents that
           ;; contain literal ANSI escape sequences.  Color-words handles its
           ;; own ANSI/debug output in its washer.
           (majutsu-process-apply-ansi-colors nil))
      ;; Refresh JIT Lock so it renders the faces attached by the washer.
      (font-lock-mode 1)
      (unless (eq backend 'color-words)
        (majutsu-diff--set-left-margin 0))
      (magit-insert-section (diffbuf)
        (magit-run-section-hook 'majutsu-diff-sections-hook))
      (when (eq majutsu-diff-fontify-hunk 'all)
        (majutsu-diff--update-hunk-syntax))
      (when (eq majutsu-diff-refine-hunk 'all)
        (majutsu-diff--update-hunk-refinement)))))

;;;###autoload(autoload 'majutsu-diff-dwim "majutsu-diff" nil t)
(transient-define-suffix majutsu-diff-dwim (&optional args range filesets)
  "Show changes for the thing at point."
  :advice* #'majutsu--transient-with-selection-buffer
  (interactive (majutsu-diff-arguments))
  (let* ((rev (pcase (majutsu-diff--dwim)
                (`(,_ . ,rev) rev)
                (_ "@")))
         (range (or range
                    (list (concat "--revisions=" rev)))))
    (majutsu-diff-setup-buffer args range filesets)))

;;;###autoload
(defun majutsu-diff-revset (revset &optional args _range filesets)
  "Show changes for a REVSET.

REVSET is passed to jj diff using `--revisions='."
  (interactive (cons (majutsu-read-revset "Diff revset")
                     (majutsu-diff-arguments)))
  (let ((range (list (concat "--revisions=" (substring-no-properties revset)))))
    (majutsu-diff-setup-buffer args range filesets)))

;; TODO: implement more DWIM cases
(defun majutsu-diff--dwim ()
  "Return information for performing DWIM diff."
  (when-let* ((rev (or (majutsu-thing-at-point 'jj-revision t)
                       (majutsu-revision-at-point))))
    (cons 'revision rev)))

(defun majutsu-diff-setup-buffer (args range filesets &optional locked)
  "Display a diff buffer configured by ARGS, RANGE and FILESETS."
  (let* ((args (or (majutsu-diff--remembered-args args)
                   (get 'majutsu-diff-mode 'majutsu-diff-default-arguments)))
         (buffer (majutsu-setup-buffer #'majutsu-diff-mode locked
                   (majutsu-buffer-diff-args args)
                   (majutsu-buffer-diff-range range)
                   (majutsu-buffer-diff-filesets filesets))))
    (with-current-buffer buffer
      (majutsu-diff--sync-backend majutsu-buffer-diff-args))
    buffer))

;;; Commands
;;;; Prefix Commands

;;;###autoload(autoload 'majutsu-diff "majutsu-diff" nil t)
(transient-define-prefix majutsu-diff ()
  "Internal transient for jj diff."
  :man-page "jj-diff"
  :class 'majutsu-diff-prefix
  :jj-command "diff"
  :incompatible '(("--revisions=" "--from=")
                  ("--revisions=" "--to=")
                  ("--stat" "--summary")
                  ("--git" "--color-words"))
  :transient-non-suffix t
  :description "JJ Diff"
  [["Selection"
    (majutsu-diff:-r)
    (majutsu-diff:--from)
    (majutsu-diff:--to)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Paths"
    (majutsu-diff:--)]
   ["Options"
    (majutsu-diff:--color-words)
    (majutsu-diff:--git)
    (majutsu-diff:--stat)
    (majutsu-diff:--summary)
    (majutsu-diff:--context)
    (majutsu-diff:-w)
    (majutsu-diff:-b)]
   ["Actions"
    ("d" "Execute" majutsu-diff-dwim)
    ("s" "Save as default" majutsu-diff-save-arguments)
    ("W" "Save as repo default" majutsu-transient-save-repository-defaults)]]
  (interactive)
  (transient-setup
   'majutsu-diff nil nil
   :scope (majutsu-selection-session-begin)))

;;;; Infix Commands

(transient-define-argument majutsu-diff:--git ()
  :description "Show git style diff"
  :class 'transient-switch
  :key "-g"
  :argument "--git")

(transient-define-argument majutsu-diff:--color-words ()
  :description "Show color-words diff"
  :class 'transient-switch
  :key "-W"
  :argument "--color-words")

(transient-define-argument majutsu-diff:--stat ()
  :description "Show stats"
  :class 'transient-switch
  :key "-S"
  :argument "--stat")

(transient-define-argument majutsu-diff:--summary ()
  :description "Show summary"
  :class 'transient-switch
  :shortarg "-s"
  :argument "--summary")

(transient-define-argument majutsu-diff:--context ()
  :description "Context lines"
  :class 'transient-option
  :key "-c"
  :argument "--context="
  :reader #'transient-read-number-N0)

(transient-define-argument majutsu-diff:-- ()
  :description "Limit to files"
  :class 'transient-files
  :key "--"
  :argument "--"
  :prompt "Limit to file,s: "
  :reader #'majutsu-read-files
  :multi-value t)

(transient-define-argument majutsu-diff:-r ()
  :description "Revisions"
  :class 'majutsu-diff-range-option
  :selection-label "[REVS]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :selection-toggle-key "r"
  :shortarg "-r"
  :argument "--revisions="
  :multi-value 'repeat
  :prompt "Revisions"
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-diff:--from ()
  :description "From"
  :class 'majutsu-diff-range-option
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :selection-toggle-key "f"
  :shortarg "-f"
  :argument "--from="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-diff:--to ()
  :description "To"
  :class 'majutsu-diff-range-option
  :selection-label "[TO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :selection-toggle-key "t"
  :shortarg "-t"
  :argument "--to="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-diff:-b ()
  :description "Ignore changes in amount of whitespace"
  :class 'transient-switch
  :shortarg "-b"
  :argument "--ignore-space-change")

(transient-define-argument majutsu-diff:-w ()
  :description "Ignore whitespace"
  :class 'transient-switch
  :shortarg "-w"
  :argument "--ignore-all-space")

(transient-define-suffix majutsu-diff-save-arguments ()
  "Save current transient arguments as global defaults."
  :transient t
  (interactive)
  (unless (and transient--prefix
               (object-of-class-p transient--prefix 'majutsu-diff-prefix))
    (user-error "Not in a Majutsu diff transient"))
  (transient-save-value transient--prefix)
  (message "Saved diff arguments as global defaults"))

;;; _
(provide 'majutsu-diff)
;;; majutsu-diff.el ends here
