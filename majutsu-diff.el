;;; majutsu-diff.el --- Diff viewing and editing for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025-2026 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library implements jj diff buffers and related transients,
;; including context management, refinement, and diffedit helpers.

;;; Code:

(require 'majutsu-core)
(require 'majutsu-mode)
(require 'majutsu-process)
(require 'majutsu-config)
(require 'majutsu-selection)
(require 'majutsu-conflict)
(require 'magit-diff)      ; for faces/font-lock keywords
(require 'diff-mode)
(require 'smerge-mode)

(declare-function majutsu-find-file "majutsu-file" (revset path))
(declare-function majutsu-find-file-at-point "majutsu-file" ())
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
  (list #'majutsu-insert-diff
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

(defcustom majutsu-diff-refine-ignore-whitespace smerge-refine-ignore-whitespace
  "Whether to ignore whitespace while refining hunks."
  :group 'majutsu
  :type 'boolean)

(defcustom majutsu-diff-refine-max-chars 4000
  "Skip word refinement when a hunk spans more than this many characters.
Set to nil to always refine."
  :group 'majutsu
  :type '(choice (const :tag "No limit" nil)
          (integer :tag "Max characters")))

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

(defun majutsu-diff--backend-uses-ansi-p (backend)
  "Return non-nil when BACKEND should be ANSI-processed by process layer.
Color-words keeps raw debug output for marker parsing and applies ANSI later."
  (not (eq backend 'color-words)))

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

(defconst majutsu-diff--range-arg-prefixes
  '("--revisions=" "--from=" "--to=")
  "Prefixes for arguments that restrict the diff range.")

(defun majutsu-diff--range-arg-p (arg)
  "Return non-nil if ARG is a `jj diff' range argument."
  (and (stringp arg)
       (seq-some (lambda (prefix) (string-prefix-p prefix arg))
                 majutsu-diff--range-arg-prefixes)))

(defun majutsu-diff--extract-range-args (args)
  "Return the subset of ARGS that restrict `jj diff' range."
  (seq-filter #'majutsu-diff--range-arg-p args))

(defun majutsu-diff--transient-original-buffer ()
  (and (buffer-live-p transient--original-buffer)
       transient--original-buffer))

(defun majutsu-diff--transient-default-revset ()
  (with-current-buffer (or (majutsu-diff--transient-original-buffer)
                           (current-buffer))
    (or (magit-section-value-if 'jj-commit) "@")))

(defun majutsu-diff--transient-read-revset (prompt initial-input _history)
  (unless current-prefix-arg
    (majutsu-read-revset prompt (or initial-input (majutsu-diff--transient-default-revset)))))

;;; Arguments
;;;; Prefix Classes

(defclass majutsu-diff-prefix (transient-prefix)
  ((history-key :initform 'majutsu-diff)
   (major-mode :initform 'majutsu-diff-mode)))

;;;; Infix Classes

(defclass majutsu-diff-range-option (majutsu-selection-option) ())

(cl-defmethod transient-init-value ((obj majutsu-diff-prefix))
  (pcase-let ((`(,args ,range ,filesets)
               (majutsu-diff--get-value (oref obj major-mode) 'prefix)))
    (oset obj value
          (if filesets
              `(("--" ,@filesets) ,@range ,@args)
            (append range args)))))

(cl-defmethod transient-prefix-value ((obj majutsu-diff-prefix))
  "Return (ARGS RANGE FILESETS) for the Majutsu diff transient.

ARGS are remembered diff formatting arguments.  RANGE is a list of jj
diff range arguments derived from `-r' or `--from/--to'.  FILESETS is a
list of filesets (path filters)."
  (let* ((raw (cl-call-next-method obj))
         (args (majutsu-diff--remembered-args raw))
         (range (majutsu-diff--extract-range-args raw))
         (filesets (cdr (assoc "--" raw))))
    (list args range filesets)))

(cl-defmethod transient-set-value ((obj majutsu-diff-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode))
         (args (car (transient-args (oref obj command)))))
    (put mode 'majutsu-diff-current-arguments args)
    (transient--history-push obj)
    (when (eq major-mode mode)
      (majutsu-diff--set-buffer-args args))))

(cl-defmethod transient-save-value ((obj majutsu-diff-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode))
         (key (intern (format "majutsu-diff:%s" mode)))
         (args (car (transient-args (oref obj command)))))
    (put mode 'majutsu-diff-current-arguments args)
    (setf (alist-get key transient-values) args)
    (transient-save-values)
    (transient--history-push obj)
    (when (eq major-mode mode)
      (majutsu-diff--set-buffer-args args))))

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
                ((plist-member (symbol-plist mode) 'majutsu-diff-current-arguments)
                 (get mode 'majutsu-diff-current-arguments))
                ((when-let* ((elt (assq (intern (format "majutsu-diff:%s" mode))
                                        transient-values)))
                   (cdr elt)))
                (t
                 (get mode 'majutsu-diff-default-arguments))))
         (range (and use-current majutsu-buffer-diff-range))
         (filesets (and use-current majutsu-buffer-diff-filesets)))
    (list args range filesets)))

;; FIXME: 不应该存在
(defun majutsu-diff--set-buffer-args (args &optional _filesets _refresh)
  "Set current buffer's remembered diff formatting ARGS."
  (setq-local majutsu-buffer-diff-args
              (or (majutsu-diff--remembered-args args)
                  (get 'majutsu-diff-mode 'majutsu-diff-default-arguments)))
  (majutsu-diff--sync-backend majutsu-buffer-diff-args)
  (put 'majutsu-diff-mode 'majutsu-diff-current-arguments majutsu-buffer-diff-args))

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

(defun majutsu-diff--collect-diff-files ()
  "Return a list of file paths from \"diff --git\" headers in the buffer.
The list is in the same order as the diff headers appear."
  (save-excursion
    (goto-char (point-min))
    (let (files)
      (while (re-search-forward "^diff --git a/\\(.*\\) b/\\(.*\\)$" nil t)
        (push (match-string-no-properties 2) files))
      (nreverse files))))

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

(defun majutsu-insert-diff ()
  "Insert a diff section and wash it."
  (let* ((args (append (list "diff")
                       majutsu-buffer-diff-args
                       majutsu-buffer-diff-range
                       majutsu-buffer-diff-filesets))
         (backend (majutsu-diff--sync-backend args))
         (washer (majutsu-diff--backend-washer backend)))
    (magit-insert-section (diff-root)
      (magit-insert-heading (format "jj %s" (string-join args " ")))
      (majutsu-jj-wash washer 'wash-anyway args))))

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
      (majutsu-diff-wash-file))
    (unless (bolp) (insert "\n"))))

(defun majutsu-diff-wash-file ()
  "Parse a single file section at point and wrap it in Magit sections."
  (when (looking-at "^diff --git a/\\(.*\\) b/\\(.*\\)$")
    (let* ((file-a (match-string-no-properties 1))
           (file-b (match-string-no-properties 2))
           (file (or file-b file-a))
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
      (magit-insert-section
          (jj-file file nil
                   :header (concat diff-header (string-join (nreverse headers) "")))
        (magit-insert-heading
          (propertize (majutsu-diff--file-heading file (nreverse headers))
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
         (combined nil)
         (from-range nil)
         (from-ranges nil)
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
      (setq combined (= (length ranges) 3))
      (setq from-ranges (and combined (butlast ranges)))
      (setq from-range (if combined (car from-ranges) (car ranges)))
      (setq to-range (car (last ranges))))
    ;; Remove original header and insert a propertized one.
    (majutsu-diff--delete-line)
    ;; Use (file . from-range) as unique hunk identity to avoid collisions.
    (magit-insert-section
        (jj-hunk (cons file from-range) nil
                 :combined combined
                 :from-range from-range
                 :from-ranges from-ranges
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

;;; Refinement

(defun majutsu-diff--update-hunk-refinement (&optional section allow-remove)
  "Apply or remove word-level refinement overlays.
When SECTION is nil, walk all hunk sections."
  (if section
      (unless (oref section hidden)
        (pcase (list majutsu-diff-refine-hunk
                     (oref section refined)
                     (eq section (magit-current-section)))
          ((or `(all nil ,_) '(t nil t))
           (oset section refined t)
           (if (eq majutsu-diff-backend 'color-words)
               (majutsu-diff--color-words-refine-hunk section)
             (save-excursion
               (goto-char (oref section start))
               ;; `diff-refine-hunk' cannot handle combined hunks.
               (unless (looking-at "@@@")
                 (let ((len (- (oref section end) (oref section start))))
                   (if (and majutsu-diff-refine-max-chars
                            (> len majutsu-diff-refine-max-chars))
                       (progn
                         (oset section refined nil)
                         (remove-overlays (oref section start)
                                          (oref section end)
                                          'diff-mode 'fine))
                     (let ((smerge-refine-ignore-whitespace
                            majutsu-diff-refine-ignore-whitespace)
                           (write-region-inhibit-fsync t))
                       (diff-refine-hunk))))))))
          ((and (guard allow-remove)
                (or `(nil t ,_) '(t t nil)))
           (oset section refined nil)
           (remove-overlays (oref section start)
                            (oref section end)
                            'diff-mode 'fine))))
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

(defun majutsu-diff--range-value (range prefix)
  "Return the value in RANGE for argument starting with PREFIX."
  (when range
    (when-let* ((arg (seq-find (lambda (item) (string-prefix-p prefix item)) range)))
      (substring arg (length prefix)))))

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
         (from (majutsu-diff--range-value range "--from="))
         (to (majutsu-diff--range-value range "--to="))
         (revisions (majutsu-diff--range-value range "--revisions=")))
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
         (to (majutsu-diff--range-value range "--to="))
         (revisions (majutsu-diff--range-value range "--revisions=")))
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

;;;###autoload
(defun majutsu-diff-resolve-conflict ()
  "Visit the workspace file and jump to its conflict markers.

Enable `majutsu-conflict-mode' for JJ markers or `smerge-mode' for
Git-style markers."
  (interactive)
  (let ((file (majutsu-file-at-point)))
    (unless file
      (user-error "No file at point"))
    (majutsu-diff-visit-file t)
    (majutsu-conflict-ensure-mode)
    (cond
     (majutsu-conflict-mode
      (majutsu-conflict-goto-nearest)
      (when diff-refine
        (ignore-errors (majutsu-conflict-refine))))
     (smerge-mode
      (condition-case nil
          (smerge-match-conflict)
        (error
         (smerge-next)))))
    (message "Use C-c ^ commands to resolve conflicts.")))

(defvar-keymap majutsu-file-section-map
  :doc "Keymap for `jj-file' sections."
  :parent majutsu-diff-section-map
  "v" #'majutsu-find-file-at-point)

(defvar-keymap majutsu-hunk-section-conflict-map
  :doc "Keymap bound to `smerge-command-prefix' in `majutsu-hunk-section-map'."
  "RET" #'majutsu-diff-resolve-conflict)

(defvar-keymap majutsu-hunk-section-map
  :doc "Keymap for `jj-hunk' sections."
  :parent majutsu-diff-section-map)

(let ((key (key-description smerge-command-prefix)))
  (when (key-valid-p key)
    (keymap-set majutsu-hunk-section-map key majutsu-hunk-section-conflict-map)))

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

(defvar-keymap majutsu-diff-mode-map
  :doc "Keymap for `majutsu-diff-mode'."
  :parent majutsu-mode-map
  "t" #'majutsu-diff-toggle-refine-hunk
  "+" #'majutsu-diff-more-context
  "-" #'majutsu-diff-less-context
  "0" #'majutsu-diff-default-context
  "j" #'majutsu-jump-to-diffstat-or-diff)

(define-derived-mode majutsu-diff-mode majutsu-mode "Majutsu Diff"
  "Major mode for viewing jj diffs."
  :group 'majutsu
  (setq-local line-number-mode nil)
  ;; Use diff-mode's keywords as a fallback, but primarily rely on
  ;; `font-lock-face' properties applied during the washing process.
  ;; We set this to enable JIT Lock, which renders our `font-lock-face' properties.
  (setq-local font-lock-defaults '(diff-font-lock-keywords t))
  (setq-local font-lock-multiline t))

(cl-defmethod majutsu-buffer-value (&context (major-mode majutsu-diff-mode))
  (list majutsu-buffer-diff-args
        majutsu-buffer-diff-range
        majutsu-buffer-diff-filesets))

(defun majutsu-diff-refresh-buffer ()
  "Refresh the current diff buffer."
  (interactive)
  (when majutsu-buffer-diff-args
    (let* ((backend (majutsu-diff--sync-backend))
           (majutsu-jj-global-arguments
            (cons (if (eq backend 'color-words) "--color=debug" "--color=never")
                  (seq-remove (lambda (arg) (string-prefix-p "--color" arg))
                              majutsu-jj-global-arguments)))
           (majutsu-process-apply-ansi-colors
            (majutsu-diff--backend-uses-ansi-p backend)))
      (if (eq backend 'color-words)
          (progn
            ;; No diff-mode keywords — ANSI faces via `font-lock-face'
            ;; provide all coloring.  Font-lock must stay enabled so the
            ;; display engine honours `font-lock-face' text properties.
            (setq-local font-lock-defaults '(nil t))
            (font-lock-mode 1))
        (setq-local font-lock-defaults '(diff-font-lock-keywords t))
        (font-lock-mode 1))
      (unless (eq backend 'color-words)
        (majutsu-diff--set-left-margin 0))
      (magit-insert-section (diffbuf)
        (magit-run-section-hook 'majutsu-diff-sections-hook))
      (when (eq majutsu-diff-refine-hunk 'all)
        (majutsu-diff--update-hunk-refinement)))))

;;;###autoload
(defun majutsu-diff-dwim (&optional args range filesets)
  "Show changes for the thing at point."
  (interactive (majutsu-diff-arguments))
  (let* ((rev (pcase (majutsu-diff--dwim)
                (`(commit . ,rev) rev)
                (_ "@")))
         (range (or range
                    (list (concat "--revisions=" (substring-no-properties rev))))))
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
  (if-let* ((rev (magit-section-value-if 'jj-commit)))
      (cons 'commit rev)
    nil))

(defun majutsu-diff-setup-buffer (args range filesets &optional locked)
  "Display a diff buffer configured by ARGS, RANGE and FILESETS."
  (let ((buffer (majutsu-setup-buffer #'majutsu-diff-mode locked
                  (majutsu-buffer-diff-args args)
                  (majutsu-buffer-diff-range range)
                  (majutsu-buffer-diff-filesets filesets))))
    (with-current-buffer buffer
      (majutsu-diff--sync-backend majutsu-buffer-diff-args))
    buffer))

;;; Commands
;;;; Prefix Commands

;;;###autoload
(transient-define-prefix majutsu-diff ()
  "Internal transient for jj diff."
  :man-page "jj-diff"
  :class 'majutsu-diff-prefix
  :incompatible '(("--revisions=" "--from=")
                  ("--revisions=" "--to=")
                  ("--stat" "--summary")
                  ("--git" "--color-words"))
  :transient-non-suffix t
  [:description "JJ Diff"
   :class transient-columns
   ["Selection"
    (majutsu-diff:-r)
    (majutsu-diff:--from)
    (majutsu-diff:--to)
    (majutsu-diff:revisions)
    (majutsu-diff:from)
    (majutsu-diff:to)
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
    ("s" "Save as default" majutsu-diff-save-arguments :transient t)
    ("g" "Refresh" majutsu-refresh :transient t)
    ("q" "Quit" transient-quit-one)]]
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
  :key "-s"
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
  :key "-r"
  :argument "--revisions="
  :multi-value 'repeat
  :prompt "Revisions: ")

(transient-define-argument majutsu-diff:revisions ()
  :description "Revisions (toggle at point)"
  :class 'majutsu-selection-toggle-option
  :key "r"
  :argument "--revisions="
  :multi-value 'repeat)

(transient-define-argument majutsu-diff:--from ()
  :description "From"
  :class 'majutsu-diff-range-option
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :key "-f"
  :argument "--from="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-diff:--to ()
  :description "To"
  :class 'majutsu-diff-range-option
  :selection-label "[TO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :key "-t"
  :argument "--to="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-diff:from ()
  :description "From (toggle at point)"
  :class 'majutsu-selection-toggle-option
  :key "f"
  :argument "--from=")

(transient-define-argument majutsu-diff:to ()
  :description "To (toggle at point)"
  :class 'majutsu-selection-toggle-option
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :key "t"
  :argument "--to=")

(transient-define-argument majutsu-diff:-b ()
  :description "Ignore changes in amount of whitespace"
  :class 'transient-switch
  :key "-b"
  :argument "--ignore-space-change")

(transient-define-argument majutsu-diff:-w ()
  :description "Ignore whitespace"
  :class 'transient-switch
  :key "-w"
  :argument "--ignore-all-space")

(defun majutsu-diff-save-arguments ()
  "Save current transient arguments as defaults."
  (interactive)
  (unless (object-of-class-p transient--prefix 'majutsu-diff-prefix)
    (user-error "Not in a Majutsu diff transient"))
  (transient-save-value transient--prefix)
  (message "Saved diff arguments as defaults"))

(defun majutsu-diff-refresh ()
  "Refresh diff buffer with current transient arguments."
  (interactive)
  (pcase-let* ((`(,args ,range ,_filesets)
                (transient-args 'majutsu-diff)))
    (cond
     ((eq major-mode 'majutsu-diff-mode)
      (majutsu-diff--set-buffer-args args)
      (setq-local majutsu-buffer-diff-range range)
      (majutsu-diff-refresh-buffer))
     ((and (memq majutsu-prefix-use-buffer-arguments '(always selected))
           (when-let* ((buf (majutsu--get-mode-buffer
                             'majutsu-diff-mode
                             (eq majutsu-prefix-use-buffer-arguments 'selected))))
             (with-current-buffer buf
               (majutsu-diff--set-buffer-args args)
               (setq-local majutsu-buffer-diff-range range)
               (majutsu-diff-refresh-buffer))
             t)))
     (t
      (user-error "No majutsu diff buffer found to refresh")))))

;;; _
(provide 'majutsu-diff)
;;; majutsu-diff.el ends here
