;;; majutsu-diff.el --- Diff viewing and editing for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
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
(require 'majutsu-log)
(require 'magit-diff)      ; for faces/font-lock keywords
(require 'diff-mode)
(require 'smerge-mode)

;;; Options

(defcustom majutsu-diff-refine-hunk t
  "Whether to show word-granularity differences inside hunks.

`nil'  Never show refinement.
`all'  Refine all hunks immediately.
`t'    Refine the current hunk when it becomes selected; keep the
       overlays until the buffer is refreshed."
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

(defface majutsu-diffstat-binary
  '((t :inherit font-lock-constant-face :foreground "#81c8be"))
  "Face for the (binary) label in diffstat entries."
  :group 'majutsu)

(defvar majutsu-diff--tab-width-cache nil
  "Alist mapping file names to cached tab widths.")

(defvar-local majutsu-diff--last-refined-section nil)
(defvar-local majutsu-diff--paint-whitespace-enabled t)
(defvar-local majutsu-diff--inserted-bytes 0)
(defvar-local majutsu-diff--filesets nil
  "Filesets filter for current diff buffer.")

(defcustom majutsu-diff-use-buffer-arguments 'selected
  "Whether to use buffer arguments in diff transient.
`always'   - Always use buffer args if available
`selected' - Use args from selected buffer
`current'  - Use args only from current buffer
`never'    - Never use buffer args"
  :group 'majutsu
  :type '(choice (const :tag "Always" always)
          (const :tag "Selected" selected)
          (const :tag "Current" current)
          (const :tag "Never" never)))
(defcustom majutsu-diff-whitespace-max-chars 12000
  "Skip whitespace painting for hunks larger than this many chars.
Set to nil to always paint whitespace inside hunks."
  :group 'majutsu
  :type '(choice (const :tag "No limit" nil)
          (integer :tag "Max characters")))
(defcustom majutsu-diff-whitespace-max-bytes 800000
  "Disable whitespace painting when a diff exceeds this many bytes.
Set to nil to always allow painting."
  :group 'majutsu
  :type '(choice (const :tag "No limit" nil)
          (integer :tag "Max bytes")))

(defun majutsu-diff--delete-line ()
  "Delete current line, including trailing newline if present."
  (delete-region (line-beginning-position)
                 (min (point-max) (1+ (line-end-position)))))

(defun majutsu--get-mode-buffer (mode)
  "Get a buffer in MODE, preferring visible ones."
  (or (seq-find (lambda (buf)
                  (and (eq (buffer-local-value 'major-mode buf) mode)
                       (get-buffer-window buf)))
                (buffer-list))
      (seq-find (lambda (buf)
                  (eq (buffer-local-value 'major-mode buf) mode))
                (buffer-list))))

(defun majutsu-diff--get-value (&optional use-buffer-args)
  "Get diff arguments based on USE-BUFFER-ARGS strategy.
Returns (args filesets) pair."
  (setq use-buffer-args (or use-buffer-args majutsu-diff-use-buffer-arguments))
  (cond
   ;; Current buffer has args
   ((and (memq use-buffer-args '(always current))
         (eq major-mode 'majutsu-diff-mode))
    (list majutsu-diff--last-args majutsu-diff--filesets))

   ;; Selected diff buffer has args
   ((and (memq use-buffer-args '(always selected))
         (when-let ((buf (majutsu--get-mode-buffer 'majutsu-diff-mode)))
           (list (buffer-local-value 'majutsu-diff--last-args buf)
                 (buffer-local-value 'majutsu-diff--filesets buf)))))

   ;; Saved transient values
   ((when-let ((saved (alist-get 'majutsu-diff transient-values)))
      (list saved nil)))

   ;; Mode default
   ((list (or (get 'majutsu-diff-mode 'majutsu-diff-default-arguments)
              '("--context" "3"))
          nil))))

(defun majutsu-diff--set-value (args &optional save filesets)
  "Set diff arguments and optionally SAVE them.
ARGS are the diff options, FILESETS are path filters."
  (when save
    (setf (alist-get 'majutsu-diff transient-values) args)
    (transient-save-values))
  (when (eq major-mode 'majutsu-diff-mode)
    (setq-local majutsu-diff--last-args args)
    (setq-local majutsu-diff--filesets filesets)
    (majutsu-diff-refresh)))

(defun majutsu-diff-arguments ()
  "Return current diff arguments."
  (if (eq transient-current-command 'majutsu-diff-transient--internal)
      (transient-args 'majutsu-diff-transient--internal)
    (car (majutsu-diff--get-value))))

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
        (push (match-string 2) files))
      (nreverse files))))

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
          (let* ((file (match-string 1))
                 (sep  (match-string 2))
                 (cnt  (match-string 3))
                 (add  (match-string 4))
                 (del  (match-string 5)))
            (majutsu-diff--delete-line)
            (when (string-match " +$" file)
              (setq sep (concat (match-string 0 file) sep))
              (setq file (substring file 0 (match-beginning 0))))
            (setq file (string-trim-right file))
            (magit-insert-section (jj-file (pop files))
              (insert (magit-format-file 'stat file 'magit-filename))
              (insert sep)
              (cond
               ((string-match "\\`(binary)\\(?: +\\([+-][0-9]+\\) bytes\\)?\\'" cnt)
                (insert (propertize "(binary)" 'font-lock-face
                                    'majutsu-diffstat-binary))
                (when-let ((delta (match-string 1 cnt)))
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

(transient-define-argument majutsu-diff:--stat ()
  :description "Show stats"
  :class 'transient-switch
  :key "-s"
  :argument "--stat")

(transient-define-argument majutsu-diff:--summary ()
  :description "Show summary"
  :class 'transient-switch
  :key "-S"
  :argument "--summary")

(transient-define-argument majutsu-diff:--context ()
  :description "Context lines"
  :class 'transient-option
  :key "-c"
  :argument "--context "
  :reader #'transient-read-number-N0)

;;; Diff Parsing & Display

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
  (when (and majutsu-diff--paint-whitespace-enabled
             majutsu-diff-paint-whitespace
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

(defun majutsu--insert-diff (section &optional args keep-error)
  "Run `jj diff' with ARGS, insert into SECTION, and wash.
When KEEP-ERROR is non-nil, preserve existing content on error."
  (let* ((buf (current-buffer))
         (diff-args (majutsu--ensure-flag (copy-sequence args) "--git")))
    (majutsu-run-jj-async
     (cons "diff" diff-args)
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t)
                 (magit-insert-section--parent section))
             (save-excursion
               (goto-char (oref section content))
               (delete-region (point) (oref section end))
               (if (and output (not (string-empty-p output)))
                   (majutsu--insert-diff-hunks output diff-args)
                 (insert (propertize "No changes." 'face 'shadow)))
               (insert "\n")
               (set-marker (oref section end) (point)))))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t)
                 (magit-insert-section--parent section))
             (save-excursion
               (goto-char (oref section content))
               (unless keep-error
                 (delete-region (point) (oref section end))
                 (insert (format "Error loading diffs: %s\n" err)))
               (set-marker (oref section end) (point))))))))))

(defun majutsu-insert-diff (&optional args heading)
  "Insert a diff section and populate it asynchronously.
ARGS are passed to `jj diff' (\"--git\" is ensured)."
  (let ((section (magit-insert-section (diffbuf)
                   (magit-insert-heading (or heading "Working Copy Changes"))
                   (insert "Loading diffs...\n"))))
    (majutsu--insert-diff section args)
    section))

(defun majutsu-diff-wash-diffs (args)
  "Parse a jj diff already inserted into the current buffer.
Assumes point is at the start of the diff output."
  (goto-char (point-min))
  (when (member "--stat" args)
    (majutsu-diff-wash-diffstat))
  (goto-char (point-min))
  (when (re-search-forward "^diff --git " nil t)
    (goto-char (match-beginning 0)))
  (cond
   ((looking-at "^diff --git ")
    (while (and (not (eobp))
                (looking-at "^diff --git "))
      (majutsu-diff-wash-file))
    (unless (bolp) (insert "\n")))
   ((save-excursion
      (goto-char (point-min))
      (looking-at-p (rx (* (any " \t\n")) eos)))
    (insert (propertize "(No diff)" 'face 'shadow)))))

(defun majutsu-diff-wash-file ()
  "Parse a single file section at point and wrap it in Magit sections."
  (when (looking-at "^diff --git a/\\(.*\\) b/\\(.*\\)$")
    (let* ((file-a (match-string 1))
           (file-b (match-string 2))
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
          (propertize (majutsu--diff-file-heading file (nreverse headers))
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
    (magit-insert-section
        (jj-hunk file nil
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
        (when majutsu-diff--paint-whitespace-enabled
          (majutsu-diff--paint-hunk-whitespace body-start (point) file))))))

(defun majutsu--diff-line-matching-p (regexp lines)
  "Return non-nil if any string in LINES matches REGEXP."
  (seq-some (lambda (line) (and line (string-match-p regexp line))) lines))

(defun majutsu--diff-file-status (lines)
  "Infer the jj diff status for a file based on LINES."
  (cond
   ((majutsu--diff-line-matching-p "^new file" lines) "new file")
   ((majutsu--diff-line-matching-p "^deleted file" lines) "deleted")
   ((majutsu--diff-line-matching-p "^rename \\(from\\|to\\)" lines) "renamed")
   ((majutsu--diff-line-matching-p "^copy \\(from\\|to\\)" lines) "copied")
   (t "modified")))

(defun majutsu--diff-file-heading (file lines)
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
                     (diff-refine-hunk)))))))
          ((and (guard allow-remove)
                (or `(nil t ,_) '(t t nil)))
           (oset section refined nil)
           (remove-overlays (oref section start)
                            (oref section end)
                            'diff-mode 'fine))))
    (cl-labels ((walk (node)
                  (if (magit-section-match 'majutsu-hunk-section node)
                      (majutsu-diff--update-hunk-refinement node t)
                    (dolist (child (oref node children))
                      (walk child)))))
      (walk magit-root-section))))

(cl-defmethod magit-section--refine ((section majutsu-hunk-section))
  (when (eq majutsu-diff-refine-hunk t)
    ;; Clear previous refined hunk when moving focus.
    (when (and majutsu-diff--last-refined-section
               (not (eq majutsu-diff--last-refined-section section)))
      (majutsu-diff--update-hunk-refinement majutsu-diff--last-refined-section t))
    (majutsu-diff--update-hunk-refinement section)
    (setq majutsu-diff--last-refined-section section)))

(cl-defmethod magit-section-paint ((section majutsu-hunk-section) highlight)
  "Paint a hunk so focus highlighting behaves like Magit.

This mirrors `magit-section-paint' for `magit-hunk-section' but
works with the simplified jj diff we render here."
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
        (forward-line))))
  (oset section painted (if highlight 'highlight 'plain)))

(defun majutsu-diff--maybe-clear-refinement ()
  "When leaving a hunk in `t' mode, drop old refinement overlays."
  (when (and (eq majutsu-diff-refine-hunk t)
             majutsu-diff--last-refined-section
             (not (magit-section-match 'majutsu-hunk-section
                                       (magit-current-section))))
    (majutsu-diff--update-hunk-refinement majutsu-diff--last-refined-section t)
    (setq majutsu-diff--last-refined-section nil)))

;;; Navigation

(defun majutsu-diff--file-at-point ()
  "Return the file for the current diff/diffstat section, if any."
  (when-let* ((section (magit-current-section)))
    (cond
     ((magit-section-match 'jj-hunk section)
      (or (magit-section-parent-value section)
          (oref section value)))
     ((magit-section-match 'jj-file section)
      (oref section value)))))

(defun majutsu-goto-diff-line ()
  "Jump to the line in the file corresponding to the diff line at point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (_ (magit-section-match 'jj-hunk section))
              (file (magit-section-parent-value section))
              (repo-root (majutsu--root)))
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
               (full-file-path (expand-file-name file repo-root)))
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
  (when-let* ((file (majutsu-diff--file-at-point))
              (repo-root (majutsu--root)))
    (let ((full-file-path (expand-file-name file repo-root)))
      (find-file full-file-path))))

;;;###autoload
(defun majutsu-diff-visit-file ()
  "Visit the file at point.

When point is on a hunk section, jump to the corresponding line in the
file."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ((and section (magit-section-match 'jj-hunk section))
      (majutsu-goto-diff-line))
     ((majutsu-diff--file-at-point)
      (majutsu-visit-file))
     (t
      (user-error "No file at point")))))

;;; Section Keymaps

(defvar-keymap majutsu-diff-section-map
  :doc "Keymap for diff sections."
  "<remap> <majutsu-visit-thing>" #'majutsu-diff-visit-file)

(defvar-keymap majutsu-file-section-map
  :doc "Keymap for `jj-file' sections."
  :parent majutsu-diff-section-map)

(defvar-keymap majutsu-hunk-section-map
  :doc "Keymap for `jj-hunk' sections."
  :parent majutsu-diff-section-map)

;;; Diff Edit

;;;###autoload
(defun majutsu-diffedit-emacs ()
  "Emacs-based diffedit using built-in ediff."
  (interactive)
  (let* ((file (majutsu-diff--file-at-point)))
    (if file
        (majutsu-diffedit-with-ediff file)
      (majutsu-diffedit-all))))

(defun majutsu-diffedit-with-ediff (file)
  "Open ediff session for a specific file against parent."
  (let* ((repo-root (majutsu--root))
         (full-file-path (expand-file-name file repo-root))
         (file-ext (file-name-extension file))
         (parent-temp-file (make-temp-file (format "majutsu-parent-%s" (file-name-nondirectory file))
                                           nil (when file-ext (concat "." file-ext))))
         (parent-content (let ((default-directory repo-root))
                           (majutsu-run-jj "file" "show" "-r" "@-" file))))

    ;; Write parent content to temp file
    (with-temp-file parent-temp-file
      (insert parent-content)
      ;; Enable proper major mode for syntax highlighting
      (when file-ext
        (let ((mode (assoc-default (concat "." file-ext) auto-mode-alist 'string-match)))
          (when mode
            (funcall mode)))))

    ;; Set up cleanup
    (add-hook 'ediff-quit-hook
              (lambda ()
                (when (file-exists-p parent-temp-file)
                  (delete-file parent-temp-file))
                (majutsu-log-refresh))
              nil t)

    ;; Start ediff session
    (ediff-files parent-temp-file full-file-path)
    (message "Ediff: Left=Parent (@-), Right=Current (@). Edit right side, then 'q' to quit and save.")))

(defvar-local majutsu-smerge-file nil
  "File being merged in smerge session.")

(defvar-local majutsu-smerge-repo-root nil
  "Repository root for smerge session.")

;;;###autoload
(defun majutsu-diffedit-smerge ()
  "Emacs-based diffedit using smerge-mode (merge conflict style)."
  (interactive)
  (let* ((file (majutsu-diff--file-at-point)))
    (if file
        (majutsu-diffedit-with-smerge file)
      (majutsu-diffedit-all))))

(defun majutsu-diffedit-with-smerge (file)
  "Open smerge-mode session for a specific file."
  (let* ((repo-root (majutsu--root))
         (full-file-path (expand-file-name file repo-root))
         (parent-content (let ((default-directory repo-root))
                           (majutsu-run-jj "file" "show" "-r" "@-" file)))
         (current-content (if (file-exists-p full-file-path)
                              (with-temp-buffer
                                (insert-file-contents full-file-path)
                                (buffer-string))
                            ""))
         (merge-buffer (get-buffer-create (format "*majutsu-smerge-%s*" (file-name-nondirectory file)))))

    (with-current-buffer merge-buffer
      (erase-buffer)

      ;; Create merge-conflict format
      (insert "<<<<<<< Parent (@-)\n")
      (insert parent-content)
      (unless (string-suffix-p "\n" parent-content)
        (insert "\n"))
      (insert "=======\n")
      (insert current-content)
      (unless (string-suffix-p "\n" current-content)
        (insert "\n"))
      (insert ">>>>>>> Current (@)\n")

      ;; Enable smerge-mode
      (smerge-mode 1)
      (setq-local majutsu-smerge-file file)
      (setq-local majutsu-smerge-repo-root repo-root)

      ;; Add save hook
      (add-hook 'after-save-hook 'majutsu-smerge-apply-changes nil t)

      (goto-char (point-min)))

    (switch-to-buffer-other-window merge-buffer)
    (message "SMerge mode: Use C-c ^ commands to navigate/resolve conflicts, then save to apply.")))

(defun majutsu-smerge-apply-changes ()
  "Apply smerge changes to the original file."
  (when (and (boundp 'majutsu-smerge-file) majutsu-smerge-file)
    (let* ((file majutsu-smerge-file)
           (repo-root majutsu-smerge-repo-root)
           (full-file-path (expand-file-name file repo-root))
           (content (buffer-string)))

      ;; Only apply if no conflict markers remain
      (unless (or (string-match "^<<<<<<<" content)
                  (string-match "^=======" content)
                  (string-match "^>>>>>>>" content))
        (with-temp-file full-file-path
          (insert content))
        (majutsu-log-refresh)
        (message "Changes applied to %s" file)))))

(defun majutsu-diffedit-all ()
  "Open diffedit interface for all changes."
  (let* ((changed-files (majutsu--get-changed-files))
         (choice (if (= (length changed-files) 1)
                     (car changed-files)
                   (completing-read "Edit file: " changed-files))))
    (when choice
      (majutsu-diffedit-with-ediff choice))))

(defun majutsu--get-changed-files ()
  "Get list of files with changes in working copy."
  (let ((diff-output (majutsu-run-jj "diff" "--name-only")))
    (split-string diff-output "\n" t)))

;;; Diff Commands

(defun majutsu-diff-clear-selections ()
  "Clear all diff selections."
  (interactive)
  (majutsu-selection-clear)
  (when (called-interactively-p 'interactive)
    (message "Cleared diff selections")))

(defun majutsu-diff-set-from ()
  "Set the commit at point as diff --from."
  (interactive)
  (majutsu-selection-select 'from))

(defun majutsu-diff-set-to ()
  "Set the commit at point as diff --to."
  (interactive)
  (majutsu-selection-select 'to))

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
         (val majutsu-diff--last-args)
         (arg (seq-find (##string-match "^--context " %) val))
         (num (if arg
                  (string-to-number (substring arg 10))
                def))
         (val (delq arg val))
         (num (funcall fn num))
         (arg (and num (not (= num def)) (format "--context %d" num)))
         (val (if arg (cons arg val) val)))
    (majutsu-diff--set-value val)))

(defun majutsu-diff-toggle-refine-hunk (&optional style)
  "Toggle word-level refinement within hunks.
With prefix STYLE, cycle between `all' and `t'."
  (interactive "P")
  (setq-local majutsu-diff-refine-hunk
              (if style
                  (if (eq majutsu-diff-refine-hunk 'all) t 'all)
                (not majutsu-diff-refine-hunk)))
  (pcase majutsu-diff-refine-hunk
    ('all
     (setq majutsu-diff--last-refined-section nil)
     (majutsu-diff--update-hunk-refinement))
    ('t
     (majutsu-diff--maybe-clear-refinement)
     (let ((cur (magit-current-section)))
       (when (magit-section-match 'majutsu-hunk-section cur)
         (majutsu-diff--update-hunk-refinement cur)
         (setq majutsu-diff--last-refined-section cur))))
    (_
     (majutsu-diff--update-hunk-refinement nil t)
     (setq majutsu-diff--last-refined-section nil))))

(defvar-keymap majutsu-diff-mode-map
  :doc "Keymap for `majutsu-diff-mode'."
  :parent majutsu-mode-map
  "t" #'majutsu-diff-toggle-refine-hunk
  "+" #'majutsu-diff-more-context
  "-" #'majutsu-diff-less-context
  "0" #'majutsu-diff-default-context
  "j" #'majutsu-jump-to-diffstat-or-diff)

(define-derived-mode majutsu-diff-mode majutsu-mode "JJ Diff"
  "Major mode for viewing jj diffs."
  :group 'majutsu
  (setq-local line-number-mode nil)
  ;; Use diff-mode's keywords as a fallback, but primarily rely on
  ;; `font-lock-face' properties applied during the washing process.
  ;; We set this to enable JIT Lock, which renders our `font-lock-face' properties.
  (setq-local font-lock-defaults '(diff-font-lock-keywords t))
  (setq-local font-lock-multiline t)
  (add-hook 'post-command-hook #'majutsu-diff--maybe-clear-refinement nil t))

(defvar-local majutsu-diff--last-args nil
  "Arguments used for the last jj diff command in this buffer.")

(defun majutsu-diff-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the current diff buffer."
  (interactive)
  (majutsu--assert-mode 'majutsu-diff-mode)
  (when majutsu-diff--last-args
    (let ((inhibit-read-only t)
          (repo-root (majutsu--root)))
      (erase-buffer)
      (setq-local majutsu--repo-root repo-root)
      (let* ((default-directory repo-root)
             (cmd-args majutsu-diff--last-args)
             ;; Avoid ANSI; let our painting run lazily.
             (majutsu-process-color-mode nil)
             (majutsu-process-apply-ansi-colors nil))
        (setq-local majutsu-diff--paint-whitespace-enabled
                    (or (not majutsu-diff-whitespace-max-bytes)
                        (< (buffer-size) majutsu-diff-whitespace-max-bytes)))
        (magit-insert-section (diffbuf)
          (magit-insert-section (diff-root)
            (magit-insert-heading
              (format "jj %s" (string-join majutsu-diff--last-args " ")))
            (insert "\n")
            (majutsu-diff--wash-with-state
                #'majutsu-diff-wash-diffs 'wash-anyway cmd-args))))
      (when (eq majutsu-diff-refine-hunk 'all)
        (majutsu-diff--update-hunk-refinement))
      (goto-char (point-min)))))

;;;###autoload
(defun majutsu-diff (&optional args)
  "Show diff with ARGS.
If called interactively, defaults to diffing the commit at point (if in
log view) or the working copy (if elsewhere)."
  (interactive
   (list (if-let* ((rev (magit-section-value-if 'jj-commit)))
             (list "-r" rev)
           nil)))
  (let* ((repo-root (majutsu--root))
         (buf (get-buffer-create "*majutsu-diff*"))
         (final-args (append '("diff" "--git") args)))
    (with-current-buffer buf
      (setq default-directory repo-root)
      (majutsu-diff-mode)
      (setq-local majutsu--repo-root repo-root)
      (setq-local majutsu-diff--last-args final-args)
      (setq-local revert-buffer-function #'majutsu-refresh-buffer)
      (majutsu-diff-refresh)
      (majutsu-display-buffer buf 'diff))))

(defun majutsu-diff-execute (&optional args)
  "Execute diff using transient selections or ARGS."
  (interactive (list (majutsu-diff-arguments)))
  (let* ((from (car (majutsu-selection-values 'from)))
         (to (car (majutsu-selection-values 'to)))
         (final-args (copy-sequence args)))
    ;; If we have selections, they override or supplement standard args
    (when from
      (setq final-args (append final-args (list "--from" from))))
    (when to
      (setq final-args (append final-args (list "--to" to))))

    ;; If no selections and no specific revision args, fallback to DWIM
    (when (and (not from) (not to)
               (not (member "-r" final-args))
               (not (member "--from" final-args)))
      ;; DWIM logic: if in log view, diff commit at point. Else working copy.
      (if-let* ((rev (magit-section-value-if 'jj-commit)))
          (setq final-args (append final-args (list "-r" rev)))
        (setq final-args (append final-args (list "-r" "@")))))

    (majutsu-diff final-args)
    (majutsu-selection-session-end)))

;;;###autoload
(defun majutsu-diff-transient ()
  "Transient for jj diff operations."
  (interactive)
  (majutsu-selection-session-begin
   '((:key from
      :label "[FROM]"
      :face (:background "dark orange" :foreground "black")
      :type single)
     (:key to
      :label "[TO]"
      :face (:background "dark cyan" :foreground "white")
      :type single)))
  (add-hook 'transient-exit-hook #'majutsu-selection-session-end nil t)
  (majutsu-diff-transient--internal))

;;; Diff Transient

(transient-define-prefix majutsu-diff-transient--internal ()
  "Internal transient for jj diff."
  :man-page "jj-diff"
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  :value (lambda () (car (majutsu-diff--get-value)))
  [:description
   (lambda ()
     (concat "JJ Diff"
             (when-let* ((from (car (majutsu-selection-values 'from))))
               (format " | From: %s" from))
             (when-let* ((to (car (majutsu-selection-values 'to))))
               (format " | To: %s" to))))
   :class transient-columns
   ["Selection"
    ("f" "Set 'from'" majutsu-diff-set-from
     :description (lambda ()
                    (if (> (majutsu-selection-count 'from) 0)
                        (format "Set 'from' (current: %s)"
                                (car (majutsu-selection-values 'from)))
                      "Set 'from'"))
     :transient t)
    ("t" "Set 'to'" majutsu-diff-set-to
     :description (lambda ()
                    (if (> (majutsu-selection-count 'to) 0)
                        (format "Set 'to' (current: %s)"
                                (car (majutsu-selection-values 'to)))
                      "Set 'to'"))
     :transient t)
    ("c" "Clear selections" majutsu-diff-clear-selections :transient t)]
   ["Options"
    (majutsu-diff:--stat)
    (majutsu-diff:--summary)
    (majutsu-diff:--context)]
   ["Actions"
    ("d" "Execute" majutsu-diff-execute)
    ("s" "Save as default" majutsu-diff-save-arguments :transient t)
    ("g" "Refresh" majutsu-diff-refresh-transient :transient t)
    ("q" "Quit" transient-quit-one)]])

(defun majutsu-diff-save-arguments ()
  "Save current transient arguments as defaults."
  (interactive)
  (majutsu-diff--set-value
   (transient-args 'majutsu-diff-transient--internal)
   'save)
  (message "Saved diff arguments as defaults"))

(defun majutsu-diff-refresh-transient ()
  "Refresh diff buffer with current transient arguments."
  (interactive)
  (when (eq major-mode 'majutsu-diff-mode)
    (majutsu-diff--set-value
     (transient-args 'majutsu-diff-transient--internal))))

(defun majutsu-diff--wash-with-state (washer keep-error &rest args)
  "Wrap `majutsu--wash' to also track diff buffer bookkeeping.
Sets `majutsu-diff--inserted-bytes' and whitespace painting flag
after the wash finishes.  KEEP-ERROR and ARGS are forwarded unchanged."
  (declare (indent 2))
  (let ((before-size (buffer-size)))
    (prog1
        (apply #'majutsu--wash washer keep-error args)
      (let ((bytes (- (buffer-size) before-size)))
        (setq-local majutsu-diff--inserted-bytes bytes)
        (setq-local majutsu-diff--paint-whitespace-enabled
                    (or (not majutsu-diff-whitespace-max-bytes)
                        (<= bytes majutsu-diff-whitespace-max-bytes)))))))

;;; _
(provide 'majutsu-diff)
;;; majutsu-diff.el ends here
