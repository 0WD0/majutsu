;;; majutsu-annotate.el --- Blame support for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Annotates each line in file-visiting buffer with information from
;; the revision which last modified the line, using `jj file annotate'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'majutsu-base)
(require 'majutsu-jj)
(require 'majutsu-process)

(declare-function majutsu-diff-revset "majutsu-diff" (revset &optional args range filesets))
(declare-function majutsu-find-file "majutsu-file" (revset path))

;;; Options

(defgroup majutsu-annotate nil
  "Annotate (blame) support for Majutsu."
  :group 'majutsu)

(defcustom majutsu-annotate-styles
  '((headings
     (heading-format . "%-20a %C %s\n"))
    (highlight
     (highlight-face . majutsu-annotate-highlight))
    (lines
     (show-lines . t)
     (show-message . t)))
  "List of styles used to visualize annotation information.

The style used in the current buffer can be cycled using \\`c'.
Annotation commands use the first style as the initial style.

Each entry has the form (IDENT (KEY . VALUE)...).  IDENT has
to be a symbol uniquely identifying the style.  The following
KEYs are recognized:

 `show-lines'
    Whether to prefix each chunk of lines with a thin line.
    This has no effect if `heading-format' is non-nil.
 `show-message'
    Whether to display a commit's summary line in the echo area
    when crossing chunks.
 `highlight-face'
    Face used to highlight the first line of each chunk.
    If this is nil, then those lines are not highlighted.
 `heading-format'
    String specifying the information to be shown above each
    chunk of lines.  It must end with a newline character.

The following %-specs can be used in `heading-format':

  %H    change-id (full)     using face `majutsu-annotate-hash'
  %h    change-id (short)    using face `majutsu-annotate-hash'
  %s    summary              using face `majutsu-annotate-summary'
  %a    author               using face `majutsu-annotate-name'
  %C    committer time       using face `majutsu-annotate-date'"
  :type '(alist :key-type symbol
          :value-type (alist :key-type symbol :value-type sexp))
  :group 'majutsu-annotate)

(defcustom majutsu-annotate-time-format "%F %H:%M"
  "Format for time strings in annotation headings."
  :type 'string
  :group 'majutsu-annotate)

(defcustom majutsu-annotate-read-only t
  "Whether to initially make the annotated buffer read-only."
  :type 'boolean
  :group 'majutsu-annotate)

(defcustom majutsu-annotate-disable-modes '(fci-mode yascroll-bar-mode)
  "List of modes not compatible with Majutsu-Annotate mode.
These modes are turned off when Majutsu-Annotate mode is turned on,
and then turned on again when turning off the latter."
  :type '(repeat (symbol :tag "Mode"))
  :group 'majutsu-annotate)

(defcustom majutsu-annotate-mode-lighter " Annotate"
  "Mode-line lighter for `majutsu-annotate-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :group 'majutsu-annotate)

(defcustom majutsu-annotate-goto-chunk-hook
  (list #'majutsu-annotate-maybe-show-message)
  "Hook run after point entered another chunk."
  :type 'hook
  :options (list #'majutsu-annotate-maybe-show-message)
  :group 'majutsu-annotate)

;;; Faces

(defface majutsu-annotate-highlight
  '((((class color) (background light))
     :extend t :background "grey80" :foreground "black")
    (((class color) (background dark))
     :extend t :background "grey25" :foreground "white"))
  "Face used for highlighting when annotating."
  :group 'majutsu-faces)

(defface majutsu-annotate-heading
  '((t :extend t :inherit majutsu-annotate-highlight
     :weight normal :slant normal))
  "Face used for annotation headings."
  :group 'majutsu-faces)

(defface majutsu-annotate-hash '((t nil))
  "Face used for change-ids when annotating."
  :group 'majutsu-faces)

(defface majutsu-annotate-name '((t nil))
  "Face used for author names when annotating."
  :group 'majutsu-faces)

(defface majutsu-annotate-date '((t nil))
  "Face used for dates when annotating."
  :group 'majutsu-faces)

(defface majutsu-annotate-summary '((t nil))
  "Face used for commit summaries when annotating."
  :group 'majutsu-faces)

;;; Variables

(defvar-local majutsu-annotate-buffer-read-only nil)
(defvar-local majutsu-annotate-disabled-modes nil)
(defvar-local majutsu-annotate-recursive-p nil)
(defvar-local majutsu-annotate--previous-chunk nil)
(defvar-local majutsu-annotate--style nil)
(defvar-local majutsu-annotate-cache nil
  "Hash table mapping change-id to revinfo alist.")
(defvar-local majutsu-annotate-separator nil)

;;; Chunk structure

(cl-defstruct majutsu-annotate-chunk
  "Structure representing an annotation chunk.
Slots:
  orig-rev     - The change-id that introduced the lines
  orig-line    - Original line number in the introducing commit
  final-line   - Line number in the current file
  num-lines    - Number of lines in this chunk
  prev-rev     - Parent change-id (for recursive blame)
  prev-file    - File path in parent revision
  orig-file    - Original file path"
  orig-rev orig-line final-line num-lines
  prev-rev prev-file orig-file)

;;; Keymaps

(defvar-keymap majutsu-annotate-mode-map
  :doc "Keymap for `majutsu-annotate-mode'.
Note that most annotation key bindings are defined
in `majutsu-annotate-read-only-mode-map' instead."
  "C-c C-q" #'majutsu-annotate-quit)

(defvar-keymap majutsu-annotate-read-only-mode-map
  :doc "Keymap for `majutsu-annotate-read-only-mode'."
  "RET" #'majutsu-annotate-show-commit
  "p"   #'majutsu-annotate-previous-chunk
  "P"   #'majutsu-annotate-previous-chunk-same-commit
  "n"   #'majutsu-annotate-next-chunk
  "N"   #'majutsu-annotate-next-chunk-same-commit
  "b"   #'majutsu-annotate-addition
  "c"   #'majutsu-annotate-cycle-style
  "q"   #'majutsu-annotate-quit
  "M-w" #'majutsu-annotate-copy-hash
  "SPC"   #'majutsu-annotate-show-or-scroll-up
  "S-SPC" #'majutsu-annotate-show-or-scroll-down
  "DEL"   #'majutsu-annotate-show-or-scroll-down)

;;; Modes

(define-minor-mode majutsu-annotate-mode
  "Display annotation information inline."
  :lighter majutsu-annotate-mode-lighter
  :interactive nil
  (cond (majutsu-annotate-mode
         (add-hook 'post-command-hook #'majutsu-annotate-goto-chunk-hook nil t)
         (add-hook 'read-only-mode-hook #'majutsu-annotate-toggle-read-only t t)
         (setq majutsu-annotate-buffer-read-only buffer-read-only)
         (when (or majutsu-annotate-read-only
                   (bound-and-true-p majutsu-buffer-blob-path))
           (read-only-mode 1))
         ;; Disable incompatible modes
         (dolist (mode majutsu-annotate-disable-modes)
           (when (and (boundp mode) (symbol-value mode))
             (funcall mode -1)
             (push mode majutsu-annotate-disabled-modes)))
         ;; Initialize style
         (setq majutsu-annotate-separator (majutsu-annotate--format-separator))
         (unless majutsu-annotate--style
           (setq majutsu-annotate--style (car majutsu-annotate-styles))))
        (t
         (remove-hook 'post-command-hook #'majutsu-annotate-goto-chunk-hook t)
         (remove-hook 'read-only-mode-hook #'majutsu-annotate-toggle-read-only t)
         (unless majutsu-annotate-buffer-read-only
           (read-only-mode -1))
         (majutsu-annotate-read-only-mode -1)
         ;; Re-enable disabled modes
         (dolist (mode majutsu-annotate-disabled-modes)
           (funcall mode 1))
         (kill-local-variable 'majutsu-annotate-disabled-modes)
         (kill-local-variable 'majutsu-annotate--style)
         (kill-local-variable 'majutsu-annotate-cache)
         (majutsu-annotate--remove-overlays))))

(define-minor-mode majutsu-annotate-read-only-mode
  "Provide keybindings for Majutsu-Annotate mode.

This minor-mode provides the key bindings for Majutsu-Annotate mode,
but only when Read-Only mode is also enabled because these key
bindings would otherwise conflict badly with regular bindings.

When both Majutsu-Annotate mode and Read-Only mode are enabled, then
this mode gets automatically enabled too and when one of these
modes is toggled, then this mode also gets toggled automatically.

\\{majutsu-annotate-read-only-mode-map}")

(defun majutsu-annotate-toggle-read-only ()
  "Toggle read-only mode for annotation."
  (majutsu-annotate-read-only-mode (if buffer-read-only 1 -1)))

(defun majutsu-annotate-goto-chunk-hook ()
  "Hook to run when point moves to a different chunk."
  (when-let* ((chunk (majutsu-annotate-chunk-at (point))))
    (unless (eq chunk majutsu-annotate--previous-chunk)
      (run-hooks 'majutsu-annotate-goto-chunk-hook)
      (setq majutsu-annotate--previous-chunk chunk))))

;;; Style helpers

(defsubst majutsu-annotate--style-get (key)
  "Get the value of KEY from current style."
  (cdr (assoc key (cdr majutsu-annotate--style))))

;;; Template for structured parsing

(defconst majutsu-annotate--template
  (concat
   "separate(\"\\t\","
   "  commit.change_id().short(12),"
   "  commit.parents().map(|p| p.change_id().short(12)).join(\",\"),"
   "  truncate_end(20, commit.author().name()),"
   "  commit_timestamp(commit).local().format(\"%Y-%m-%d %H:%M:%S\"),"
   "  line_number,"
   "  original_line_number,"
   "  first_line_in_hunk,"
   "  commit.description().first_line(),"
   ") ++ \"\\n\"")
  "Template for jj file annotate output.
Produces tab-separated fields:
  0: change-id (12 chars)
  1: parent change-ids (comma-separated)
  2: author (truncated to 20 chars)
  3: timestamp
  4: line number
  5: original line number
  6: first-line-in-hunk (true/false)
  7: description first line")

;;; Parsing

(defun majutsu-annotate--parse-output (output)
  "Parse OUTPUT from `jj file annotate' into chunks.
Returns a list of `majutsu-annotate-chunk' structures."
  (let ((lines (split-string output "\n" t))
        chunks current-chunk)
    (dolist (line lines)
      (let* ((fields (split-string line "\t"))
             (change-id (nth 0 fields))
             (parents (nth 1 fields))
             (author (nth 2 fields))
             (timestamp (nth 3 fields))
             (line-num (string-to-number (or (nth 4 fields) "0")))
             (orig-line-num (string-to-number (or (nth 5 fields) "0")))
             (first-in-hunk (equal (nth 6 fields) "true"))
             (summary (or (nth 7 fields) "")))
        (when (and change-id (> line-num 0))
          ;; Cache revinfo
          (unless (gethash change-id majutsu-annotate-cache)
            (puthash change-id
                     `(("summary" . ,summary)
                       ("author" . ,author)
                       ("committer-time" . ,timestamp))
                     majutsu-annotate-cache))
          (if (and current-chunk
                   (not first-in-hunk)
                   (equal change-id (majutsu-annotate-chunk-orig-rev current-chunk)))
              ;; Extend current chunk
              (cl-incf (majutsu-annotate-chunk-num-lines current-chunk))
            ;; Start new chunk
            (when current-chunk
              (push current-chunk chunks))
            (setq current-chunk
                  (make-majutsu-annotate-chunk
                   :orig-rev change-id
                   :orig-line orig-line-num
                   :final-line line-num
                   :num-lines 1
                   :prev-rev (and (not (string-empty-p parents))
                                  (car (split-string parents ",")))))))))
    (when current-chunk
      (push current-chunk chunks))
    (nreverse chunks)))

;;; Overlays

(defun majutsu-annotate--line-beginning-position (line)
  "Return position of beginning of LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun majutsu-annotate--make-overlays (chunks)
  "Create overlays for CHUNKS."
  (save-excursion
    (save-restriction
      (widen)
      (dolist (chunk chunks)
        (majutsu-annotate--make-chunk-overlays chunk)))))

(defun majutsu-annotate--make-chunk-overlays (chunk)
  "Create overlays for a single CHUNK."
  (let* ((line (majutsu-annotate-chunk-final-line chunk))
         (beg (majutsu-annotate--line-beginning-position line))
         (end (majutsu-annotate--line-beginning-position
               (+ line (majutsu-annotate-chunk-num-lines chunk)))))
    ;; Main chunk overlay with heading
    (majutsu-annotate--make-heading-overlay chunk beg end)
    ;; Highlight overlay for first line
    (majutsu-annotate--make-highlight-overlay chunk beg)))

(defun majutsu-annotate--make-heading-overlay (chunk beg end)
  "Create heading overlay for CHUNK from BEG to END."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'majutsu-annotate-chunk chunk)
    (overlay-put ov 'majutsu-annotate-heading t)
    (majutsu-annotate--update-heading-overlay ov)))

(defun majutsu-annotate--make-highlight-overlay (chunk beg)
  "Create highlight overlay for CHUNK starting at BEG."
  (let ((ov (make-overlay beg (save-excursion
                                (goto-char beg)
                                (1+ (line-end-position))))))
    (overlay-put ov 'majutsu-annotate-chunk chunk)
    (overlay-put ov 'majutsu-annotate-highlight t)
    (majutsu-annotate--update-highlight-overlay ov)))

(defun majutsu-annotate--update-overlays ()
  "Update all annotation overlays for current style."
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (cond ((overlay-get ov 'majutsu-annotate-heading)
             (majutsu-annotate--update-heading-overlay ov))
            ((overlay-get ov 'majutsu-annotate-highlight)
             (majutsu-annotate--update-highlight-overlay ov))))))

(defun majutsu-annotate--update-heading-overlay (ov)
  "Update heading overlay OV for current style."
  (overlay-put
   ov 'before-string
   (if-let* ((format (majutsu-annotate--style-get 'heading-format)))
       (majutsu-annotate--format-string ov format 'majutsu-annotate-heading)
     (and (majutsu-annotate--style-get 'show-lines)
          majutsu-annotate-separator))))

(defun majutsu-annotate--update-highlight-overlay (ov)
  "Update highlight overlay OV for current style."
  (overlay-put ov 'font-lock-face (majutsu-annotate--style-get 'highlight-face)))

(defun majutsu-annotate--format-string (ov format face)
  "Format annotation string for overlay OV using FORMAT and FACE."
  (let* ((chunk (overlay-get ov 'majutsu-annotate-chunk))
         (change-id (majutsu-annotate-chunk-orig-rev chunk))
         (revinfo (gethash change-id majutsu-annotate-cache)))
    (propertize
     (format-spec
      format
      `((?H . ,(propertize change-id 'font-lock-face 'majutsu-annotate-hash))
        (?h . ,(propertize (substring change-id 0 (min 8 (length change-id)))
                           'font-lock-face 'majutsu-annotate-hash))
        (?a . ,(propertize (or (cdr (assoc "author" revinfo)) "")
                           'font-lock-face 'majutsu-annotate-name))
        (?C . ,(propertize (or (cdr (assoc "committer-time" revinfo)) "")
                           'font-lock-face 'majutsu-annotate-date))
        (?s . ,(propertize (or (cdr (assoc "summary" revinfo)) "")
                           'font-lock-face 'majutsu-annotate-summary))))
     'font-lock-face face)))

(defun majutsu-annotate--format-separator ()
  "Format the separator line between chunks."
  (propertize (concat (propertize " " 'display '(space :height (2)))
                      (propertize "\n" 'line-height t))
              'font-lock-face
              `(:extend t
                :background
                ,(face-attribute 'majutsu-annotate-heading :background nil t))))

(defun majutsu-annotate--remove-overlays (&optional beg end)
  "Remove annotation overlays between BEG and END."
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (or beg (point-min))
                             (or end (point-max))))
      (when (overlay-get ov 'majutsu-annotate-chunk)
        (delete-overlay ov)))))

;;; Chunk navigation

(defun majutsu-annotate-chunk-at (pos)
  "Return the annotation chunk at POS."
  (seq-some (lambda (ov) (overlay-get ov 'majutsu-annotate-chunk))
            (overlays-at pos)))

(defun majutsu-annotate-current-chunk ()
  "Return the annotation chunk at point."
  (or (majutsu-annotate-chunk-at (point))
      (user-error "No chunk at point")))

(defun majutsu-annotate--overlay-at (&optional pos key)
  "Return overlay at POS with KEY property."
  (unless pos
    (setq pos (point)))
  (seq-find (lambda (ov) (overlay-get ov (or key 'majutsu-annotate-chunk)))
            (nconc (overlays-at pos)
                   (overlays-in pos pos))))

;;; Commands

(defun majutsu-annotate--file-exists-p (rev file)
  "Check if FILE exists in REV."
  (let ((output (ansi-color-apply (majutsu-jj-string "file" "list" "-r" rev file))))
    ;; jj outputs "Warning: No matching entries..." when file doesn't exist
    (not (string-prefix-p "Warning:" output))))

;;;###autoload
(defun majutsu-annotate-addition (&optional revision)
  "Annotate the current file showing when each line was added.
With prefix argument, prompt for REVISION.

If already annotating with the same type, and point is on a chunk
that has a parent revision, then recursively annotate the parent."
  (interactive
   (list (and current-prefix-arg
              (majutsu-read-revset "Annotate from revision"))))
  (majutsu-annotate--pre-assert)
  (if (and majutsu-annotate-mode
           (when-let* ((chunk (majutsu-annotate-chunk-at (point)))
                       (prev-rev (majutsu-annotate-chunk-prev-rev chunk)))
             ;; Recursive blame - visit parent and re-annotate
             (let ((style majutsu-annotate--style))
               (majutsu-annotate-visit-other-file)
               (setq-local majutsu-annotate--style style)
               (setq-local majutsu-annotate-recursive-p t)
               (redisplay)
               ;; Now run annotation in the new buffer
               (majutsu-annotate--run nil)
               t)))
      nil  ; Already handled recursive case
    (majutsu-annotate--run revision)))

(defun majutsu-annotate--pre-assert ()
  "Assert preconditions for annotation."
  (unless (majutsu-toplevel)
    (user-error "Not in a jj repository"))
  (unless (or buffer-file-name
              (bound-and-true-p majutsu-buffer-blob-path))
    (user-error "Buffer is not visiting a file")))

(defun majutsu-annotate--run (revision)
  "Run annotation for REVISION."
  (let* ((root (majutsu-toplevel))
         (file (or (and (bound-and-true-p majutsu-buffer-blob-path)
                        majutsu-buffer-blob-path)
                   (file-relative-name buffer-file-name root)))
         (rev (or revision
                  (and (bound-and-true-p majutsu-buffer-blob-revision)
                       majutsu-buffer-blob-revision)
                  "@"))
         (default-directory root))
    (message "Annotating...")
    (let ((output (ansi-color-apply (majutsu-jj-string
                                     "file" "annotate"
                                     "-r" rev
                                     "-T" majutsu-annotate--template
                                     file))))
      (when (string-empty-p output)
        (user-error "No annotation output"))
      (unless majutsu-annotate-mode
        (majutsu-annotate-mode 1))
      (setq majutsu-annotate-cache (make-hash-table :test #'equal))
      (majutsu-annotate--remove-overlays)
      (let ((chunks (majutsu-annotate--parse-output output)))
        (majutsu-annotate--make-overlays chunks)
        (when majutsu-annotate-read-only
          (majutsu-annotate-read-only-mode 1))
        (message "Annotating...done")))))

(defun majutsu-annotate-visit-other-file ()
  "Visit the blob related to the current chunk's parent revision."
  (interactive)
  (let* ((chunk (majutsu-annotate-current-chunk))
         (prev-rev (majutsu-annotate-chunk-prev-rev chunk))
         (orig-line (majutsu-annotate-chunk-orig-line chunk)))
    (unless prev-rev
      (user-error "Chunk has no further history"))
    ;; Get file path from parent - use current file for now
    (let ((file (or (bound-and-true-p majutsu-buffer-blob-path)
                    (and buffer-file-name
                         (file-relative-name buffer-file-name (majutsu-toplevel))))))
      ;; Check if file exists in parent revision
      (unless (majutsu-annotate--file-exists-p prev-rev file)
        (user-error "File does not exist in revision %s" prev-rev))
      (majutsu-find-file prev-rev file)
      ;; Jump to original line
      (goto-char (point-min))
      (forward-line (1- orig-line)))))

(defun majutsu-annotate-visit-file ()
  "Visit the blob related to the current chunk."
  (interactive)
  (let* ((chunk (majutsu-annotate-current-chunk))
         (orig-rev (majutsu-annotate-chunk-orig-rev chunk))
         (orig-line (majutsu-annotate-chunk-orig-line chunk)))
    (let ((file (or (bound-and-true-p majutsu-buffer-blob-path)
                    (and buffer-file-name
                         (file-relative-name buffer-file-name (majutsu-toplevel))))))
      (majutsu-find-file orig-rev file)
      ;; Jump to original line
      (goto-char (point-min))
      (forward-line (1- orig-line)))))

(defun majutsu-annotate-quit ()
  "Turn off Majutsu-Annotate mode.
If the buffer was created during a recursive blame,
then also kill the buffer."
  (interactive)
  (majutsu-annotate-mode -1)
  (when majutsu-annotate-recursive-p
    (kill-buffer)))

(defun majutsu-annotate-next-chunk ()
  "Move to the next chunk."
  (interactive)
  (if-let* ((next (next-single-char-property-change
                   (point) 'majutsu-annotate-chunk)))
      (goto-char next)
    (user-error "No more chunks")))

(defun majutsu-annotate-previous-chunk ()
  "Move to the previous chunk."
  (interactive)
  (if-let* ((prev (previous-single-char-property-change
                   (point) 'majutsu-annotate-chunk)))
      (goto-char prev)
    (user-error "No more chunks")))

(defun majutsu-annotate-next-chunk-same-commit (&optional previous)
  "Move to the next chunk from the same commit.
If PREVIOUS is non-nil, move to the previous chunk instead."
  (interactive)
  (let* ((chunk (majutsu-annotate-current-chunk))
         (rev (majutsu-annotate-chunk-orig-rev chunk))
         (pos (point))
         found)
    (save-excursion
      (while (and (not found)
                  (not (= pos (if previous (point-min) (point-max)))))
        (setq pos (funcall
                   (if previous
                       #'previous-single-char-property-change
                     #'next-single-char-property-change)
                   pos 'majutsu-annotate-chunk))
        (when-let* ((ov (majutsu-annotate--overlay-at pos))
                    (other-chunk (overlay-get ov 'majutsu-annotate-chunk)))
          (when (equal (majutsu-annotate-chunk-orig-rev other-chunk) rev)
            (setq found (overlay-start ov))))))
    (if found
        (goto-char found)
      (user-error "No more chunks from same commit"))))

(defun majutsu-annotate-previous-chunk-same-commit ()
  "Move to the previous chunk from the same commit."
  (interactive)
  (majutsu-annotate-next-chunk-same-commit t))

(defun majutsu-annotate-cycle-style ()
  "Change how annotation information is visualized.
Cycle through the elements of option `majutsu-annotate-styles'."
  (interactive)
  (setq majutsu-annotate--style
        (or (cadr (cl-member (car majutsu-annotate--style)
                             majutsu-annotate-styles :key #'car))
            (car majutsu-annotate-styles)))
  (majutsu-annotate--update-overlays)
  (message "Switched to %s style" (car majutsu-annotate--style)))

(defun majutsu-annotate-show-commit ()
  "Show the commit for the current chunk."
  (interactive)
  (let* ((chunk (majutsu-annotate-current-chunk))
         (change-id (majutsu-annotate-chunk-orig-rev chunk)))
    (majutsu-diff-revset change-id)))

(defun majutsu-annotate-copy-hash ()
  "Copy the change-id of the current chunk to the kill ring.

When the region is active, then save the region's content
instead of the hash, like `kill-ring-save' would."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((chunk (majutsu-annotate-current-chunk))
           (change-id (majutsu-annotate-chunk-orig-rev chunk)))
      (kill-new (message "%s" change-id)))))

(defun majutsu-annotate-show-or-scroll-up ()
  "Show the commit at point or scroll up if already showing."
  (interactive)
  (let* ((chunk (majutsu-annotate-current-chunk))
         (change-id (majutsu-annotate-chunk-orig-rev chunk)))
    (if-let* ((win (get-buffer-window
                    (format "*majutsu-diff: %s*" change-id))))
        (with-selected-window win
          (scroll-up))
      (majutsu-annotate-show-commit))))

(defun majutsu-annotate-show-or-scroll-down ()
  "Show the commit at point or scroll down if already showing."
  (interactive)
  (let* ((chunk (majutsu-annotate-current-chunk))
         (change-id (majutsu-annotate-chunk-orig-rev chunk)))
    (if-let* ((win (get-buffer-window
                    (format "*majutsu-diff: %s*" change-id))))
        (with-selected-window win
          (scroll-down))
      (majutsu-annotate-show-commit))))

(defun majutsu-annotate-maybe-show-message ()
  "Show summary of current chunk in echo area."
  (when (majutsu-annotate--style-get 'show-message)
    (when-let* ((chunk (majutsu-annotate-chunk-at (point)))
                (change-id (majutsu-annotate-chunk-orig-rev chunk))
                (revinfo (gethash change-id majutsu-annotate-cache))
                (summary (cdr (assoc "summary" revinfo))))
      (message "%s" summary))))

;;; _
(provide 'majutsu-annotate)
;;; majutsu-annotate.el ends here
