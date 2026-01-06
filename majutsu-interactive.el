;;; majutsu-interactive.el --- Interactive partial patching for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides Magit-style partial hunk staging for Jujutsu.
;; It enables region-based and hunk-based selection within diff buffers,
;; then applies those selections via jj split/squash/restore -i commands.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'magit-section)
(require 'transient)
(require 'majutsu-base)
(require 'majutsu-diff)
(require 'majutsu-process)

;;; Options

(defgroup majutsu-interactive nil
  "Interactive partial patching for Majutsu."
  :group 'majutsu)

(defcustom majutsu-interactive-applypatch-program "git"
  "Program used to apply patches.
Can be `git' (uses `git apply') or `patch'."
  :type '(choice (const :tag "git apply" "git")
                 (const :tag "patch" "patch")
                 (string :tag "Custom"))
  :group 'majutsu-interactive)

(defface majutsu-interactive-selected-hunk
  '((t :background "#3a5f3a"))
  "Face for selected hunks."
  :group 'majutsu-interactive)

(defface majutsu-interactive-selected-region
  '((t :background "#5f3a5f"))
  "Face for selected regions within hunks."
  :group 'majutsu-interactive)

;;; Selection Model

(defvar-local majutsu-interactive--selections nil
  "Hash table mapping hunk-id to selection spec.
Selection spec is either `:all' for whole hunk, or (BEG . END) for region.")

(defvar-local majutsu-interactive--overlays nil
  "List of overlays for selection visualization.")

(defun majutsu-interactive--hunk-id (section)
  "Return unique identifier for hunk SECTION."
  (oref section value))

(defun majutsu-interactive--file-for-hunk (section)
  "Return the file name for hunk SECTION."
  (let ((val (oref section value)))
    (if (consp val) (car val) val)))

(defun majutsu-interactive--ensure-selections ()
  "Ensure selections hash table exists."
  (unless majutsu-interactive--selections
    (setq majutsu-interactive--selections (make-hash-table :test 'equal))))

(defun majutsu-interactive--get-selection (hunk-id)
  "Get selection for HUNK-ID."
  (majutsu-interactive--ensure-selections)
  (gethash hunk-id majutsu-interactive--selections))

(defun majutsu-interactive--set-selection (hunk-id spec)
  "Set selection SPEC for HUNK-ID."
  (majutsu-interactive--ensure-selections)
  (if spec
      (puthash hunk-id spec majutsu-interactive--selections)
    (remhash hunk-id majutsu-interactive--selections)))

;;; Selection Functions

(defun majutsu-interactive-toggle-hunk ()
  "Toggle selection of the hunk at point."
  (interactive)
  (when-let* ((section (magit-current-section)))
    (when (cl-typep section 'majutsu-hunk-section)
      (let* ((hunk-id (majutsu-interactive--hunk-id section))
             (current (majutsu-interactive--get-selection hunk-id)))
        (majutsu-interactive--set-selection
         hunk-id (if current nil :all))
        (majutsu-interactive--render-overlays)
        (message "%s hunk" (if current "Deselected" "Selected"))))))

(defun majutsu-interactive-toggle-file ()
  "Toggle selection of all hunks in the file at point."
  (interactive)
  (when-let* ((section (magit-current-section)))
    (let ((file-section
           (cond
            ((cl-typep section 'majutsu-file-section) section)
            ((cl-typep section 'majutsu-hunk-section) (oref section parent)))))
      (when (and file-section (cl-typep file-section 'majutsu-file-section))
        (let* ((hunks (oref file-section children))
               (all-selected (cl-every
                              (lambda (h)
                                (majutsu-interactive--get-selection
                                 (majutsu-interactive--hunk-id h)))
                              hunks)))
          (dolist (hunk hunks)
            (when (cl-typep hunk 'majutsu-hunk-section)
              (majutsu-interactive--set-selection
               (majutsu-interactive--hunk-id hunk)
               (unless all-selected :all))))
          (majutsu-interactive--render-overlays)
          (message "%s all hunks in file"
                   (if all-selected "Deselected" "Selected")))))))

(defun majutsu-interactive-toggle-region ()
  "Toggle selection of the region within the current hunk."
  (interactive)
  (unless (use-region-p)
    (user-error "No region active"))
  (when-let* ((section (magit-current-section)))
    (when (cl-typep section 'majutsu-hunk-section)
      (let* ((hunk-id (majutsu-interactive--hunk-id section))
             (rbeg (region-beginning))
             (rend (region-end))
             (hunk-content (oref section content))
             (hunk-end (oref section end)))
        ;; Clamp region to hunk body
        (setq rbeg (max rbeg hunk-content))
        (setq rend (min rend hunk-end))
        (when (< rbeg rend)
          (let ((current (majutsu-interactive--get-selection hunk-id)))
            (if (and (consp current)
                     (= (car current) rbeg)
                     (= (cdr current) rend))
                (majutsu-interactive--set-selection hunk-id nil)
              (majutsu-interactive--set-selection hunk-id (cons rbeg rend))))
          (majutsu-interactive--render-overlays)
          (deactivate-mark)
          (message "Region selection updated"))))))

(defun majutsu-interactive-clear ()
  "Clear all selections."
  (interactive)
  (setq majutsu-interactive--selections nil)
  (majutsu-interactive--render-overlays)
  (message "Cleared all selections"))

;;; Overlay Rendering

(defun majutsu-interactive--clear-overlays ()
  "Remove all selection overlays."
  (mapc #'delete-overlay majutsu-interactive--overlays)
  (setq majutsu-interactive--overlays nil))

(defun majutsu-interactive--render-overlays ()
  "Render overlays for current selections."
  (majutsu-interactive--clear-overlays)
  (when majutsu-interactive--selections
    (maphash
     (lambda (hunk-id spec)
       (when-let* ((section (majutsu-interactive--find-hunk-section hunk-id)))
         (let* ((start (if (eq spec :all)
                           (oref section start)
                         (car spec)))
                (end (if (eq spec :all)
                         (oref section end)
                       (cdr spec)))
                (face (if (eq spec :all)
                          'majutsu-interactive-selected-hunk
                        'majutsu-interactive-selected-region))
                (ov (make-overlay start end)))
           (overlay-put ov 'face face)
           (overlay-put ov 'evaporate t)
           (push ov majutsu-interactive--overlays))))
     majutsu-interactive--selections)))

(defun majutsu-interactive--find-hunk-section (hunk-id)
  "Find hunk section with HUNK-ID in current buffer."
  (let ((result nil))
    (when magit-root-section
      (cl-labels ((walk (section)
                    (when (and (cl-typep section 'majutsu-hunk-section)
                               (equal (majutsu-interactive--hunk-id section) hunk-id))
                      (setq result section))
                    (dolist (child (oref section children))
                      (unless result (walk child)))))
        (walk magit-root-section)))
    result))

;;; Patch Generation

(defun majutsu-interactive--collect-selected-hunks ()
  "Collect all selected hunks with their selection specs.
Returns list of (FILE-SECTION HUNK-SECTION SPEC)."
  (let ((result nil))
    (when majutsu-interactive--selections
      (maphash
       (lambda (hunk-id spec)
         (when-let* ((hunk (majutsu-interactive--find-hunk-section hunk-id)))
           (push (list (oref hunk parent) hunk spec) result)))
       majutsu-interactive--selections))
    (nreverse result)))

(defun majutsu-interactive--hunk-header (section)
  "Extract hunk header line from SECTION."
  (buffer-substring-no-properties
   (oref section start)
   (1- (oref section content))))

(defun majutsu-interactive--hunk-body-lines (section)
  "Extract hunk body lines from SECTION as list of strings."
  (let ((lines nil)
        (start (oref section content))
        (end (oref section end)))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (push (buffer-substring-no-properties
               (line-beginning-position)
               (min (1+ (line-end-position)) end))
              lines)
        (forward-line 1)))
    (nreverse lines)))

(defun majutsu-interactive--filter-hunk-lines (section spec)
  "Filter hunk body lines in SECTION according to SPEC.
SPEC is either `:all' or (BEG . END) region bounds.
Returns filtered lines as a list of strings."
  (if (eq spec :all)
      (majutsu-interactive--hunk-body-lines section)
    ;; Region-based filtering
    (let ((lines nil)
          (start (oref section content))
          (end (oref section end))
          (rbeg (car spec))
          (rend (cdr spec)))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (let* ((bol (line-beginning-position))
                 (eol (line-end-position))
                 (line-text (buffer-substring-no-properties bol (min (1+ eol) end)))
                 (first-char (and (> (length line-text) 0) (aref line-text 0)))
                 (in-region (and (>= eol rbeg) (<= bol rend))))
            (cond
             ;; Context lines: always keep
             ((or (eq first-char ?\s) (eq first-char ?@))
              (push line-text lines))
             ;; In region: keep as-is
             (in-region
              (push line-text lines))))
           ;; Outside region +/- lines: implicitly dropped
          (forward-line 1)))
      (nreverse lines))))

(defun majutsu-interactive--build-file-patch (file-section hunks-with-specs)
  "Build patch string for FILE-SECTION with HUNKS-WITH-SPECS.
HUNKS-WITH-SPECS is list of (HUNK-SECTION SPEC)."
  (let ((header (oref file-section header))
        (hunk-patches nil))
    (dolist (hs hunks-with-specs)
      (let* ((hunk (car hs))
             (spec (cadr hs))
             (hunk-header (majutsu-interactive--hunk-header hunk))
             (filtered-lines (majutsu-interactive--filter-hunk-lines hunk spec)))
        (when filtered-lines
          (push (concat hunk-header "\n"
                        (mapconcat #'identity filtered-lines ""))
                hunk-patches))))
    (when hunk-patches
      (concat header
              (mapconcat #'identity (nreverse hunk-patches) "")))))

(defun majutsu-interactive--build-patch ()
  "Build complete patch from current selections.
Returns patch string or nil if no selections."
  (let* ((selected (majutsu-interactive--collect-selected-hunks))
         (by-file (make-hash-table :test 'eq)))
    (unless selected
      (user-error "No hunks selected"))
    ;; Group by file section
    (dolist (item selected)
      (let ((file (car item))
            (hunk (cadr item))
            (spec (caddr item)))
        (push (list hunk spec) (gethash file by-file))))
    ;; Build patches per file
    (let ((patches nil))
      (maphash
       (lambda (file hunks)
         (when-let* ((patch (majutsu-interactive--build-file-patch
                             file (nreverse hunks))))
           (push patch patches)))
       by-file)
      (when patches
        (majutsu-interactive--fixup-patch
         (mapconcat #'identity (nreverse patches) ""))))))

(defun majutsu-interactive--fixup-patch (patch)
  "Fix hunk header line counts in PATCH using diff-mode."
  (with-temp-buffer
    (insert patch)
    (diff-mode)
    (diff-fixup-modifs (point-min) (point-max))
    (buffer-string)))

;;; Tool Invocation

(defvar majutsu-interactive--temp-dir nil
  "Temporary directory for patch files.")

(defun majutsu-interactive--temp-dir ()
  "Return or create temporary directory."
  (unless (and majutsu-interactive--temp-dir
               (file-directory-p majutsu-interactive--temp-dir))
    (setq majutsu-interactive--temp-dir
          (make-temp-file "majutsu-interactive-" t)))
  majutsu-interactive--temp-dir)

(defun majutsu-interactive--write-patch (patch)
  "Write PATCH to a temporary file and return its path."
  (let ((file (expand-file-name "patch.diff" (majutsu-interactive--temp-dir))))
    (with-temp-file file
      (insert patch))
    file))

(defun majutsu-interactive--write-applypatch-script ()
  "Write the applypatch helper script and return its path."
  (let ((script (expand-file-name "applypatch.sh" (majutsu-interactive--temp-dir))))
    (with-temp-file script
      (insert "#!/bin/sh\n")
      (insert "# Majutsu applypatch helper\n")
      (insert "# Args: $1=left $2=right $3=patchfile\n")
      (insert "LEFT=\"$1\"\n")
      (insert "RIGHT=\"$2\"\n")
      (insert "PATCH=\"$3\"\n")
      ;; Strategy: copy LEFT to RIGHT, then apply patch
      (insert "# Copy left to right first\n")
      (insert "rm -rf \"$RIGHT\"/* 2>/dev/null || true\n")
      (insert "rm -rf \"$RIGHT\"/.[!.]* 2>/dev/null || true\n")
      (insert "if [ -n \"$(ls -A \"$LEFT\" 2>/dev/null)\" ]; then\n")
      (insert "  cp -a \"$LEFT\"/. \"$RIGHT\"/\n")
      (insert "fi\n")
      (insert "cd \"$RIGHT\"\n")
      ;; Try git apply with --recount which recalculates line numbers
      (insert "git apply --recount --unidiff-zero -v \"$PATCH\" 2>&1 && exit 0\n")
      ;; Fallback: init git repo for 3way merge
      (insert "git init -q 2>/dev/null\n")
      (insert "git add -A 2>/dev/null\n")
      (insert "git commit -q -m 'base' --allow-empty 2>/dev/null\n")
      (insert "git apply --3way --recount -v \"$PATCH\" 2>&1\n")
      (insert "EXIT=$?\n")
      (insert "rm -rf .git 2>/dev/null\n")
      (insert "exit $EXIT\n"))
    (set-file-modes script #o755)
    script))

(defun majutsu-interactive--build-tool-config (patch-file)
  "Build jj --config arguments for applypatch tool with PATCH-FILE."
  (let ((script (majutsu-interactive--write-applypatch-script)))
    (list
     "--config" (format "merge-tools.majutsu-applypatch.program=%s"
                        (shell-quote-argument script))
     "--config" (format "merge-tools.majutsu-applypatch.edit-args=[\"$left\",\"$right\",%s]"
                        (prin1-to-string patch-file)))))

(defun majutsu-interactive--run-jj-with-patch (command args patch)
  "Run jj COMMAND with ARGS, applying PATCH via custom tool."
  (let* ((patch-file (majutsu-interactive--write-patch patch))
         (tool-config (majutsu-interactive--build-tool-config patch-file))
         (full-args (append (list command)
                            args
                            (list "-i" "--tool" "majutsu-applypatch")
                            tool-config)))
    ;; Use with-editor since split/squash may prompt for commit message
    (majutsu-run-jj-with-editor full-args)))

(defun majutsu-interactive--buffer-revision ()
  "Extract the revision from current diff buffer's range.
Returns the revision string or nil."
  (when (boundp 'majutsu-buffer-diff-range)
    (let ((range majutsu-buffer-diff-range))
      (cond
       ;; --revisions=REV format
       ((and range (stringp (car range))
             (string-match "^--revisions?=\\(.+\\)$" (car range)))
        (match-string 1 (car range)))
       ;; --from=REV --to=REV format - use --to as the revision
       ((and range (seq-some (lambda (arg)
                               (string-match "^--to=\\(.+\\)$" arg))
                             range))
        (let ((to-arg (seq-find (lambda (arg)
                                  (string-match "^--to=\\(.+\\)$" arg))
                                range)))
          (when (string-match "^--to=\\(.+\\)$" to-arg)
            (match-string 1 to-arg))))
       ;; -r REV format (list like ("-r" "REV"))
       ((and range (equal (car range) "-r") (cadr range))
        (cadr range))))))

;;; Commands

;;;###autoload
(defun majutsu-interactive-split (&optional revision)
  "Split selected changes from REVISION into a new child revision.
If REVISION is nil, uses the revision from the current diff buffer."
  (interactive)
  (let ((patch (majutsu-interactive--build-patch))
        (rev (or revision
                 (majutsu-interactive--buffer-revision)
                 (user-error "Cannot determine revision from buffer"))))
    (majutsu-interactive--run-jj-with-patch
     "split"
     (list "-r" rev)
     patch)
    (majutsu-interactive-clear)))

;;;###autoload
(defun majutsu-interactive-squash (&optional from into)
  "Squash selected changes from FROM revision into INTO revision.
If FROM is nil, uses the revision from the current diff buffer."
  (interactive)
  (let* ((patch (majutsu-interactive--build-patch))
         (from-rev (or from
                       (majutsu-interactive--buffer-revision)
                       (user-error "Cannot determine revision from buffer")))
         (into-rev (or into (concat from-rev "-"))))
    (majutsu-interactive--run-jj-with-patch
     "squash"
     (list "--from" from-rev "--into" into-rev)
     patch)
    (majutsu-interactive-clear)))

;;;###autoload
(defun majutsu-interactive-restore (&optional revision)
  "Restore (undo) selected changes from REVISION.
If REVISION is nil, uses the revision from the current diff buffer."
  (interactive)
  (let ((patch (majutsu-interactive--build-patch))
        (rev (or revision
                 (majutsu-interactive--buffer-revision)
                 (user-error "Cannot determine revision from buffer"))))
    (majutsu-interactive--run-jj-with-patch
     "restore"
     (list "--changes-in" rev)
     patch)
    (majutsu-interactive-clear)))

;;; Transient

(transient-define-prefix majutsu-interactive ()
  "Interactive partial patching for jj."
  :transient-non-suffix t
  ["Selection"
   ("h" "Toggle hunk" majutsu-interactive-toggle-hunk :transient t)
   ("f" "Toggle file" majutsu-interactive-toggle-file :transient t)
   ("r" "Toggle region" majutsu-interactive-toggle-region :transient t)
   ("c" "Clear all" majutsu-interactive-clear :transient t)]
  ["Actions"
   ("s" "Split selected" majutsu-interactive-split)
   ("S" "Squash selected" majutsu-interactive-squash)
   ("R" "Restore selected" majutsu-interactive-restore)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

;;; Keymaps

(defvar majutsu-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-i") #'majutsu-interactive)
    (define-key map (kbd "M-s") #'majutsu-interactive-toggle-hunk)
    (define-key map (kbd "M-S") #'majutsu-interactive-toggle-file)
    map)
  "Keymap for `majutsu-interactive-mode'.")

;;;###autoload
(define-minor-mode majutsu-interactive-mode
  "Minor mode for interactive partial patching in diff buffers."
  :lighter " MjI"
  :keymap majutsu-interactive-mode-map
  (if majutsu-interactive-mode
      (majutsu-interactive--ensure-selections)
    (majutsu-interactive--clear-overlays)
    (setq majutsu-interactive--selections nil)))

;;; _
(provide 'majutsu-interactive)
;;; majutsu-interactive.el ends here
