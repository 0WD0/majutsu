;;; majutsu-diff.el --- Diff viewing and editing for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Diff viewing, parsing, and editing commands for Majutsu.

;;; Code:

(require 'majutsu-core)
(require 'majutsu-process)
(require 'majutsu-log)
(require 'majutsu-transient)
(require 'magit-section)
(require 'smerge-mode)

;;; Classes

(defclass majutsu-diff-section (magit-section) ())
(defclass majutsu-file-section (magit-section)
  ((file :initarg :file)))
(defclass majutsu-hunk-section (magit-section)
  ((file :initarg :file)
   (start :initarg :hunk-start)
   (header :initarg :header)))

;;; Diff Parsing & Display

(defun majutsu--insert-diff-hunks (diff-output)
  "Parse and insert diff output as navigable hunk sections."
  (let ((lines (split-string diff-output "\n"))
        current-file
        file-section-content
        in-file-section)
    (dolist (line lines)
      (let ((clean-line (substring-no-properties line)))
        (cond
         ;; File header
         ((string-match "^diff --git a/\\(.*\\) b/\\(.*\\)$" clean-line)
          (let ((file-a (match-string 1 clean-line))
                (file-b (match-string 2 clean-line)))
            ;; Finish previous file section
            (when (and in-file-section current-file)
              (majutsu--insert-file-section current-file file-section-content))
            ;; Start new file section
            (setq current-file (or file-b file-a)
                  file-section-content (list line)
                  in-file-section t))) 
         ;; Collect lines for current file section
         (in-file-section
          (push line file-section-content))
         ;; Outside of any file section
         (t nil))))
    ;; Process final file section if any
    (when (and in-file-section current-file)
      (majutsu--insert-file-section current-file file-section-content))))

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

(defun majutsu--insert-file-section (file lines)
  "Insert a file section with its hunks."
  ;; Loosely modeled after `magit-diff-insert-file-section' to leverage
  ;; Magit's section toggling behavior for large revisions.
  (let ((ordered-lines (nreverse lines)))
    (magit-insert-section  (majutsu-file-section file nil :file file)
      (magit-insert-heading
        (propertize (majutsu--diff-file-heading file ordered-lines)
                    'font-lock-face 'magit-diff-file-heading))
      ;; Process the lines to find and insert hunks
      (let ((hunk-lines nil)
            (in-hunk nil)
            (hunk-header nil))
        (dolist (line ordered-lines)
          (cond
           ;; Hunk header
           ((string-match "^@@ .* @@" line)
            (when (and in-hunk hunk-header)
              (majutsu--insert-hunk-section file hunk-header (nreverse hunk-lines)))
            (setq hunk-header line
                  hunk-lines nil
                  in-hunk t))
           ;; Hunk content
           (in-hunk
            (push line hunk-lines))))
        ;; Insert final hunk
        (when (and in-hunk hunk-header)
          (majutsu--insert-hunk-section file hunk-header (nreverse hunk-lines)))))))

(defun majutsu--insert-hunk-section (file header lines)
  "Insert a hunk section."
  (magit-insert-section (majutsu-hunk-section file nil :file file :header header)
    (magit-insert-heading
      (propertize header 'font-lock-face 'magit-diff-hunk-heading))
    (dolist (line lines)
      (insert (propertize line
                          'font-lock-face
                          (cond
                           ((string-prefix-p "+" line) 'magit-diff-added)
                           ((string-prefix-p "-" line) 'magit-diff-removed)
                           (t 'magit-diff-context))))
      (insert "\n"))))

;;; Navigation

(defun majutsu-goto-diff-line ()
  "Jump to the line in the file corresponding to the diff line at point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (_ (eq (oref section type) 'majutsu-hunk-section))
              (file (oref section file))
              (header (oref section header))
              (repo-root (majutsu--root)))
    ;; Parse the hunk header to get line numbers
    (when (string-match "^@@.*\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?.*@@" header)
      (let* ((start-line (string-to-number (match-string 1 header)))
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
          (message "Jumped to line %d in %s" (1+ target-line) file))))))

(defun majutsu-visit-file ()
  "Visit the file at point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (file (oref section file))
              (repo-root (majutsu--root)))
    (let ((full-file-path (expand-file-name file repo-root)))
      (find-file full-file-path))))

;;; Diff Edit

(defun majutsu-diffedit-emacs ()
  "Emacs-based diffedit using built-in ediff."
  (interactive)
  (let* ((section (magit-current-section))
         (file (cond
                ((and section (eq (oref section type) 'majutsu-file-section))
                 (oref section file))
                ((and section (eq (oref section type) 'majutsu-hunk-section))
                 (oref section file))
                (t nil))))
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
                           (majutsu--run-command "file" "show" "-r" "@-" file))))

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

(defun majutsu-diffedit-smerge ()
  "Emacs-based diffedit using smerge-mode (merge conflict style)."
  (interactive)
  (let* ((section (magit-current-section))
         (file (cond
                ((and section (eq (oref section type) 'majutsu-file-section))
                 (oref section file))
                ((and section (eq (oref section type) 'majutsu-hunk-section))
                 (oref section file))
                (t nil))))
    (if file
        (majutsu-diffedit-with-smerge file)
      (majutsu-diffedit-all))))

(defun majutsu-diffedit-with-smerge (file)
  "Open smerge-mode session for a specific file."
  (let* ((repo-root (majutsu--root))
         (full-file-path (expand-file-name file repo-root))
         (parent-content (let ((default-directory repo-root))
                           (majutsu--run-command "file" "show" "-r" "@-" file)))
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
  (let ((diff-output (majutsu--run-command "diff" "--name-only")))
    (split-string diff-output "\n" t)))

;;; Diff Commands

(defun majutsu-diff-clear-selections ()
  "Clear all diff selections."
  (interactive)
  (majutsu--transient-clear-overlays majutsu-diff-from)
  (majutsu--transient-clear-overlays majutsu-diff-to)
  (setq majutsu-diff-from nil
        majutsu-diff-to nil)
  (when (called-interactively-p 'interactive)
    (message "Cleared diff selections")))

(defun majutsu-diff-set-from ()
  "Set the commit at point as diff --from."
  (interactive)
  (majutsu--transient-select-refset
   :kind "from"
   :label "[FROM]"
   :face '(:background "dark orange" :foreground "black")
   :collection-var 'majutsu-diff-from))

(defun majutsu-diff-set-to ()
  "Set the commit at point as diff --to."
  (interactive)
  (majutsu--transient-select-refset
   :kind "to"
   :label "[TO]"
   :face '(:background "dark cyan" :foreground "white")
   :collection-var 'majutsu-diff-to))

(defvar-keymap majutsu-diff-mode-map
  :doc "Keymap for `majutsu-diff-mode'."
  :parent majutsu-mode-map)

(define-derived-mode majutsu-diff-mode majutsu-mode "JJ Diff"
  "Major mode for viewing jj diffs."
  :group 'majutsu
  (setq-local line-number-mode nil))

(defvar-local majutsu-diff--last-args nil
  "Arguments used for the last jj diff command in this buffer.")

(defun majutsu-diff-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the current diff buffer."
  (interactive)
  (when majutsu-diff--last-args
    (let ((inhibit-read-only t)
          (repo-root (majutsu--root)))
      (erase-buffer)
      (setq-local majutsu--repo-root repo-root)
      (let ((default-directory repo-root)
            (output (apply #'majutsu--run-command-color majutsu-diff--last-args)))
        (magit-insert-section (diff-root)
          (magit-insert-heading (format "jj %s" (string-join majutsu-diff--last-args " ")))
          (insert "\n")
          (if (string-empty-p output)
              (insert (propertize "(No diff)" 'face 'shadow))
            (majutsu--insert-diff-hunks output))))
      (goto-char (point-min)))))

;;;###autoload
(defun majutsu-diff (&optional args)
  "Show diff with ARGS.
If called interactively, defaults to diffing the commit at point (if in
log view) or the working copy (if elsewhere)."
  (interactive
   (list (if-let* ((rev (majutsu-log--revset-at-point)))
             (list "-r" rev)
           nil)))
  (let* ((repo-root (majutsu--root))
         (buf (get-buffer-create "*majutsu-diff*"))
         ;; Ensure we use --git format for our parser to work correctly
         (final-args (append '("diff")
                             (unless (member "--git" args) '("--git"))
                             args)))
    (with-current-buffer buf
      (setq default-directory repo-root)
      (majutsu-diff-mode)
      (setq-local majutsu--repo-root repo-root)
      (setq-local majutsu-diff--last-args final-args)
      (setq-local revert-buffer-function #'majutsu-diff-refresh)
      (majutsu-diff-refresh)
      (majutsu--display-buffer-for-editor buf))))

(defun majutsu-diff-execute (&optional args)
  "Execute diff using transient selections or ARGS."
  (interactive (list (transient-args 'majutsu-diff-transient--internal)))
  (let* ((from-entry (car majutsu-diff-from))
         (to-entry (car majutsu-diff-to))
         (from (when from-entry (majutsu--transient-entry-revset from-entry)))
         (to (when to-entry (majutsu--transient-entry-revset to-entry)))
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
      (if-let* ((rev (majutsu-log--revset-at-point)))
          (setq final-args (append final-args (list "-r" rev)))
        (setq final-args (append final-args (list "-r" "@")))))

    (majutsu-diff final-args)
    ;; Clear selections after successful execution
    (majutsu-diff-clear-selections)))

(defun majutsu-diff-cleanup-on-exit ()
  "Clean up diff selections when transient exits."
  (majutsu-diff-clear-selections)
  (remove-hook 'transient-exit-hook 'majutsu-diff-cleanup-on-exit t))

;;;###autoload
(defun majutsu-diff-transient ()
  "Transient for jj diff operations."
  (interactive)
  (add-hook 'transient-exit-hook 'majutsu-diff-cleanup-on-exit nil t)
  (majutsu-diff-transient--internal))

(provide 'majutsu-diff)
;;; majutsu-diff.el ends here
