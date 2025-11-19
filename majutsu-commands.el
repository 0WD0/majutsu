;;; majutsu-commands.el --- Interactive commands for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:
;; Interactive commands for Majutsu, including transient actions.

;;; Code:

(require 'majutsu-core)
(require 'majutsu-process)
(require 'majutsu-log)
(require 'majutsu-status)
(require 'majutsu-transient)
(require 'transient)

;;; majutsu-commit

(defun majutsu-commit ()
  "Create a commit using Emacs as the editor."
  (interactive)
  (majutsu--with-editor-run '("commit")
                            "Successfully committed changes"
                            "Failed to commit"))

;;; majutsu-describe

(defun majutsu-describe ()
  "Update the description for the commit at point."
  (interactive)
  (let ((revset (or (majutsu-log--revset-at-point) "@")))
    (majutsu--with-editor-run (list "describe" "-r" revset)
                              (format "Description updated for %s" revset)
                              "Failed to update description")))

;;; Log Transient Commands

(defun majutsu-log-transient-set-revisions ()
  "Prompt for a revset and store it in log state."
  (interactive)
  (let* ((current (majutsu-log--state-get :revisions))
         (input (string-trim (read-from-minibuffer "Revset (empty to clear): " current))))
    (majutsu-log--state-set :revisions (unless (string-empty-p input) input))
    (majutsu-log-transient--redisplay)))

(defun majutsu-log-transient-clear-revisions ()
  "Clear the stored revset."
  (interactive)
  (majutsu-log--state-set :revisions nil)
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-set-limit ()
  "Prompt for a numeric limit and store it in log state."
  (interactive)
  (let* ((current (majutsu-log--state-get :limit))
         (input (string-trim (read-from-minibuffer "Limit (empty to clear): " current))))
    (cond
     ((string-empty-p input)
      (majutsu-log--state-set :limit nil))
     ((string-match-p "\\`[0-9]+\\'" input)
      (majutsu-log--state-set :limit input))
     (t
      (user-error "Limit must be a positive integer"))))
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-clear-limit ()
  "Clear the stored limit."
  (interactive)
  (majutsu-log--state-set :limit nil)
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient--toggle (key)
  "Toggle boolean KEY in log state."
  (majutsu-log--state-set key (not (majutsu-log--state-get key)))
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-toggle-reversed ()
  "Toggle reversed log ordering."
  (interactive)
  (majutsu-log-transient--toggle :reversed))

(defun majutsu-log-transient-toggle-no-graph ()
  "Toggle whether jj log should hide the ASCII graph."
  (interactive)
  (majutsu-log-transient--toggle :no-graph))

(defun majutsu-log-transient-add-path ()
  "Add a fileset/path filter to the log view."
  (interactive)
  (let* ((input (string-trim (read-from-minibuffer "Add path/pattern: ")))
         (paths (majutsu-log--state-get :filesets)))
    (when (and (not (string-empty-p input))
               (not (member input paths)))
      (majutsu-log--state-set :filesets (append paths (list input)))
      (majutsu-log-transient--redisplay))))

(defun majutsu-log-transient-clear-paths ()
  "Clear all path filters."
  (interactive)
  (majutsu-log--state-set :filesets nil)
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-reset ()
  "Reset log state to defaults."
  (interactive)
  (majutsu-log--reset-state)
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-apply ()
  "Apply the current log state by refreshing or opening the log view."
  (interactive)
  (majutsu-log--refresh-view))

;;; majutsu-edit

(defun majutsu-enter-dwim ()
  "Context-sensitive Enter key behavior."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ;; On a changeset/commit - edit it with jj edit
     ((and section
           (eq (oref section type) 'majutsu-log-entry-section)
           (slot-boundp section 'commit-id))
      (majutsu-edit-changeset-at-point))

     ;; On a diff hunk line - jump to that line in the file
     ((and section
           (eq (oref section type) 'majutsu-hunk-section)
           (slot-boundp section 'file))
      (majutsu-goto-diff-line))

     ;; On a file section - visit the file
     ((and section
           (eq (oref section type) 'majutsu-file-section)
           (slot-boundp section 'file))
      (majutsu-visit-file)))))

(defun majutsu-edit-changeset ()
  "Edit commit at point."
  (interactive)
  (when-let* ((revset (majutsu-log--revset-at-point)))
    (let ((result (majutsu--run-command "edit" revset)))
      (if (majutsu--handle-command-result (list "edit" revset) result
                                          (format "Now editing commit %s" revset)
                                          "Failed to edit commit")
          (majutsu-log-refresh)))))

(defun majutsu-edit-changeset-at-point ()
  "Edit the commit at point using jj edit."
  (interactive)
  (when-let* ((revset (majutsu-log--revset-at-point)))
    (let ((result (majutsu--run-command "edit" revset)))
      (if (majutsu--handle-command-result (list "edit" revset) result
                                          (format "Now editing revset %s" revset)
                                          "Failed to edit commit")
          (progn
            (majutsu-log-refresh)
            (back-to-indentation))))))

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

;;; majutsu-diffedit

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

;;; majutsu-undo

(defun majutsu-undo ()
  "Undo the last change."
  (interactive)
  (if (and majutsu-confirm-critical-actions
           (not (yes-or-no-p "Undo the most recent change? ")))
      (message "Undo canceled")
    (let ((revset (majutsu-log--revset-at-point)))
      (majutsu--run-command "undo")
      (majutsu-log-refresh)
      (when revset
        (majutsu-goto-commit revset)))))

;;; majutsu-redo

(defun majutsu-redo ()
  "Redo the last undone change."
  (interactive)
  (if (and majutsu-confirm-critical-actions
           (not (yes-or-no-p "Redo the previously undone change? ")))
      (message "Redo canceled")
    (let ((revset (majutsu-log--revset-at-point)))
      (majutsu--run-command "redo")
      (majutsu-log-refresh)
      (when revset
        (majutsu-goto-commit revset)))))

;;; majutsu-abandon

(defun majutsu-abandon ()
  "Abandon the changeset at point."
  (interactive)
  (if-let* ((revset (majutsu-log--revset-at-point)))
      (if (and majutsu-confirm-critical-actions
               (not (yes-or-no-p (format "Abandon changeset %s? " revset))))
          (message "Abandon canceled")
        (progn
          (majutsu--run-command "abandon" "-r" revset)
          (majutsu-log-refresh)))
    (message "No changeset at point to abandon")))

;;; majutsu-squash

;;;###autoload
(defun majutsu-squash-clear-selections ()
  "Clear all squash selections and overlays."
  (interactive)
  (majutsu--transient-clear-overlays majutsu-squash-from)
  (majutsu--transient-clear-overlays majutsu-squash-into)
  (setq majutsu-squash-from nil
        majutsu-squash-into nil)
  (message "Cleared all squash selections"))

;;;###autoload
(defun majutsu-squash-set-from ()
  "Set the commit at point as squash `from' source."
  (interactive)
  (majutsu--transient-toggle-refsets
   :kind "from"
   :label "[FROM]"
   :face '(:background "dark orange" :foreground "white")
   :collection-var 'majutsu-squash-from))

;;;###autoload
(defun majutsu-squash-set-into ()
  "Set the commit at point as squash `into' destination."
  (interactive)
  (majutsu--transient-select-refset
   :kind "into"
   :label "[INTO]"
   :face '(:background "dark cyan" :foreground "white")
   :collection-var 'majutsu-squash-into))

;;;###autoload
(defun majutsu-squash-execute (&optional args)
  "Execute squash with selections recorded in the transient."
  (interactive (list (transient-args 'majutsu-squash-transient--internal)))
  (let* ((keep (member "--keep" args))
         (from-entries majutsu-squash-from)
         (into-entry (majutsu-squash--into-entry))
         (from-revsets (majutsu--transient-normalize-revsets from-entries))
         (into (when into-entry (majutsu--transient-entry-revset into-entry))))
    (cond
     ((and from-revsets into)
      (majutsu--squash-run from-revsets into keep))
     (from-revsets
      (majutsu--squash-run from-revsets nil keep))
     ((majutsu-log--revset-at-point)
      (majutsu--squash-run (list (majutsu-log--revset-at-point)) nil keep))
     (t
      (majutsu--message-with-log "No commit selected for squash")))))

(defun majutsu--squash-run (from-list into keep)
  "Run jj squash using with-editor."
  (let* ((normalized (majutsu--transient-normalize-revsets from-list))
         (froms (or normalized '("@")))
         (args (append '("squash")
                       (apply #'append (mapcar (lambda (rev) (list "--from" rev)) froms))
                       (when into (list "--into" into))
                       (when keep '("--keep-emptied"))))
         (from-display (string-join froms ", "))
         (success-msg (if into
                          (format "Squashed %s into %s" from-display into)
                        (format "Squashed %s into parent" from-display))))
    (majutsu--with-editor-run args success-msg "Squash failed"
                              #'majutsu-squash-clear-selections)))

(defun majutsu-squash-cleanup-on-exit ()
  "Clean up squash selections when transient exits."
  (unless (eq this-command 'majutsu-mode-bury-squash)
    (majutsu-squash-clear-selections)
    (remove-hook 'transient-exit-hook 'majutsu-squash-cleanup-on-exit t)))

;;;###autoload
(defun majutsu-squash-transient ()
  "Transient for jj squash operations."
  (interactive)
  ;; Add cleanup hook for when transient exits
  (add-hook 'transient-exit-hook 'majutsu-squash-cleanup-on-exit nil t)
  (majutsu-squash-transient--internal))

(defun majutsu-mode-bury-squash ()
  (interactive)
  (transient-quit-one))

;;; majutsu-new

(defun majutsu-new (arg)
  "Create a new changeset.
Without prefix ARG, use the changeset at point (or `@` when unavailable).
With prefix ARG, open the new transient for interactive selection."
  (interactive "P")
  (if arg
      (majutsu-new-transient)
    (let* ((parent (majutsu-log--revset-at-point))
           (parents (when parent (list parent)))
           (args (majutsu-new--build-args
                  :parents parents
                  :after '()
                  :before '()
                  :message nil
                  :no-edit nil)))
      (majutsu-new--run-command args))))

;;;###autoload
(defun majutsu-new-clear-selections ()
  "Clear all jj new selections and overlays."
  (interactive)
  (majutsu--transient-clear-overlays majutsu-new-parents)
  (majutsu--transient-clear-overlays majutsu-new-after)
  (majutsu--transient-clear-overlays majutsu-new-before)
  (setq majutsu-new-parents nil
        majutsu-new-after nil
        majutsu-new-before nil
        majutsu-new-message nil
        majutsu-new-no-edit nil)
  (when (called-interactively-p 'interactive)
    (message "Cleared all jj new selections")))

;;;###autoload
(defun majutsu-new-toggle-parent ()
  "Toggle the commit at point as a jj new parent."
  (interactive)
  (majutsu--transient-toggle-refsets
   :kind "parent"
   :label "[PARENT]"
   :face '(:background "dark orange" :foreground "black")
   :collection-var 'majutsu-new-parents))

;;;###autoload
(defun majutsu-new-toggle-after ()
  "Toggle the commit at point as a jj new --after target."
  (interactive)
  (majutsu--transient-toggle-refsets
   :kind "--after"
   :label "[AFTER]"
   :face '(:background "dark blue" :foreground "white")
   :collection-var 'majutsu-new-after))

;;;###autoload
(defun majutsu-new-toggle-before ()
  "Toggle the commit at point as a jj new --before target."
  (interactive)
  (majutsu--transient-toggle-refsets
   :kind "--before"
   :label "[BEFORE]"
   :face '(:background "dark magenta" :foreground "white")
   :collection-var 'majutsu-new-before))

;;;###autoload
(defun majutsu-new-edit-message ()
  "Prompt for a jj new commit message."
  (interactive)
  (let ((input (read-string "New change message (empty to clear): " majutsu-new-message)))
    (if (string-empty-p input)
        (setq majutsu-new-message nil)
      (setq majutsu-new-message input))
    (message (if majutsu-new-message
                 "Set message for jj new"
               "Cleared message for jj new"))))

;;;###autoload
(defun majutsu-new-toggle-no-edit ()
  "Toggle passing --no-edit to jj new."
  (interactive)
  (setq majutsu-new-no-edit (not majutsu-new-no-edit))
  (message "jj new --no-edit %s"
           (if majutsu-new-no-edit "enabled" "disabled")))

(defun majutsu-new--run-command (args)
  "Execute jj new with ARGS and refresh the log on success."
  (let ((result (apply #'majutsu--run-command args)))
    (when (majutsu--handle-command-result
           args result
           "Created new changeset"
           "Failed to create new changeset")
      (majutsu-log-refresh)
      (majutsu-goto-current)
      t)))

;;;###autoload
(defun majutsu-new-execute ()
  "Execute jj new using the current transient selections."
  (interactive)
  (let ((args (majutsu-new--build-args)))
    (when (majutsu-new--run-command args)
      (majutsu-new-clear-selections))))

;;;###autoload
(defun majutsu-new-cleanup-on-exit ()
  "Clean up jj new selections when the transient exits."
  (majutsu-new-clear-selections)
  (remove-hook 'transient-exit-hook 'majutsu-new-cleanup-on-exit t))

;;;###autoload
(defun majutsu-new-transient ()
  "Open the jj new transient."
  (interactive)
  (add-hook 'transient-exit-hook 'majutsu-new-cleanup-on-exit nil t)
  (majutsu-new-transient--internal))

;;; majutsu-diff

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

(defvar majutsu-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "RET") 'majutsu-enter-dwim)
    map)
  "Keymap for `majutsu-diff-mode'.")

(define-derived-mode majutsu-diff-mode magit-section-mode "JJ Diff"
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

;;; majutsu-navigation

(defun majutsu-goto-current ()
  "Jump to the current changeset (@)."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward "^.*@.*$" nil t)
      (goto-char (line-beginning-position))
    (message "Current changeset (@) not found")))

(defun majutsu-goto-commit (commit-id)
  "Jump to a specific COMMIT-ID in the log."
  (interactive "sCommit ID: ")
  (let ((start-pos (point)))
    (goto-char (point-min))
    (if (re-search-forward (regexp-quote commit-id) nil t)
        (goto-char (line-beginning-position))
      (goto-char start-pos)
      (message "Commit %s not found" commit-id))))

(defun majutsu--goto-log-entry (change-id &optional commit-id)
  "Move point to the log entry section matching CHANGE-ID.
When CHANGE-ID is nil, fall back to COMMIT-ID.
Return non-nil when the section could be located."
  (when-let* ((section (majutsu--find-log-entry-section change-id commit-id)))
    (magit-section-goto section)
    (goto-char (oref section start))
    t))

;;;###autoload
(defun majutsu-goto-next-changeset ()
  "Navigate to the next changeset in the log."
  (interactive)
  (let ((pos (point))
        found)
    (while (and (not found)
                (< (point) (point-max)))
      (magit-section-forward)
      (when-let* ((section (magit-current-section)))
        (when (and (eq (oref section type) 'majutsu-log-entry-section)
                   (> (point) pos))
          (setq found t))))
    (unless found
      (goto-char pos)
      (message "No more changesets"))))

;;;###autoload
(defun majutsu-goto-prev-changeset ()
  "Navigate to the previous changeset in the log."
  (interactive)
  (let ((pos (point))
        found)
    (while (and (not found)
                (> (point) (point-min)))
      (magit-section-backward)
      (when-let* ((section (magit-current-section)))
        (when (and (eq (oref section type) 'majutsu-log-entry-section)
                   (< (point) pos))
          (setq found t))))
    (unless found
      (goto-char pos)
      (message "No more changesets"))))

;;; majutsu-duplicate

(defun majutsu-duplicate-clear-selections ()
  "Clear duplicate selections and overlays."
  (interactive)
  (majutsu--transient-clear-overlays majutsu-duplicate-sources)
  (majutsu--transient-clear-overlays majutsu-duplicate-destinations)
  (majutsu--transient-clear-overlays majutsu-duplicate-after)
  (majutsu--transient-clear-overlays majutsu-duplicate-before)
  (setq majutsu-duplicate-sources nil
        majutsu-duplicate-destinations nil
        majutsu-duplicate-after nil
        majutsu-duplicate-before nil)
  (when (called-interactively-p 'interactive)
    (message "Cleared duplicate selections")))

(defun majutsu-duplicate-toggle-source ()
  "Toggle the commit at point as a duplicate source."
  (interactive)
  (majutsu--transient-toggle-refsets
   :kind "source"
   :label "[SRC]"
   :face '(:background "goldenrod" :foreground "black")
   :collection-var 'majutsu-duplicate-sources))

(defun majutsu-duplicate-toggle-destination ()
  "Toggle the commit at point as a duplicate destination."
  (interactive)
  (majutsu--transient-toggle-refsets
   :kind "--destination"
   :label "[DEST]"
   :face '(:background "dark green" :foreground "white")
   :collection-var 'majutsu-duplicate-destinations))

(defun majutsu-duplicate-toggle-after ()
  "Toggle the commit at point as a duplicate --after entry."
  (interactive)
  (majutsu--transient-toggle-refsets
   :kind "--after"
   :label "[AFTER]"
   :face '(:background "dark blue" :foreground "white")
   :collection-var 'majutsu-duplicate-after))

(defun majutsu-duplicate-toggle-before ()
  "Toggle the commit at point as a duplicate --before entry."
  (interactive)
  (majutsu--transient-toggle-refsets
   :kind "--before"
   :label "[BEFORE]"
   :face '(:background "dark magenta" :foreground "white")
   :collection-var 'majutsu-duplicate-before))

(defun majutsu-duplicate--run-command (args)
  "Execute jj duplicate with ARGS and refresh log."
  (let ((result (apply #'majutsu--run-command args)))
    (when (majutsu--handle-command-result
           args result
           "Duplicated changeset(s)"
           "Failed to duplicate changeset(s)")
      (majutsu-log-refresh)
      t)))

(defun majutsu-duplicate-execute ()
  "Execute jj duplicate using transient selections."
  (interactive)
  (when (majutsu-duplicate--run-command (majutsu-duplicate--build-args))
    (majutsu-duplicate-clear-selections)))

(defun majutsu-duplicate-cleanup-on-exit ()
  "Cleanup duplicate selections after transient exit."
  (majutsu-duplicate-clear-selections)
  (remove-hook 'transient-exit-hook 'majutsu-duplicate-cleanup-on-exit t))

;;;###autoload
(defun majutsu-duplicate-transient ()
  "Open the jj duplicate transient."
  (interactive)
  (add-hook 'transient-exit-hook 'majutsu-duplicate-cleanup-on-exit nil t)
  (majutsu-duplicate-transient--internal))

;;;###autoload
(defun majutsu-duplicate (arg)
  "Duplicate the changeset at point.
With prefix ARG, open the duplicate transient."
  (interactive "P")
  (if arg
      (majutsu-duplicate-transient)
    (let* ((rev (majutsu-log--revset-at-point))
           (args (majutsu-duplicate--build-args
                  :sources (list (or rev "@")))))
      (majutsu-duplicate--run-command args))))

;;; majutsu-rebase

;;;###autoload
(defun majutsu-rebase-clear-selections ()
  "Clear all rebase selections and overlays."
  (interactive)
  (majutsu--transient-clear-overlays majutsu-rebase-source)
  (majutsu--transient-clear-overlays majutsu-rebase-destinations)
  (setq majutsu-rebase-source nil
        majutsu-rebase-destinations nil)
  (message "Cleared all rebase selections"))

;;;###autoload
(defun majutsu-rebase-set-source ()
  "Set the commit at point as rebase source."
  (interactive)
  (majutsu--transient-select-refset
   :kind "source"
   :label "[SOURCE]"
   :face '(:background "dark green" :foreground "white")
   :collection-var 'majutsu-rebase-source))

;;;###autoload
(defun majutsu-rebase-toggle-destination ()
  "Toggle the commit at point as a rebase destination."
  (interactive)
  (majutsu--transient-toggle-refsets
   :kind "destination"
   :label "[DEST]"
   :face '(:background "dark blue" :foreground "white")
   :collection-var 'majutsu-rebase-destinations))

;;;###autoload
(defun majutsu-rebase-execute ()
  "Execute rebase with selected source and destinations."
  (interactive)
  (if (and (majutsu-rebase--source-entry) majutsu-rebase-destinations)
      (let* ((source-entry (majutsu-rebase--source-entry))
             (source-rev (majutsu--transient-entry-revset source-entry))
             (source-display (majutsu--transient-entry-display source-entry))
             (dest-revs (seq-filter (lambda (rev) (and rev (not (string-empty-p rev))))
                                    (majutsu-rebase--destination-revsets)))
             (dest-display (majutsu-rebase--destination-display)))
        (when (and source-rev dest-revs
                   (yes-or-no-p (format "Rebase %s -> %s? " source-display dest-display)))
          (let* ((dest-args (apply #'append (mapcar (lambda (dest) (list "-d" dest)) dest-revs)))
                 (all-args (append (list "rebase" "-s" source-rev) dest-args))
                 (progress-msg (format "Rebasing %s onto %s" source-display dest-display))
                 (success-msg (format "Rebase completed: %s -> %s" source-display dest-display)))
            (majutsu--message-with-log "%s..." progress-msg)
            (let ((result (apply #'majutsu--run-command all-args)))
              (if (majutsu--handle-command-result all-args result success-msg "Rebase failed")
                  (progn
                    (majutsu-rebase-clear-selections)
                    (majutsu-log-refresh)))))))
    (majutsu--message-with-log "Please select source (s) and at least one destination (d) first")))

(defun majutsu-rebase-cleanup-on-exit ()
  "Clean up rebase selections when transient exits."
  (majutsu-rebase-clear-selections)
  (remove-hook 'transient-exit-hook 'majutsu-rebase-cleanup-on-exit t))

;;;###autoload
(defun majutsu-rebase-transient ()
  "Transient for jj rebase operations."
  (interactive)
  ;; Add cleanup hook for when transient exits
  (add-hook 'transient-exit-hook 'majutsu-rebase-cleanup-on-exit nil t)
  (majutsu-rebase-transient--internal))

;;; majutsu-bookmark

;;;###autoload
(defun majutsu-bookmark-transient ()
  "Transient for jj bookmark operations."
  (interactive)
  (majutsu-bookmark-transient--internal))

(defun majutsu--extract-bookmark-names (text)
  "Extract bookmark names from jj command output TEXT."
  (let ((names '())
        (start 0))
    (while (string-match "bookmark[: ]+\\([^ \n,]+\\)" text start)
      (push (match-string 1 text) names)
      (setq start (match-end 0)))
    (nreverse names)))

(defun majutsu--get-bookmark-names (&optional all-remotes)
  "Return bookmark names using --quiet to suppress hints.
When ALL-REMOTES is non-nil, include remote bookmarks formatted as NAME@REMOTE."
  (let* ((template (if all-remotes
                       "if(remote, name ++ '@' ++ remote ++ '\n', '')"
                     "name ++ '\n'"))
         (args (append '("bookmark" "list" "--quiet")
                       (and all-remotes '("--all"))
                       (list "-T" template))))
    (delete-dups (split-string (apply #'majutsu--run-command args) "\n" t))))

(defun majutsu-bookmark-create ()
  "Create a new bookmark."
  (interactive)
  (let* ((revset (or (majutsu-log--revset-at-point) "@"))
         (name (read-string "Bookmark name: ")))
    (unless (string-empty-p name)
      (majutsu--run-command "bookmark" "create" name "-r" revset)
      (majutsu-log-refresh))))

(defun majutsu-bookmark-delete ()
  "Delete a bookmark and propagate on next push."
  (interactive)
  (let* ((names (majutsu--get-bookmark-names))
         (table (majutsu--completion-table-with-category names 'majutsu-bookmark))
         (choice (and names (completing-read "Delete bookmark (propagates on push): " table nil t))))
    (if (not choice)
        (message "No bookmarks found")
      (majutsu--run-command "bookmark" "delete" choice)
      (majutsu-log-refresh)
      (message "Deleted bookmark '%s'" choice))))

(defun majutsu-bookmark-forget ()
  "Forget a bookmark (local only, no deletion propagation)."
  (interactive)
  (let* ((names (majutsu--get-bookmark-names))
         (table (majutsu--completion-table-with-category names 'majutsu-bookmark))
         (choice (and names (completing-read "Forget bookmark: " table nil t))))
    (if (not choice)
        (message "No bookmarks found")
      (majutsu--run-command "bookmark" "forget" choice)
      (majutsu-log-refresh)
      (message "Forgot bookmark '%s'" choice))))

(defun majutsu-bookmark-track ()
  "Track remote bookmark(s)."
  (interactive)
  (let* ((remote-bookmarks (majutsu--get-bookmark-names t))
         (table (majutsu--completion-table-with-category remote-bookmarks 'majutsu-bookmark))
         (choice (and remote-bookmarks (completing-read "Track remote bookmark: " table nil t))))
    (if (not choice)
        (message "No remote bookmarks found")
      (majutsu--run-command "bookmark" "track" choice)
      (majutsu-log-refresh)
      (message "Tracking bookmark '%s'" choice))))

;;;###autoload
(defun majutsu-bookmark-list (&optional all)
  "List bookmarks in a temporary buffer.
With prefix ALL, include remote bookmarks."
  (interactive "P")
  (let* ((args (append '("bookmark" "list" "--quiet") (and all '("--all"))))
         (output (apply #'majutsu--run-command-color args))
         (buf (get-buffer-create "*Majutsu Bookmarks*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
      (view-mode 1))
    (funcall majutsu-log-display-function buf)))

;;;###autoload
(defun majutsu--bookmark-read-move-args ()
  "Return interactive arguments for bookmark move commands."
  (let* ((existing (majutsu--get-bookmark-names))
         (table (majutsu--completion-table-with-category existing 'majutsu-bookmark))
         (crm-separator (or (bound-and-true-p crm-separator) ", *"))
         (names (completing-read-multiple "Move bookmark(s): " table nil t))
         (at (or (majutsu-log--revset-at-point) "@"))
         (rev (read-string (format "Target revision (default %s): " at) nil nil at)))
    (ignore crm-separator)
    (list rev names)))

(defun majutsu--bookmark-move (commit names &optional allow-backwards)
  "Internal helper to move bookmark(s) NAMES to COMMIT.
When ALLOW-BACKWARDS is non-nil, include `--allow-backwards'."
  (when names
    (let ((args (append '("bookmark" "move")
                        (and allow-backwards '("--allow-backwards"))
                        (list "-t" commit)
                        names)))
      (apply #'majutsu--run-command args)
      (majutsu-log-refresh)
      (message (if allow-backwards
                   "Moved bookmark(s) (allow backwards) to %s: %s"
                 "Moved bookmark(s) to %s: %s")
               commit (string-join names ", ")))))

;;;###autoload
(defun majutsu-bookmark-move (commit names &optional allow-backwards)
  "Move existing bookmark(s) NAMES to COMMIT.
With optional ALLOW-BACKWARDS, pass `--allow-backwards' to jj."
  (interactive (majutsu--bookmark-read-move-args))
  (majutsu--bookmark-move commit names allow-backwards))

;;;###autoload
(defun majutsu-bookmark-move-allow-backwards (commit names)
  "Move bookmark(s) NAMES to COMMIT allowing backwards moves."
  (interactive (majutsu--bookmark-read-move-args))
  (majutsu--bookmark-move commit names t))

;;;###autoload
(defun majutsu-bookmark-rename (old new)
  "Rename bookmark OLD to NEW."
  (interactive
   (let* ((existing (majutsu--get-bookmark-names))
          (table (majutsu--completion-table-with-category existing 'majutsu-bookmark))
          (old (completing-read "Rename bookmark: " table nil t))
          (new (read-string (format "New name for %s: " old))))
     (list old new)))
  (when (and (not (string-empty-p old)) (not (string-empty-p new)))
    (majutsu--run-command "bookmark" "rename" old new)
    (majutsu-log-refresh)
    (message "Renamed bookmark '%s' -> '%s'" old new)))

;;;###autoload
(defun majutsu-bookmark-set (name commit)
  "Create or update bookmark NAME to point to COMMIT."
  (interactive
   (let* ((existing (majutsu--get-bookmark-names))
          (table (majutsu--completion-table-with-category existing 'majutsu-bookmark))
          (name (completing-read "Set bookmark: " table nil nil))
          (at (or (majutsu-log--revset-at-point) "@"))
          (rev (read-string (format "Target revision (default %s): " at) nil nil at)))
     (list name rev)))
  (majutsu--run-command "bookmark" "set" name "-r" commit)
  (majutsu-log-refresh)
  (message "Set bookmark '%s' to %s" name commit))

;;;###autoload
(defun majutsu-bookmark-untrack (names)
  "Stop tracking remote bookmark(s) NAMES (e.g., name@remote)."
  (interactive
   (let* ((remote-names (majutsu--get-bookmark-names t))
          (table (majutsu--completion-table-with-category remote-names 'majutsu-bookmark))
          (crm-separator (or (bound-and-true-p crm-separator) ", *"))
          (names (completing-read-multiple "Untrack remote bookmark(s): " table nil t)))
     (list names)))

  (defvar crm-separator)
  (when names
    (apply #'majutsu--run-command (append '("bookmark" "untrack") names))
    (majutsu-log-refresh)
    (message "Untracked: %s" (string-join names ", "))))

(defun majutsu--completion-table-with-category (candidates category)
  "Wrap CANDIDATES with completion METADATA to set CATEGORY.
This prevents third-party UIs (e.g., icons for `bookmark') from
misclassifying Majutsu candidates."
  (let ((metadata `(metadata (category . ,category))))
    (cond
     ((fboundp 'completion-table-with-metadata)
      (completion-table-with-metadata candidates metadata))
     ((functionp candidates)
      (lambda (string pred action)
        (if (eq action 'metadata)
            metadata
          (funcall candidates string pred action))))
     (t
      (lambda (string pred action)
        (if (eq action 'metadata)
            metadata
          (complete-with-action action candidates string pred)))))))

;;; majutsu-git

(defun majutsu-git-push (args)
  "Push to git remote with ARGS."
  (interactive (list (transient-args 'majutsu-git-push-transient)))
  (let* ((allow-new? (member "--allow-new" args))
         (all? (member "--all" args))
         (tracked? (member "--tracked" args))
         (deleted? (member "--deleted" args))
         (allow-empty? (member "--allow-empty-description" args))
         (allow-private? (member "--allow-private" args))
         (dry-run? (member "--dry-run" args))

         (remote-arg (seq-find (lambda (arg) (string-prefix-p "--remote=" arg)) args))
         (remote (when remote-arg (substring remote-arg (length "--remote="))))

         ;; Collect potential multi-value options supplied via --opt=value
         (bookmark-args (seq-filter (lambda (arg) (string-prefix-p "--bookmark=" arg)) args))
         (revision-args (seq-filter (lambda (arg) (string-prefix-p "--revisions=" arg)) args))
         (change-args   (seq-filter (lambda (arg) (string-prefix-p "--change=" arg)) args))
         (named-args    (seq-filter (lambda (arg) (string-prefix-p "--named=" arg)) args))

         (cmd-args (append '("git" "push")
                           (when remote (list "--remote" remote))
                           (when allow-new? '("--allow-new"))
                           (when all? '("--all"))
                           (when tracked? '("--tracked"))
                           (when deleted? '("--deleted"))
                           (when allow-empty? '("--allow-empty-description"))
                           (when allow-private? '("--allow-private"))
                           (when dry-run? '("--dry-run"))

                           ;; Expand = style into separate args as jj accepts space-separated
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--bookmark" (substring s (length "--bookmark="))))
                                                   bookmark-args))
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--revisions" (substring s (length "--revisions="))))
                                                   revision-args))
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--change" (substring s (length "--change="))))
                                                   change-args))
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--named" (substring s (length "--named="))))
                                                   named-args))))

         (success-msg (cond
                       ((and bookmark-args (= (length bookmark-args) 1))
                        (format "Successfully pushed bookmark %s"
                                (substring (car bookmark-args) (length "--bookmark="))))
                       (bookmark-args "Successfully pushed selected bookmarks")
                       (t "Successfully pushed to remote"))))
    (let ((result (apply #'majutsu--run-command cmd-args)))
      (when (majutsu--handle-push-result cmd-args result success-msg)
        (majutsu-log-refresh)))))

(defun majutsu--handle-push-result (_cmd-args result success-msg)
  "Enhanced push result handler with bookmark analysis."
  (let ((trimmed-result (string-trim result)))
    (majutsu--debug "Push result: %s" trimmed-result)

    ;; Always show the raw command output first (like CLI)
    (unless (string-empty-p trimmed-result)
      (message "%s" trimmed-result))

    (cond
     ;; Check for bookmark push restrictions
     ((or (string-match-p "Refusing to push" trimmed-result)
          (string-match-p "Refusing to create new remote bookmark" trimmed-result)
          (string-match-p "would create new heads" trimmed-result))
      ;; Extract bookmark names that couldn't be pushed
      (let ((bookmark-names (majutsu--extract-bookmark-names trimmed-result)))
        (if bookmark-names
            (message "💡 Use 'jj git push --allow-new' to push new bookmarks: %s"
                     (string-join bookmark-names ", "))
          (message "💡 Use 'jj git push --allow-new' to push new bookmarks")))
      nil)

     ;; Check for authentication issues
     ((string-match-p "Permission denied\\|authentication failed\\|403" trimmed-result)
      (message "💡 Check your git credentials and repository permissions")
      nil)

     ;; Check for network issues
     ((string-match-p "Could not resolve hostname\\|Connection refused\\|timeout" trimmed-result)
      (message "💡 Check your network connection and remote URL")
      nil)

     ;; Check for non-fast-forward issues
     ((string-match-p "non-fast-forward\\|rejected.*fetch first" trimmed-result)
      (message "💡 Run 'jj git fetch' first to update remote tracking")
      nil)

     ;; Analyze majutsu-specific push patterns and provide contextual help
     ((string-match-p "Nothing changed" trimmed-result)
      (message "💡 Nothing to push - all bookmarks are up to date")
      t)

     ;; General error check
     ((or (string-match-p "^error:" trimmed-result)
          (string-match-p "^fatal:" trimmed-result))
      nil)                              ; Error already shown above

     ;; Success case
     (t
      (when (string-empty-p trimmed-result)
        (message "%s" success-msg))
      t))))

(defun majutsu-git-fetch (args)
  "Fetch from git remote with ARGS from transient."
  (interactive (list (transient-args 'majutsu-git-fetch-transient)))
  (majutsu--message-with-log "Fetching from remote...")
  (let* ((tracked? (member "--tracked" args))
         (all-remotes? (member "--all-remotes" args))

         (branch-args (seq-filter (lambda (arg) (string-prefix-p "--branch=" arg)) args))
         (remote-args (seq-filter (lambda (arg) (string-prefix-p "--remote=" arg)) args))

         (cmd-args (append '("git" "fetch")
                           (when tracked? '("--tracked"))
                           (when all-remotes? '("--all-remotes"))
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--branch" (substring s (length "--branch="))))
                                                   branch-args))
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--remote" (substring s (length "--remote="))))
                                                   remote-args))))
         (result (apply #'majutsu--run-command cmd-args)))
    (if (majutsu--handle-command-result cmd-args result
                                        "Fetched from remote" "Fetch failed")
        (majutsu-log-refresh))))

(defun majutsu--get-git-remotes ()
  "Return a list of Git remote names for the current repository.
Tries `jj git remote list' first, then falls back to `git remote'."
  (let* ((out (condition-case _
                  (majutsu--run-command "git" "remote" "list")
                (error "")))
         (names (if (and out (not (string-empty-p out)))
                    (let* ((lines (split-string out "\n" t))
                           (names (mapcar (lambda (l)
                                            (car (split-string l "[ :\t]" t)))
                                          lines)))
                      (delete-dups (copy-sequence names)))
                  ;; Fallback to plain `git remote`
                  (with-temp-buffer
                    (let* ((default-directory (majutsu--root))
                           (exit (process-file "git" nil t nil "remote")))
                      (when (eq exit 0)
                        (split-string (buffer-string) "\n" t)))))))
    names))

(defun majutsu-git-remote-list ()
  "List Git remotes in a temporary buffer."
  (interactive)
  (let* ((output (majutsu--run-command-color "git" "remote" "list"))
         (buf (get-buffer-create "*Majutsu Git Remotes*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
      (view-mode 1))
    (funcall majutsu-log-display-function buf)))

(defun majutsu-git-remote-add (args)
  "Add a Git remote. Prompts for name and URL; respects ARGS from transient."
  (interactive (list (transient-args 'majutsu-git-remote-transient)))
  (let* ((remote (read-string "Remote name: "))
         (url (read-string (format "URL for %s: " remote)))
         (fetch-tags (seq-find (lambda (a) (string-prefix-p "--fetch-tags=" a)) args))
         (cmd-args (append '("git" "remote" "add")
                           (when fetch-tags (list fetch-tags))
                           (list remote url)))
         (result (apply #'majutsu--run-command cmd-args)))
    (majutsu--handle-command-result cmd-args result
                                    (format "Added remote %s" remote)
                                    "Failed to add remote")))

(defun majutsu-git-remote-remove ()
  "Remove a Git remote and forget its bookmarks."
  (interactive)
  (let* ((remotes (or (majutsu--get-git-remotes) '("origin")))
         (remote (completing-read "Remove remote: " remotes nil t)))
    (when (and remote (not (string-empty-p remote)))
      (let* ((cmd-args (list "git" "remote" "remove" remote))
             (result (apply #'majutsu--run-command cmd-args)))
        (majutsu--handle-command-result cmd-args result
                                        (format "Removed remote %s" remote)
                                        "Failed to remove remote")))))

(defun majutsu-git-remote-rename ()
  "Rename a Git remote."
  (interactive)
  (let* ((remotes (or (majutsu--get-git-remotes) '("origin")))
         (old (completing-read "Rename remote: " remotes nil t))
         (new (read-string (format "New name for %s: " old))))
    (when (and (not (string-empty-p old)) (not (string-empty-p new)))
      (let* ((cmd-args (list "git" "remote" "rename" old new))
             (result (apply #'majutsu--run-command cmd-args)))
        (majutsu--handle-command-result cmd-args result
                                        (format "Renamed remote %s -> %s" old new)
                                        "Failed to rename remote")))))

(defun majutsu-git-remote-set-url ()
  "Set URL of a Git remote."
  (interactive)
  (let* ((remotes (or (majutsu--get-git-remotes) '("origin")))
         (remote (completing-read "Set URL for remote: " remotes nil t))
         (url (read-string (format "New URL for %s: " remote))))
    (when (and (not (string-empty-p remote)) (not (string-empty-p url)))
      (let* ((cmd-args (list "git" "remote" "set-url" remote url))
             (result (apply #'majutsu--run-command cmd-args)))
        (majutsu--handle-command-result cmd-args result
                                        (format "Set URL for %s" remote)
                                        "Failed to set remote URL")))))

(defun majutsu-git-clone (args)
  "Clone a Git repo into a new jj repo.
Prompts for SOURCE and optional DEST; uses ARGS."
  (interactive (list (transient-args 'majutsu-git-clone-transient)))
  (let* ((source (read-string "Source (URL or path): "))
         (dest   (let ((d (read-directory-name "Destination (optional): " nil nil t)))
                   (when (and d (not (string-empty-p (expand-file-name d))))
                     ;; If user picks current dir, treat as empty and let jj default
                     (let ((dd (directory-file-name d)))
                       (if (string= dd (directory-file-name default-directory)) nil dd)))))
         (remote-name (let ((arg (seq-find (lambda (a) (string-prefix-p "--remote=" a)) args)))
                        (when arg (substring arg (length "--remote=")))))
         (depth (let ((arg (seq-find (lambda (a) (string-prefix-p "--depth=" a)) args)))
                  (when arg (substring arg (length "--depth=")))))
         (fetch-tags (seq-find (lambda (a) (string-prefix-p "--fetch-tags=" a)) args))
         (colocate? (member "--colocate" args))
         (no-colocate? (member "--no-colocate" args))
         (cmd-args (append '("git" "clone")
                           (when remote-name (list "--remote" remote-name))
                           (when colocate? '("--colocate"))
                           (when no-colocate? '("--no-colocate"))
                           (when depth (list "--depth" depth))
                           (when fetch-tags (list fetch-tags))
                           (list source)
                           (when dest (list dest))))
         (result (apply #'majutsu--run-command cmd-args)))
    (majutsu--handle-command-result cmd-args result
                                    "Clone completed"
                                    "Clone failed")))

(defun majutsu-git-init (args)
  "Initialize a new Git-backed jj repo. Prompts for DEST; uses ARGS."
  (interactive (list (transient-args 'majutsu-git-init-transient)))
  (let* ((dest (let ((d (read-directory-name "Destination (default .): " nil nil t)))
                 (if (and d (not (string-empty-p d))) (directory-file-name d) ".")))
         (git-repo (let ((arg (seq-find (lambda (a) (string-prefix-p "--git-repo=" a)) args)))
                     (when arg (substring arg (length "--git-repo=")))))
         (colocate? (member "--colocate" args))
         (no-colocate? (member "--no-colocate" args))
         (cmd-args (append '("git" "init")
                           (when colocate? '("--colocate"))
                           (when no-colocate? '("--no-colocate"))
                           (when git-repo (list "--git-repo" git-repo))
                           (list dest)))
         (result (apply #'majutsu--run-command cmd-args)))
    (majutsu--handle-command-result cmd-args result
                                    "Init completed"
                                    "Init failed")))

(defun majutsu-git-export ()
  "Update the underlying Git repo with changes made in the repo."
  (interactive)
  (let* ((cmd '("git" "export"))
         (result (apply #'majutsu--run-command cmd)))
    (majutsu--handle-command-result cmd result "Exported to Git" "Export failed")))

(defun majutsu-git-import ()
  "Update repo with changes made in the underlying Git repo."
  (interactive)
  (let* ((cmd '("git" "import"))
         (result (apply #'majutsu--run-command cmd)))
    (majutsu--handle-command-result cmd result "Imported from Git" "Import failed")))

(defun majutsu-git-root ()
  "Show the underlying Git directory of the current repository."
  (interactive)
  (let* ((dir (string-trim (majutsu--run-command "git" "root"))))
    (if (string-empty-p dir)
        (message "No underlying Git directory found")
      (kill-new dir)
      (message "Git root: %s (copied)" dir))))

(provide 'majutsu-commands)
;;; majutsu-commands.el ends here
