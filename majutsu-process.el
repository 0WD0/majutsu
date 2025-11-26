;;; majutsu-process.el --- Process handling for majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Process execution and with-editor integration for Majutsu.

;;; Code:

(require 'magit) ; for magit-with-editor
(require 'majutsu-base)
(require 'ansi-color)
(require 'subr-x)
(require 'with-editor nil 'noerror)

(defun majutsu--run-command (&rest args)
  "Run jj command with ARGS and return output."
  (let ((start-time (current-time))
        (safe-args (seq-remove #'null args))
        result exit-code)
    (majutsu--debug "Running command: %s %s" majutsu-executable (string-join safe-args " "))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix)
            (coding-system-for-write 'utf-8-unix))
        (setq exit-code (apply #'process-file majutsu-executable nil t nil safe-args)))
      (setq result (buffer-string))
      (majutsu--debug "Command completed in %.3f seconds, exit code: %d"
                      (float-time (time-subtract (current-time) start-time))
                      exit-code)
      (when (and majutsu-show-command-output (not (string-empty-p result)))
        (majutsu--debug "Command output: %s" (string-trim result)))
      result)))

(defun majutsu--run-command-color (&rest args)
  "Run jj command with ARGS and return colorized output."
  (majutsu--debug "Running color command: %s --color=always %s" majutsu-executable (string-join args " "))
  (let ((start-time (current-time))
        result exit-code)
    (with-temp-buffer
      (let ((process-environment (cons "FORCE_COLOR=1" (cons "CLICOLOR_FORCE=1" process-environment))))
        (let ((coding-system-for-read 'utf-8-unix)
              (coding-system-for-write 'utf-8-unix))
          (setq exit-code (apply #'process-file majutsu-executable nil t nil "--color=always" args)))
        (setq result (ansi-color-apply (buffer-string)))
        (majutsu--debug "Color command completed in %.3f seconds, exit code: %d"
                        (float-time (time-subtract (current-time) start-time))
                        exit-code)
        result))))

(defun majutsu--run-command-async (args callback &optional error-callback color)
  "Run jj command with ARGS asynchronously.
CALLBACK is called with the output string on success.
ERROR-CALLBACK is called with the error output on failure.
If COLOR is non-nil, force color output."
  (let* ((default-directory (majutsu--root))
         (buffer (generate-new-buffer " *majutsu-async*"))
         (process-environment (if color
                                  (cons "FORCE_COLOR=1" (cons "CLICOLOR_FORCE=1" process-environment))
                                process-environment))
         (process (apply #'start-file-process "majutsu-async" buffer majutsu-executable args)))
    (set-process-sentinel process
                          (lambda (proc _event)
                            (let ((status (process-status proc)))
                              (when (memq status '(exit signal))
                                (let ((output (with-current-buffer buffer (buffer-string)))
                                      (exit-code (process-exit-status proc)))
                                  (kill-buffer buffer)
                                  (if (zerop exit-code)
                                      (progn
                                        (when color
                                          (setq output (ansi-color-apply output)))
                                        (funcall callback output))
                                    (if error-callback
                                        (funcall error-callback output)
                                      (message "Majutsu async error: %s" output))))))))
    process))

(defun majutsu--with-progress (message command-func)
  "Execute COMMAND-FUNC with minimal progress indication."
  (let ((start-time (current-time))
        result)
    (majutsu--debug "Starting operation: %s" message)
    (setq result (funcall command-func))
    (majutsu--debug "Operation completed in %.3f seconds"
                    (float-time (time-subtract (current-time) start-time)))
    result))

(defun majutsu--handle-command-result (command-args result &optional success-msg _error-msg)
  "Handle command result with proper error checking and messaging."
  (let ((trimmed-result (string-trim result))
        (command-name (car command-args)))
    (majutsu--debug "Command result for '%s': %s"
                    (string-join command-args " ")
                    trimmed-result)

    ;; Always show command output if it exists (like CLI)
    (unless (string-empty-p trimmed-result)
      (message "%s" trimmed-result))

    (cond
     ;; Check for various error indicators
     ((or (string-match-p "^Error:\\|^error:" trimmed-result)
          (string-match-p "^Warning:\\|^warning:" trimmed-result)
          (string-match-p "^fatal:" trimmed-result))

      ;; Provide majutsu-specific contextual suggestions
      (cond
       ;; Working copy issues
       ((string-match-p "working copy is stale\\|concurrent modification" trimmed-result)
        (message "💡 Run 'jj workspace update-stale' to fix the working copy"))

       ;; Conflict resolution needed
       ((string-match-p "merge conflict\\|conflict in" trimmed-result)
        (message "💡 Resolve conflicts manually, then run 'jj resolve' or use diffedit (E/M)"))

       ;; Revision not found
       ((string-match-p "No such revision\\|revision.*not found" trimmed-result)
        (message "💡 Check the revision ID or refresh the log (g)"))

       ;; Empty commit issues
       ((string-match-p "nothing to squash\\|would be empty" trimmed-result)
        (message "💡 Select a different commit with actual changes"))

       ;; Rebase loop detection
       ((string-match-p "would create a loop\\|circular dependency" trimmed-result)
        (message "💡 Check your rebase source and destinations for cycles"))

       ;; Authentication/permission issues
       ((string-match-p "authentication\\|permission denied" trimmed-result)
        (message "💡 Check your git credentials and repository access"))

       ;; Generic suggestion for other errors
       (t
        (message "💡 Check 'jj help %s' for more information" command-name)))
      nil)

     ;; Success case
     (t
      (when (and success-msg (string-empty-p trimmed-result))
        (message "%s" success-msg))
      t))))

;;; with-editor integration

(eval-when-compile
  (require 'with-editor nil 'noerror)
  (declare-function with-editor--setup "with-editor" ()))
(defvar with-editor-emacsclient-executable)
(defvar with-editor-filter-visit-hook)
(defvar with-editor--envvar)
(defvar with-editor-envvars)
(defvar with-editor-server-window-alist)

(defun majutsu--with-editor--normalize-editor (command)
  "Normalize editor COMMAND before exporting it to jj.
On Windows the jj launcher expects a bare executable path, so strip the
outer shell quoting that `with-editor' adds, matching Magit's workaround."
  (let ((trimmed (string-trim (or command ""))))
    (if (and (eq system-type 'windows-nt)
             (string-match "\\`\"\\([^\"]+\\)\"\\(.*\\)\\'" trimmed))
        (concat (match-string 1 trimmed)
                (match-string 2 trimmed))
      trimmed)))

(defun majutsu--with-editor-available-p ()
  "Return non-nil when `with-editor' can be used for JJ commands."
  (and (require 'with-editor nil 'noerror)
       with-editor-emacsclient-executable))

(defconst majutsu--with-editor-description-regexp
  (rx (seq (or string-start
               (seq (* (not (any ?\n)))
                    (any ?/ ?\\)))
           "editor-" (+ (in "0-9A-Za-z"))
           ".jjdescription" string-end))
  "Regexp matching temporary jj description files created for editing.")

(defun majutsu--with-editor-ensure-setup ()
  "Ensure with-editor integration specific to Majutsu is configured."
  (when (majutsu--with-editor-available-p)
    (when (boundp 'with-editor-envvars)
      (cl-pushnew majutsu-with-editor-envvar with-editor-envvars :test #'equal))
    (when (boundp 'with-editor-server-window-alist)
      (unless (cl-assoc majutsu--with-editor-description-regexp
                        with-editor-server-window-alist
                        :test #'string=)
        (push (cons majutsu--with-editor-description-regexp
                    (or majutsu-message-display-function #'pop-to-buffer))
              with-editor-server-window-alist)))
    (when (boundp 'with-editor-filter-visit-hook)
      (unless (memq #'majutsu--with-editor--apply-visit with-editor-filter-visit-hook)
        (add-hook 'with-editor-filter-visit-hook #'majutsu--with-editor--apply-visit)))
    (when (require 'server nil 'noerror)
      (unless (memq #'majutsu--with-editor--apply-visit server-visit-hook)
        (add-hook 'server-visit-hook #'majutsu--with-editor--apply-visit))
      (unless (memq #'majutsu--with-editor--apply-visit server-switch-hook)
        (add-hook 'server-switch-hook #'majutsu--with-editor--apply-visit)))))

(defvar majutsu--with-editor-visit-queue nil
  "Queue of pending initializer functions for Majutsu with-editor buffers.")

(defun majutsu--with-editor--queue-visit (fn)
  "Enqueue FN so it runs when the next Majutsu editor buffer opens."
  (push fn majutsu--with-editor-visit-queue)
  fn)

(defun majutsu--with-editor--cancel-visit (fn)
  "Remove FN from the pending with-editor queue."
  (setq majutsu--with-editor-visit-queue
        (delq fn majutsu--with-editor-visit-queue)))

(defun majutsu--with-editor--apply-visit ()
  "Run the next pending Majutsu with-editor initializer if applicable."
  (let ((next '())
        handled)
    (dolist (fn majutsu--with-editor-visit-queue)
      (if (and (not handled)
               (with-demoted-errors "Majutsu with-editor init failed: %S"
                 (funcall fn)))
          (setq handled t)
        (push fn next)))
    (setq majutsu--with-editor-visit-queue (nreverse next))
    handled))

(defun majutsu--with-editor--target-buffer-p ()
  "Return non-nil when current buffer is a jj temporary editor file."
  (and buffer-file-name
       (string-match-p majutsu--with-editor-description-regexp buffer-file-name)))

(defvar-local majutsu--with-editor-return-window nil
  "Window to restore after finishing a Majutsu with-editor session.")

(defvar-local majutsu--with-editor-return-buffer nil
  "Buffer to restore after finishing a Majutsu with-editor session.")

(defun majutsu--with-editor--restore-context ()
  "Restore window focus and buffer after a Majutsu with-editor session."
  (let ((window majutsu--with-editor-return-window)
        (buffer majutsu--with-editor-return-buffer))
    (cond
     ((and (window-live-p window)
           (buffer-live-p buffer))
      (with-selected-window window
        (switch-to-buffer buffer)))
     ((buffer-live-p buffer)
      (majutsu--display-buffer-for-editor buffer))))
  (remove-hook 'with-editor-post-finish-hook #'majutsu--with-editor--restore-context t)
  (remove-hook 'with-editor-post-cancel-hook #'majutsu--with-editor--restore-context t))

(defun majutsu--with-editor--visit-hook (window buffer)
  "Return initializer enabling `with-editor-mode' and tracking WINDOW/BUFFER."
  (let ((done nil))
    (lambda ()
      (when (and (not done)
                 (majutsu--with-editor--target-buffer-p))
        (setq done t)
        (when (fboundp 'with-editor-mode)
          (with-editor-mode 1))
        (goto-char (point-min))
        (majutsu--with-editor--setup-return window buffer)
        t))))

(defun majutsu--with-editor--setup-return (window buffer)
  "Remember WINDOW and BUFFER for restoring after the editor closes."
  (setq-local majutsu--with-editor-return-window window)
  (setq-local majutsu--with-editor-return-buffer buffer)
  (add-hook 'with-editor-post-finish-hook #'majutsu--with-editor--restore-context nil t)
  (add-hook 'with-editor-post-cancel-hook #'majutsu--with-editor--restore-context nil t))

(defmacro majutsu-with-editor (&rest body)
  "Ensure BODY runs with the correct editor environment for jj."
  (declare (indent 0) (debug (body)))
  `(let ((magit-with-editor-envvar majutsu-with-editor-envvar))
     (magit-with-editor
       ,@body)))

(defun majutsu--with-editor--sentinel (args success-msg error-msg success-callback)
  "Return sentinel handling JJ command completion.
ARGS, SUCCESS-MSG, ERROR-MSG mirror `majutsu--handle-command-result'.
SUCCESS-CALLBACK, when non-nil, is invoked after a successful command."
  (lambda (process _event)
    (let ((status (process-status process))
          (buffer (process-buffer process)))
      (when (and buffer (memq status '(exit signal)))
        (let ((output (with-current-buffer buffer
                        (ansi-color-filter-apply (buffer-string))))
              (exit-code (process-exit-status process)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer))
          (if (and (eq status 'exit) (zerop exit-code))
              (when (majutsu--handle-command-result args output success-msg nil)
                ;; We need to handle log refresh here, but majutsu-log-refresh is in majutsu-log.el
                ;; We can use a hook or check if fboundp
                (when (fboundp 'majutsu-log-refresh)
                  (funcall 'majutsu-log-refresh))
                (when success-callback
                  (funcall success-callback)))
            (majutsu--handle-command-result args output nil
                                            (or error-msg "Command failed"))))))))

(defun majutsu--with-editor-run (args success-msg error-msg &optional success-callback)
  "Run JJ ARGS using with-editor.
On success, display SUCCESS-MSG and refresh the log; otherwise use ERROR-MSG."
  (unless (majutsu--with-editor-available-p)
    (user-error "with-editor is not available in this Emacs"))
  (majutsu--with-editor-ensure-setup)
  (let* ((default-directory (majutsu--root))
         (process-environment (copy-sequence process-environment))
         (origin-window (selected-window))
         (origin-buffer (current-buffer))
         (visit-hook (majutsu--with-editor--visit-hook origin-window origin-buffer))
         process buffer)
    (majutsu--with-editor--queue-visit visit-hook)
    (condition-case err
        (majutsu-with-editor
          (setq buffer (generate-new-buffer " *majutsu-jj*"))
          (when-let* ((editor (getenv majutsu-with-editor-envvar)))
            (setq editor (majutsu--with-editor--normalize-editor editor))
            (setenv majutsu-with-editor-envvar editor)
            (setenv "EDITOR" editor))
          (setq process (apply #'start-file-process "majutsu-jj"
                               buffer majutsu-executable args)))
      (error
       (majutsu--with-editor--cancel-visit visit-hook)
       (signal (car err) (cdr err))))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (majutsu--with-editor--sentinel args success-msg error-msg success-callback))
    (majutsu--message-with-log "Launching jj %s (edit in current Emacs)..."
                               (string-join args " "))
    process))

(provide 'majutsu-process)
;;; majutsu-process.el ends here
