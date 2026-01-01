;;; majutsu-process.el --- Process handling for majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library runs jj commands synchronously and asynchronously,
;; integrating with with-editor and handling ANSI coloring.

;;; Code:

(require 'magit) ; for magit-with-editor
(require 'majutsu-base)
(require 'majutsu-mode)
(require 'majutsu-jj)
(require 'ansi-color)
(require 'subr-x)
(require 'with-editor nil 'noerror)

;;; Customization

(defgroup majutsu-process nil
  "Process execution helpers for Majutsu."
  :group 'majutsu)

(defcustom majutsu-process-color-mode 'always
  "How Majutsu asks `jj' to emit colors.

`always'  – force color by adding `--color=always'.
`auto'    – do not add flags; let jj auto-detect.
nil       – request no color."
  :type '(choice (const :tag "Always force color" always)
          (const :tag "Respect auto detection" auto)
          (const :tag "Disable color" nil))
  :group 'majutsu-process)

(defcustom majutsu-process-apply-ansi-colors t
  "When non-nil, convert ANSI escapes in jj output to text properties."
  :type 'boolean
  :group 'majutsu-process)

;;; Process buffer (Magit-style)

(defcustom majutsu-process-popup-time -1
  "Popup the process buffer if a command takes longer than this many seconds.

If -1, never popup.  If 0, popup immediately.  If a positive integer,
popup after that many seconds if the process is still running."
  :type '(choice (const :tag "Never" -1)
          (const :tag "Immediately" 0)
          (integer :tag "After this many seconds"))
  :group 'majutsu-process)

(defcustom majutsu-process-log-max 32
  "Maximum number of sections to keep in a process log buffer.

When adding a new section would go beyond the limit set here, then the
older half of the sections are removed.  Sections that belong to
processes that are still running are never removed.

When this is nil, no sections are ever removed."
  :type '(choice (const :tag "Never remove old sections" nil) integer)
  :group 'majutsu-process)

(defcustom majutsu-show-process-buffer-hint t
  "Whether to append a hint about the process buffer to JJ error messages."
  :type 'boolean
  :group 'majutsu-process)

(defcustom majutsu-process-timestamp-format nil
  "Format of timestamp for each process section in the process buffer.
When non-nil, pass this to `format-time-string' and insert the result in
the heading of each process section."
  :type '(choice (const :tag "None" nil) string)
  :group 'majutsu-process)

;;; Internal helpers

(defun majutsu--process--maybe-add-color (args)
  "Return ARGS with --color=always injected when configured.
Respects `majutsu-process-color-mode' and avoids duplication if caller
already supplied a color flag."
  (if (and (eq majutsu-process-color-mode 'always)
           (not (seq-some (lambda (arg) (string-prefix-p "--color" arg))
                          args)))
      (majutsu--ensure-flag args "--color=always" 'front)
    args))

(defun majutsu--process--maybe-add-no-pager (args)
  "Return ARGS with --no-pager injected.
Avoids duplication if caller already supplied a --no-pager flag."
  (majutsu--ensure-flag args "--no-pager" 'front))

(defun majutsu--process--maybe-add-flags (args)
  "Return ARGS with additional flags suited to majutsu-run-jj(-async)"
  (let ((clean (seq-remove #'null args)))
    (majutsu--process--maybe-add-color
     (majutsu--process--maybe-add-no-pager clean))))

(defun majutsu--process--apply-colors (output)
  "Apply ANSI color filtering to OUTPUT when enabled."
  (if (and output majutsu-process-apply-ansi-colors)
      (ansi-color-apply output)
    output))

;;; Process buffer

(defclass majutsu-process-section (magit-section)
  ((process :initform nil)))

(setf (alist-get 'process magit--section-type-alist) 'majutsu-process-section)

(defvar-keymap majutsu-process-mode-map
  :doc "Keymap for `majutsu-process-mode'."
  :parent majutsu-mode-map
  "g" #'undefined
  "k" #'majutsu-process-kill)

(define-derived-mode majutsu-process-mode majutsu-mode "Majutsu Process"
  "Mode for looking at jj process output."
  :interactive nil
  :group 'majutsu-process)

(defun majutsu-process-buffer (&optional nodisplay)
  "Display the current repository's process buffer.

If that buffer doesn't exist yet, then create it.  Non-interactively
return the buffer and unless optional NODISPLAY is non-nil also display
it."
  (interactive)
  (let* ((root (or (ignore-errors (majutsu--root))
                   (expand-file-name default-directory)))
         (name (format "*majutsu-process: %s*"
                       (abbreviate-file-name (directory-file-name root))))
         (buffer (or (majutsu--find-mode-buffer 'majutsu-process-mode root)
                     (get-buffer-create name))))
    (with-current-buffer buffer
      (setq-local majutsu--repo-root root)
      (setq default-directory root)
      (if magit-root-section
          (when majutsu-process-log-max
            (majutsu--process-truncate-log))
        (majutsu-process-mode)
        (let ((inhibit-read-only t)
              (magit-insert-section--parent nil)
              (magit-insert-section--oldroot nil))
          (make-local-variable 'text-property-default-nonsticky)
          (magit-insert-section (processbuf)
            (insert "\n")))))
    (unless nodisplay
      (majutsu-display-buffer buffer 'message))
    buffer))

(defun majutsu-process-kill ()
  "Kill the process at point."
  (interactive)
  (when-let ((process (magit-section-value-if 'process)))
    (unless (eq (process-status process) 'run)
      (user-error "Process isn't running"))
    (kill-process process)))

(defun majutsu--process--format-arguments (program args pwd)
  (let ((prefix (and (not (equal
                           (file-name-as-directory (expand-file-name pwd))
                           (file-name-as-directory (expand-file-name default-directory))))
                     (concat (file-relative-name pwd default-directory) " "))))
    (concat prefix
            (file-name-nondirectory program)
            (and args " ")
            (mapconcat #'shell-quote-argument args " "))))

(defun majutsu--process-insert-section
    (pwd program args &optional errcode errlog face)
  (let ((inhibit-read-only t)
        (magit-insert-section--current nil)
        (magit-insert-section--parent magit-root-section)
        (magit-insert-section--oldroot nil))
    (goto-char (1- (point-max)))
    (magit-insert-section (process)
      (insert (if errcode
                  (format "%3s " (propertize (number-to-string errcode)
                                             'font-lock-face 'magit-process-ng))
                "run "))
      (when majutsu-process-timestamp-format
        (insert (format-time-string majutsu-process-timestamp-format) " "))
      (let ((cmd (majutsu--process--format-arguments program args pwd)))
        (magit-insert-heading
          (if face
              (propertize cmd 'face face)
            cmd)))
      (when errlog
        (if (bufferp errlog)
            (insert (with-current-buffer errlog
                      (buffer-substring-no-properties (point-min) (point-max))))
          (insert-file-contents errlog)
          (goto-char (1- (point-max)))))
      (insert "\n"))))

(defun majutsu--process-truncate-log ()
  (let* ((head nil)
         (tail (oref magit-root-section children))
         (count (length tail)))
    (when (and (integerp majutsu-process-log-max)
               (> (1+ count) majutsu-process-log-max))
      (while (and (cdr tail)
                  (> count (/ majutsu-process-log-max 2)))
        (let* ((inhibit-read-only t)
               (section (car tail))
               (process (oref section process)))
          (cond
           ((not process))
           ((memq (process-status process) '(exit signal))
            (delete-region (oref section start)
                           (1+ (oref section end)))
            (cl-decf count))
           (t (push section head))))
        (pop tail))
      (oset magit-root-section children
            (nconc (reverse head) tail)))))

(defvar majutsu-process-error-message-regexps
  (list "^\\*ERROR\\*: \\(.*\\)$"
        "^\\(?:Error\\|error\\): \\(.*\\)$"
        "^\\(?:fatal\\): \\(.*\\)$")
  "Regexps used to extract a one-line error summary from jj output.")

(defun majutsu--process-error-summary (process-buf section)
  "Return a one-line error summary from SECTION in PROCESS-BUF."
  (and (buffer-live-p process-buf)
       (with-current-buffer process-buf
         (and (oref section content)
              (save-excursion
                (goto-char (oref section end))
                (catch 'found
                  (dolist (re majutsu-process-error-message-regexps)
                    (when-let ((match (save-excursion
                                        (when (re-search-backward re (oref section start) t)
                                          (string-trim (match-string 1))))))
                      (throw 'found match)))
                  nil))))))

(defun majutsu--process-error-summary-from-string (output)
  "Return a one-line error summary extracted from OUTPUT."
  (when (and (stringp output) (not (string-empty-p output)))
    (with-temp-buffer
      (insert output)
      (goto-char (point-max))
      (catch 'found
        (dolist (re majutsu-process-error-message-regexps)
          (when-let ((match (save-excursion
                              (when (re-search-backward re nil t)
                                (string-trim (match-string 1))))))
            (throw 'found match)))
        nil))))

(defun majutsu--process-section-output (process)
  "Return the complete output for PROCESS from the process buffer."
  (when-let* ((buf (process-buffer process))
              (section (process-get process 'section))
              (_ (buffer-live-p buf)))
    (with-current-buffer buf
      (let* ((beg (oref section content))
             (end (oref section end)))
        (cond
         ((and beg end)
          (string-trim-right
           (buffer-substring-no-properties beg end)))
         (t ""))))))

(defun majutsu--process-finish-section (section exit-code)
  (let ((inhibit-read-only t)
        (buffer (current-buffer))
        (marker (oref section start)))
    (goto-char marker)
    (save-excursion
      (delete-char 3)
      (set-marker-insertion-type marker nil)
      (insert (propertize (format "%3s" exit-code)
                          'magit-section section
                          'font-lock-face (if (= exit-code 0)
                                              'magit-process-ok
                                            'magit-process-ng)))
      (set-marker-insertion-type marker t))
    (when (and majutsu-process-apply-ansi-colors
               (oref section content))
      (ansi-color-apply-on-region (oref section content)
                                  (oref section end)))
    (cond
     ((= (oref section end)
         (+ (line-end-position) 2))
      (save-excursion
        (goto-char (1+ (line-end-position)))
        (delete-char -1)
        (oset section content nil)))
     ((and (= exit-code 0)
           (not (seq-some (lambda (window)
                            (eq (window-buffer window) buffer))
                          (window-list))))
      (magit-section-hide section)))))

(defun majutsu--process--error-usage (process-buf)
  (and majutsu-show-process-buffer-hint
       (if-let ((keys (where-is-internal 'majutsu-process-buffer)))
           (format "Type %s to see %S for details"
                   (key-description (car keys)) process-buf)
         (format "See %S for details" process-buf))))

(defun majutsu--process-finish (arg &optional process-buf command-buf default-dir section)
  "Finalize a jj process log SECTION.
ARG may be a process object or an exit code.  Return the exit code."
  (let ((process (unless (integerp arg) arg))
        exit-code)
    (unless (integerp arg)
      (setq process-buf (process-buffer arg))
      (setq command-buf (process-get arg 'command-buf))
      (setq default-dir (process-get arg 'default-dir))
      (setq section     (process-get arg 'section))
      (setq exit-code   (process-exit-status arg)))
    (when (integerp arg)
      (setq exit-code arg))

    (when (and (buffer-live-p process-buf) section (integerp exit-code))
      (with-current-buffer process-buf
        (majutsu--process-finish-section section exit-code)))

    (cond
     ((and (integerp exit-code) (= exit-code 0))
      (when-let ((success-msg (and process (process-get process 'success-msg))))
        (message "%s" success-msg))
      (when-let ((cb (and process (process-get process 'finish-callback))))
        (funcall cb process exit-code)))
     ((integerp exit-code)
      (let* ((msg (majutsu--process-error-summary process-buf section))
             (usage (majutsu--process--error-usage process-buf))
             (root (and default-dir
                        (with-temp-buffer
                          (setq default-directory default-dir)
                          (ignore-errors (majutsu--root))))))
        (when-let ((log-buf (and root (majutsu--find-mode-buffer 'majutsu-log-mode root))))
          (with-current-buffer log-buf
            (setq-local majutsu-log--this-error (or msg "Command failed"))))
        (message "jj error: %s%s"
                 (or msg "Command failed")
                 (and usage (format " [%s]" usage))))))

    (when-let ((cb (and process (process-get process 'finish-callback))))
      (unless (and (integerp exit-code) (= exit-code 0))
        (funcall cb process exit-code)))
    exit-code))

(defun majutsu--process-display-buffer (process)
  (when (process-live-p process)
    (let ((buf (process-buffer process)))
      (cond
       ((not (buffer-live-p buf)))
       ((= majutsu-process-popup-time 0)
        (if (minibufferp)
            (switch-to-buffer-other-window buf)
          (majutsu-display-buffer buf 'message)))
       ((> majutsu-process-popup-time 0)
        (run-with-timer majutsu-process-popup-time nil
                        (lambda (p)
                          (when-let* ((_(eq (process-status p) 'run))
                                      (b (process-buffer p))
                                      (_(buffer-live-p b)))
                            (if (minibufferp)
                                (switch-to-buffer-other-window b)
                              (majutsu-display-buffer b 'message))))
                        process))))))

(defun majutsu--process-filter (proc string)
  "Default filter used by `majutsu-start-jj'."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (process-mark proc))
      ;; Find last ^M in STRING.  If one was found, ignore everything
      ;; before it and delete the current line.
      (when-let ((ret-pos (cl-position ?\r string :from-end t)))
        (setq string (substring string (1+ ret-pos)))
        (delete-region (line-beginning-position) (point)))
      (insert (propertize string 'magit-section
                          (process-get proc 'section)))
      (set-marker (process-mark proc) (point)))))

(defun majutsu--process-sentinel (process _event)
  "Default sentinel used by `majutsu-start-jj'."
  (when (memq (process-status process) '(exit signal))
    (majutsu--process-finish process)
    (unless (process-get process 'inhibit-refresh)
      (let ((command-buf (process-get process 'command-buf))
            (default-dir (process-get process 'default-dir)))
        (if (buffer-live-p command-buf)
            (with-current-buffer command-buf
              (let ((default-directory (or default-dir default-directory)))
                (majutsu-refresh)))
          (with-temp-buffer
            (setq default-directory (or default-dir default-directory))
            (majutsu-refresh)))))))

(defun majutsu-start-jj (args &optional success-msg finish-callback)
  "Run jj ARGS asynchronously for side-effects and log output.

Return the process object.

SUCCESS-MSG is displayed on exit code 0.  When FINISH-CALLBACK is
non-nil, call it as (FINISH-CALLBACK PROCESS EXIT-CODE) after the
process terminates."
  (let* ((args (majutsu--process--maybe-add-flags (flatten-tree args)))
         (pwd default-directory)
         (default-directory (or (ignore-errors (majutsu--root)) default-directory))
         (process-buf (majutsu-process-buffer t))
         (section (with-current-buffer process-buf
                    (prog1 (majutsu--process-insert-section pwd majutsu-jj-executable args nil nil)
                      (backward-char 1))))
         (process (apply #'start-file-process "majutsu-jj" process-buf majutsu-jj-executable args)))
    (set-process-query-on-exit-flag process nil)
    (process-put process 'section section)
    (process-put process 'command-buf (current-buffer))
    (process-put process 'default-dir default-directory)
    (when success-msg
      (process-put process 'success-msg success-msg))
    (when finish-callback
      (process-put process 'finish-callback finish-callback))
    (oset section process process)
    (oset section value process)
    (with-current-buffer process-buf
      (set-marker (process-mark process) (point)))
    (if (fboundp 'with-editor-set-process-filter)
        (with-editor-set-process-filter process #'majutsu--process-filter)
      (set-process-filter process #'majutsu--process-filter))
    (set-process-sentinel process #'majutsu--process-sentinel)
    (majutsu--process-display-buffer process)
    process))

(defun majutsu-call-jj (&rest args)
  "Call jj synchronously in a separate process, for side-effects.

Process output goes into a new section in the buffer returned by
`majutsu-process-buffer'.  Return the exit code."
  (setq args (flatten-tree args))
  (let* ((args (majutsu--process--maybe-add-flags args))
         (pwd default-directory)
         (default-directory (or (ignore-errors (majutsu--root)) default-directory)))
    (pcase-let* ((`(,process-buf . ,section)
                  (let ((buf (majutsu-process-buffer t)))
                    (cons buf (with-current-buffer buf
                                (prog1 (majutsu--process-insert-section pwd majutsu-jj-executable args nil nil)
                                  (backward-char 1))))))
                 (inhibit-read-only t)
                 (exit (apply #'process-file majutsu-jj-executable nil process-buf nil args)))
      (setq exit (majutsu--process-finish exit process-buf (current-buffer) default-directory section))
      (majutsu-refresh)
      exit)))

(defun majutsu-run-jj (&rest args)
  "Run jj command with ARGS and return output."
  (let* ((start-time (current-time))
         (safe-args (majutsu--process--maybe-add-flags args))
         result exit-code)
    (majutsu--debug "Running command: %s %s" majutsu-jj-executable (string-join safe-args " "))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix)
            (coding-system-for-write 'utf-8-unix))
        (setq exit-code (apply #'process-file majutsu-jj-executable nil t nil safe-args)))
      (setq result (majutsu--process--apply-colors (buffer-string)))
      (majutsu--debug "Command completed in %.3f seconds, exit code: %d"
                      (float-time (time-subtract (current-time) start-time))
                      exit-code)
      (when (and majutsu-show-command-output (not (string-empty-p result)))
        (majutsu--debug "Command output: %s" (string-trim result)))
      result)))

(defun majutsu-run-jj-async (args callback &optional error-callback)
  "Run jj command with ARGS asynchronously.
CALLBACK is called with the output string on success.
ERROR-CALLBACK is called with the error output on failure."
  (let* ((default-directory (majutsu--root))
         (args (majutsu--process--maybe-add-flags args))
         (buffer (generate-new-buffer " *majutsu-async*"))
         (process (apply #'start-file-process "majutsu-async" buffer majutsu-jj-executable args)))
    (set-process-sentinel process
                          (lambda (proc _event)
                            (let ((status (process-status proc)))
                              (when (memq status '(exit signal))
                                (let ((output (with-current-buffer buffer (buffer-string)))
                                      (exit-code (process-exit-status proc)))
                                  (kill-buffer buffer)
                                  (if (zerop exit-code)
                                      (funcall callback (majutsu--process--apply-colors output))
                                    (if error-callback
                                        (funcall error-callback output)
                                      (message "Majutsu async error: %s" output))))))))
    process))

(defmacro majutsu-with-progress (msg &rest body)
  "Execute BODY with minimal progress indication using MESSAGE."
  `(let ((start-time (current-time))
         result)
     (majutsu--debug "Starting operation: %s" ,msg)
     (setq result (progn ,@body))
     (majutsu--debug "Operation completed in %.3f seconds"
                     (float-time (time-subtract (current-time) start-time)))
     result))

;;; with-editor integration

(eval-when-compile
  (require 'with-editor nil 'noerror)
  (declare-function with-editor--setup "with-editor" ()))

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
                    (majutsu--display-function 'message))
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
      (majutsu-display-buffer buffer 'message))))
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
         process)
    (majutsu--with-editor--queue-visit visit-hook)
    (condition-case err
        (majutsu-with-editor
          (when-let* ((editor (getenv majutsu-with-editor-envvar)))
            (setq editor (majutsu--with-editor--normalize-editor editor))
            (setenv majutsu-with-editor-envvar editor)
            (setenv "EDITOR" editor))
          (setq process
                (majutsu-start-jj
                 args success-msg
                 (lambda (_process exit-code)
                   (when (and (zerop exit-code) success-callback)
                     (funcall success-callback))))))
      (error
       (majutsu--with-editor--cancel-visit visit-hook)
       (signal (car err) (cdr err))))
    (majutsu--message-with-log "Launching jj %s (edit in current Emacs)..."
                               (string-join args " "))
    process))

(defun majutsu--wash (washer keep-error &rest args)
  "Run jj with ARGS, insert output at point, then call WASHER.
KEEP-ERROR matches `magit--git-wash': nil drops stderr on error,
`wash-anyway' keeps output even on non-zero exit, anything else keeps the
error text.  Output is optionally colorized based on
`majutsu-process-apply-ansi-colors'."
  (declare (indent 2))
  (setq args (flatten-tree args))
  (setq args (majutsu--process--maybe-add-flags args))
  (let ((beg (point))
        (exit (apply #'process-file majutsu-jj-executable nil t nil args)))
    (when (and majutsu-process-apply-ansi-colors
               (> (point) beg))
      (ansi-color-apply-on-region beg (point)))
    ;; `process-file' may return nil on success for some Emacs builds.
    (when (and (not exit) (integerp exit))
      (setq exit 0))
    (cond
     ;; Command produced no output.
     ((= (point) beg)
      (insert (propertize "(No diff)" 'face 'shadow)))
     ;; Failure path.
     ((and (not (eq keep-error 'wash-anyway))
           (not (= exit 0)))
      (goto-char beg)
      (insert (propertize (format "jj %s failed (exit %s)\n"
                                  (string-join args " ") exit)
                          'font-lock-face 'error)))
     ;; Success (or wash anyway).
     (t
      (save-restriction
        (narrow-to-region beg (point))
        (goto-char beg)
        (funcall washer args))
      (when (or (= (point) beg)
                (= (point) (1+ beg)))
        (magit-cancel-section))))
    exit))

;;; _
(provide 'majutsu-process)
;;; majutsu-process.el ends here
