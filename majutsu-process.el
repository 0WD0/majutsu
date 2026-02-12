;;; majutsu-process.el --- Process handling for majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025-2026 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Portions of process buffer orchestration are adapted from:
;; - Magit `lisp/magit-process.el` (commit c800f79c2061621fde847f6a53129eca0e8da728)
;;   Copyright (C) 2008-2026 The Magit Project Contributors

;;; Commentary:

;; This library runs jj commands synchronously and asynchronously,
;; integrating with with-editor and handling ANSI coloring.

;;; Code:

(require 'majutsu-base)
(require 'majutsu-mode)
(require 'majutsu-jj)
(require 'ansi-color)
(require 'subr-x)
(require 'with-editor)

(require 'magit-git) ; for magit-with-editor
(require 'magit-section)
(require 'magit-process) ; for prompt functions

;;; Customization

(defgroup majutsu-process nil
  "Process execution helpers for Majutsu."
  :group 'majutsu)

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
  (let* ((root (or (majutsu--buffer-root)
                   (majutsu-toplevel default-directory)
                   (majutsu--toplevel-safe default-directory)))
         (name (format "*majutsu-process: %s*"
                       (abbreviate-file-name (directory-file-name root))))
         (buffer (or (majutsu--find-mode-buffer 'majutsu-process-mode root)
                     (get-buffer-create name))))
    (with-current-buffer buffer
      (setq majutsu--default-directory root)
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
      (majutsu-display-buffer buffer))
    buffer))

(defun majutsu-process-kill ()
  "Kill the process at point."
  (interactive)
  (when-let* ((process (magit-section-value-if 'process)))
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
                    (when-let* ((match (save-excursion
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
          (when-let* ((match (save-excursion
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
       (if-let* ((keys (where-is-internal 'majutsu-process-buffer)))
           (format "Type %s to see %S for details"
                   (key-description (car keys)) process-buf)
         (format "See %S for details" process-buf))))

(defun majutsu-process-finish (arg &optional process-buf _command-buf default-dir section)
  "Finalize a jj process log SECTION.
ARG may be a process object or an exit code.  Return the exit code."
  (let ((process (unless (integerp arg) arg))
        exit-code)
    (unless (integerp arg)
      (setq process-buf (process-buffer arg))
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
      (when-let* ((success-msg (and process (process-get process 'success-msg))))
        (message "%s" success-msg))
      (when-let* ((cb (and process (process-get process 'finish-callback))))
        (funcall cb process exit-code)))
     ((integerp exit-code)
      (let* ((msg (majutsu--process-error-summary process-buf section))
             (usage (majutsu--process--error-usage process-buf))
             (root default-dir))
        (when-let* ((log-buf (and root (majutsu--find-mode-buffer 'majutsu-log-mode root))))
          (with-current-buffer log-buf
            (setq-local majutsu-log--this-error (or msg "Command failed"))))
        (message "jj error: %s%s"
                 (or msg "Command failed")
                 (and usage (format " [%s]" usage))))))

    (when-let* ((cb (and process (process-get process 'finish-callback))))
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
          (pop-to-buffer buf)))
       ((> majutsu-process-popup-time 0)
        (run-with-timer majutsu-process-popup-time nil
                        (lambda (p)
                          (when-let* ((_(eq (process-status p) 'run))
                                      (b (process-buffer p))
                                      (_(buffer-live-p b)))
                            (if (minibufferp)
                                (switch-to-buffer-other-window b)
                              (pop-to-buffer b))))
                        process))))))

(defun majutsu-start-process (program &optional input &rest args)
  "Start PROGRAM asynchronously, preparing for refresh, and return the process.

PROGRAM is started using `start-file-process' and then setup to use
`majutsu--process-sentinel' and `majutsu--process-filter'.  After the
process terminates, the sentinel refreshes the buffer that was current
when this function was called (if still alive), as well as the
repository's log buffer (see `majutsu-refresh')."
  (let* ((args (flatten-tree args))
         (pwd default-directory)
         (process-buf (let ((default-directory pwd))
                        (majutsu-process-buffer t)))
         (root (with-current-buffer process-buf default-directory))
         (section (with-current-buffer process-buf
                    (prog1 (majutsu--process-insert-section pwd program args nil nil)
                      (backward-char 1))))
         (process (apply #'start-file-process (file-name-nondirectory program)
                         process-buf program args)))
    (set-process-query-on-exit-flag process nil)
    (process-put process 'section section)
    (process-put process 'command-buf (current-buffer))
    (process-put process 'default-dir root)
    (oset section process process)
    (oset section value process)
    (with-current-buffer process-buf
      (set-marker (process-mark process) (point)))
    (if (fboundp 'with-editor-set-process-filter)
        (with-editor-set-process-filter process #'majutsu--process-filter)
      (set-process-filter process #'majutsu--process-filter))
    (set-process-sentinel process #'majutsu--process-sentinel)
    (when input
      (with-current-buffer input
        (process-send-region process (point-min) (point-max))
        (process-send-eof process)))
    (majutsu--process-display-buffer process)
    process))

(defun majutsu--process-filter (proc string)
  "Default filter used by `majutsu-start-process'."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (process-mark proc))
      ;; Find last ^M in STRING.  If one was found, ignore everything
      ;; before it and delete the current line.
      (when-let* ((ret-pos (cl-position ?\r string :from-end t)))
        (setq string (substring string (1+ ret-pos)))
        (delete-region (line-beginning-position) (point)))
      (insert (propertize string 'magit-section
                          (process-get proc 'section)))
      (magit-process-yes-or-no-prompt proc string)
      (magit-process-username-prompt proc string)
      (magit-process-password-prompt proc string)
      (run-hook-with-args-until-success 'magit-process-prompt-functions
                                        proc string)
      (set-marker (process-mark proc) (point)))))

(defun majutsu--process-sentinel (process _event)
  "Default sentinel used by `majutsu-start-process'."
  (when (memq (process-status process) '(exit signal))
    (majutsu-process-finish process)
    (unless (process-get process 'inhibit-refresh)
      (let ((command-buf (process-get process 'command-buf))
            (default-dir (process-get process 'default-dir)))
        (if (buffer-live-p command-buf)
            (with-current-buffer command-buf
              (let ((default-directory (or default-dir default-directory)))
                (majutsu-refresh)))
          (when (and default-dir (fboundp 'majutsu-log-refresh))
            (when-let* ((buffer (majutsu--find-mode-buffer 'majutsu-log-mode default-dir)))
              (with-current-buffer buffer
                (ignore-errors (majutsu-log-refresh))))))))))

(defun majutsu-start-jj (args &optional success-msg finish-callback)
  "Run jj ARGS asynchronously for side-effects and log output.

Return the process object.

SUCCESS-MSG is displayed on exit code 0.  When FINISH-CALLBACK is
non-nil, call it as (FINISH-CALLBACK PROCESS EXIT-CODE) after the
process terminates."
  (let* ((default-directory (majutsu--toplevel-safe default-directory))
         (jj (majutsu-jj--executable))
         (args (majutsu-process-jj-arguments args))
         (process (apply #'majutsu-start-process jj nil args)))
    (when success-msg
      (process-put process 'success-msg success-msg))
    (when finish-callback
      (process-put process 'finish-callback finish-callback))
    process))

(defun majutsu-call-jj (&rest args)
  "Call jj synchronously in a separate process, for side-effects.

Process output goes into a new section in the buffer returned by
`majutsu-process-buffer'.  Return the exit code.

Unlike `majutsu-start-jj', this does not implicitly refresh any Majutsu
buffers.  Call `majutsu-refresh' explicitly when desired."
  (let* ((default-directory (majutsu--toplevel-safe default-directory))
         (jj (majutsu-jj--executable))
         (args (majutsu-process-jj-arguments args))
         (pwd default-directory)
         (process-buf (let ((default-directory pwd))
                        (majutsu-process-buffer t)))
         (process-root (with-current-buffer process-buf default-directory)))
    (pcase-let* ((section
                  (with-current-buffer process-buf
                    (prog1 (majutsu--process-insert-section pwd jj args nil nil)
                      (backward-char 1))))
                 (inhibit-read-only t)
                 (exit (apply #'process-file jj nil process-buf nil args)))
      (setq exit (majutsu-process-finish exit process-buf (current-buffer) process-root section))
      exit)))

(defun majutsu-run-jj-async (&rest args)
  "Start jj asynchronously, preparing for refresh, and return the process.
ARGS is flattened before being passed to jj."
  (let ((flat (flatten-tree args)))
    (majutsu--message-with-log "Running %s %s"
                               (majutsu-jj--executable)
                               (string-join flat " ")))
  (majutsu-start-jj args))

(defun majutsu-run-jj (&rest args)
  "Call jj synchronously in a separate process, and refresh.

Process output goes into a new section in the buffer returned by
`majutsu-process-buffer'.  Return the exit code."
  (let ((exit (apply #'majutsu-call-jj args)))
    (majutsu-refresh)
    exit))

(defun majutsu-run-jj-with-editor (&rest args)
  "Run JJ ARGS using with-editor."
  (majutsu-with-editor (apply #'majutsu-run-jj-async args)))

;;; _
(provide 'majutsu-process)
;;; majutsu-process.el ends here
