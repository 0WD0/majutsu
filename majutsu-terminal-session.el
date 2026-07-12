;;; majutsu-terminal-session.el --- Host interactive jj terminal sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shared Ghostel hosting and repository completion for interactive jj
;; terminal sessions.

;;; Code:

(require 'cl-lib)
(require 'majutsu-jj)
(require 'majutsu-process)
(require 'majutsu-interactive)

(declare-function ghostel-exec "ghostel" (buffer program &optional args))
(declare-function ghostel-mode "ghostel" ())
(declare-function majutsu-interactive-complete-repository-operation
                  "majutsu-interactive"
                  (root origin-buffer operation-before
                        &optional unchanged-message))

(defvar ghostel-exit-functions)
(defvar ghostel-kill-buffer-on-exit)

(cl-defstruct (majutsu-terminal-session
               (:constructor majutsu-terminal-session-create))
  "A running interactive jj terminal session."
  argv origin-buffer repository-root title
  terminal-buffer process operation-id-before completed-p
  unchanged-message exec-function exit-function terminal-p)

(defvar-local majutsu-terminal-session--session nil
  "The `majutsu-terminal-session' associated with this terminal buffer.")

(defvar majutsu-terminal-session--live-sessions (make-hash-table :test 'equal)
  "Majutsu-owned interactive sessions indexed by workspace root.")

(defvar majutsu-terminal-session-exit-hook nil
  "Functions called after a Ghostel terminal session completes.

Each function receives the completed SESSION and Ghostel EVENT.  EVENT is nil
when completion follows a manual buffer kill or a startup fallback.  The hook
runs via a zero-delay timer, outside Ghostel's process sentinel.")

(defun majutsu-terminal-session-available-p ()
  "Return non-nil when Ghostel's public execution API is available."
  (and (require 'ghostel nil t)
       (fboundp 'ghostel-exec)
       (fboundp 'ghostel-mode)))

(defun majutsu-terminal-session-register (root owner)
  "Reserve ROOT for OWNER, or signal when another session owns it."
  (when (gethash root majutsu-terminal-session--live-sessions)
    (user-error "An interactive jj session is already active for this workspace"))
  (puthash root owner majutsu-terminal-session--live-sessions))

(defun majutsu-terminal-session-unregister (root owner)
  "Release ROOT when OWNER still owns its interactive-session slot."
  (when (eq (gethash root majutsu-terminal-session--live-sessions) owner)
    (remhash root majutsu-terminal-session--live-sessions)))

(defun majutsu-terminal-session--unregister (session)
  "Release SESSION's workspace slot if SESSION still owns it."
  (majutsu-terminal-session-unregister
   (majutsu-terminal-session-repository-root session) session))

(defun majutsu-terminal-session--lifecycle-process (session)
  "Return SESSION's known lifecycle process, if one has been created."
  (or (majutsu-terminal-session-process session)
      (when-let* ((buffer (majutsu-terminal-session-terminal-buffer session))
                  ((buffer-live-p buffer)))
        (get-buffer-process buffer))))

(defun majutsu-terminal-session--cleanup-unstarted-buffer (session)
  "Quietly remove SESSION's terminal buffer before its child starts."
  (when-let* ((buffer (majutsu-terminal-session-terminal-buffer session))
              ((buffer-live-p buffer)))
    (let ((inhibit-quit t))
      (with-current-buffer buffer
        (setq-local majutsu-terminal-session--session nil)
        (remove-hook 'kill-buffer-hook
                     #'majutsu-terminal-session--buffer-killed t)
        (remove-hook 'ghostel-exit-functions
                     #'majutsu-terminal-session--ghostel-exit t))
      (ignore-errors (kill-buffer buffer)))))

(defun majutsu-terminal-session--refresh-origin (session)
  "Refresh SESSION's live origin buffer after a repository state change."
  (when-let* ((buffer (majutsu-terminal-session-origin-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((default-directory
             (majutsu-terminal-session-repository-root session)))
        (when (derived-mode-p 'majutsu-mode)
          (majutsu-refresh))))))

(defun majutsu-terminal-session--invalidate-unowned (session)
  "Conservatively invalidate SESSION after an unowned child setup failure."
  (let ((inhibit-quit t))
    (majutsu-interactive-invalidate-repository
     (majutsu-terminal-session-repository-root session))
    (run-at-time 0 nil #'majutsu-terminal-session--refresh-origin session)))

(defun majutsu-terminal-session--abort-start (session)
  "Clean up SESSION after a non-local exit during startup.

If a child exists, retain its workspace slot until its lifecycle process has
finished because it may already have changed the repository."
  (let ((process (majutsu-terminal-session--lifecycle-process session)))
    (when (and process (not (majutsu-terminal-session-process session)))
      (setf (majutsu-terminal-session-process session) process))
    (cond
     ((not process)
      (let ((inhibit-quit t))
        (majutsu-terminal-session--cleanup-unstarted-buffer session)
        (majutsu-terminal-session--unregister session)))
     ((and (processp process) (process-live-p process)))
     (t
      ;; The Ghostel exit hook may already be queued.  This duplicate-safe
      ;; fallback also covers a non-local exit after a fast child termination.
      (run-at-time 0 nil #'majutsu-terminal-session--finish session nil)))))

(defun majutsu-terminal-session--complete (session &optional event)
  "Complete SESSION once after Ghostel EVENT using repository freshness."
  (unless (majutsu-terminal-session-completed-p session)
    (setf (majutsu-terminal-session-completed-p session) t)
    (let (outcome)
      (unwind-protect
          (setq outcome
                (majutsu-interactive-complete-repository-operation
                 (majutsu-terminal-session-repository-root session)
                 (majutsu-terminal-session-origin-buffer session)
                 (majutsu-terminal-session-operation-id-before session)
                 (majutsu-terminal-session-unchanged-message session)))
        (majutsu-terminal-session--unregister session))
      ;; Completion observers run after releasing the slot, so they may start
      ;; the next session immediately.
      (when-let* ((function
                   (majutsu-terminal-session-exit-function session)))
        (funcall function session event))
      (when (majutsu-terminal-session-terminal-p session)
        (run-hook-with-args 'majutsu-terminal-session-exit-hook
                            session event))
      outcome)))

(defun majutsu-terminal-session--finish (session event)
  "Finish SESSION after Ghostel EVENT outside its process sentinel."
  (majutsu-terminal-session--complete session event))

(defun majutsu-terminal-session--ghostel-exit (buffer event)
  "Defer completion of BUFFER's terminal session after Ghostel EVENT."
  (when (buffer-live-p buffer)
    (when-let* ((session
                 (buffer-local-value 'majutsu-terminal-session--session
                                     buffer)))
      (run-at-time 0 nil #'majutsu-terminal-session--finish session event))))

(defun majutsu-terminal-session--buffer-killed ()
  "Finish the session associated with a manually killed terminal buffer."
  (when majutsu-terminal-session--session
    (run-at-time 0 nil #'majutsu-terminal-session--finish-after-kill
                 majutsu-terminal-session--session)))

(defun majutsu-terminal-session--finish-after-kill (session)
  "Complete SESSION once its Ghostel lifecycle process has stopped."
  (let ((process (majutsu-terminal-session-process session)))
    (if (and (processp process) (process-live-p process))
        (run-at-time 0.05 nil #'majutsu-terminal-session--finish-after-kill
                     session)
      (majutsu-terminal-session--finish session nil))))

(defun majutsu-terminal-session--buffer-name (session)
  "Return a fresh Ghostel buffer name for SESSION."
  (format "*majutsu %s: %s*"
          (majutsu-terminal-session-title session)
          (abbreviate-file-name
           (directory-file-name
            (majutsu-terminal-session-repository-root session)))))

(defun majutsu-terminal-session--default-exec
    (_session buffer program args)
  "Run PROGRAM with ARGS in BUFFER through Ghostel."
  (let ((process-environment (majutsu-process-environment args)))
    (ghostel-exec buffer program args)))

(defun majutsu-terminal-session--start (session)
  "Start SESSION through Ghostel's public API and return SESSION."
  (let* ((root (majutsu-terminal-session-repository-root session))
         (buffer (generate-new-buffer
                  (majutsu-terminal-session--buffer-name session)))
         (argv (majutsu-terminal-session-argv session))
         (args (let ((default-directory root))
                 (majutsu-process-jj-arguments argv)))
         (program (let ((default-directory root))
                    (majutsu-jj--executable))))
    (setf (majutsu-terminal-session-terminal-buffer session) buffer)
    (with-current-buffer buffer
      (setq default-directory root)
      ;; Major-mode activation clears ordinary buffer-local values.
      (ghostel-mode)
      (setq default-directory root)
      (setq-local majutsu-terminal-session--session session)
      (add-hook 'kill-buffer-hook
                #'majutsu-terminal-session--buffer-killed nil t)
      (setq-local ghostel-kill-buffer-on-exit nil)
      (add-hook 'ghostel-exit-functions
                #'majutsu-terminal-session--ghostel-exit nil t))
    ;; Ghostel sizes an undisplayed buffer as 80x24.  Display it before
    ;; spawning full-screen TUIs so their first frame has real geometry.
    (majutsu-display-buffer buffer)
    (let* ((default-directory root)
           (candidate
            (funcall
             (or (majutsu-terminal-session-exec-function session)
                 #'majutsu-terminal-session--default-exec)
             session buffer program args))
           (process (or (and (processp candidate) candidate)
                        (get-buffer-process buffer))))
      (unless (processp process)
        (error "Terminal execution did not create a lifecycle process"))
      (setf (majutsu-terminal-session-process session) process))
    session))

(defun majutsu-terminal-session--repository-root (origin-buffer)
  "Return the repository root associated with ORIGIN-BUFFER."
  (if (buffer-live-p origin-buffer)
      (with-current-buffer origin-buffer
        (or (majutsu--buffer-root origin-buffer)
            (majutsu--toplevel-safe default-directory)))
    (majutsu--toplevel-safe default-directory)))

;;;###autoload
(cl-defun majutsu-terminal-session-start-jj
    (argv &key origin-buffer title unchanged-message exec-function
          exit-function)
  "Start interactive jj ARGV in a Ghostel terminal.

ORIGIN-BUFFER supplies the repository context and is refreshed when jj records
an operation.  TITLE names the terminal buffer.  UNCHANGED-MESSAGE is shown
only when the repository operation id remains unchanged.  EXEC-FUNCTION, when
non-nil, is called with SESSION, BUFFER, PROGRAM, and fully prepared ARGS.
EXIT-FUNCTION receives SESSION and Ghostel's EVENT after completion; EVENT can
be nil when no exit event is available."
  (unless (and (proper-list-p argv)
               argv
               (cl-every #'stringp argv))
    (user-error "A nonempty jj argument list is required"))
  ;; Refuse before probing jj or creating a terminal buffer.  A pipe is not a
  ;; safe fallback for full-screen commands such as `jj arrange'.
  (unless (majutsu-terminal-session-available-p)
    (user-error "Ghostel is required for interactive jj terminal sessions"))
  (let* ((origin-buffer (or origin-buffer (current-buffer)))
         (root (file-name-as-directory
                (majutsu-terminal-session--repository-root origin-buffer)))
         (session
          (majutsu-terminal-session-create
           :argv (copy-sequence argv)
           :origin-buffer origin-buffer
           :repository-root root
           :title (or title (car argv))
           :unchanged-message unchanged-message
           :exec-function exec-function
           :exit-function exit-function
           :terminal-p t)))
    (majutsu-terminal-session-register root session)
    (let ((started nil))
      (unwind-protect
          (progn
            ;; Capture this only after owning the workspace slot and before jj's
            ;; workspace helper gets a chance to snapshot the working copy.
            (setf (majutsu-terminal-session-operation-id-before session)
                  (majutsu-jj-operation-id root))
            (prog1 (majutsu-terminal-session--start session)
              (setq started t)))
        (unless started
          (majutsu-terminal-session--abort-start session))))))

;;; _
(provide 'majutsu-terminal-session)
;;; majutsu-terminal-session.el ends here
