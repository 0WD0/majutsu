;;; majutsu-diff-editor.el --- Host jj diff-editor sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shared argument handling and terminal hosting for jj diff-editor sessions.

;;; Code:

(require 'cl-lib)
(require 'majutsu-jj)
(require 'majutsu-process)
(require 'majutsu-interactive)

(declare-function ghostel-exec "ghostel" (buffer program &optional args))
(declare-function ghostel-mode "ghostel" ())
(declare-function majutsu-start-jj-with-editor "majutsu-process"
                  (args &optional success-msg finish-callback inhibit-refresh))

(defvar ghostel-exit-functions)
(defvar ghostel-kill-buffer-on-exit)
(defvar majutsu-process--start-created-callback)

;;; Customization

(defgroup majutsu-diff-editor nil
  "jj diff-editor sessions in Majutsu."
  :group 'majutsu)

(defcustom majutsu-diff-editor-host 'auto
  "How Majutsu hosts jj diff-editor sessions.

`auto' uses Ghostel when it is available, otherwise it permits an
ordinary process only for a known external editor.  `ghostel' requires
Ghostel.  `process' always uses an ordinary process, which cannot host
jj's built-in terminal editor."
  :type '(choice (const :tag "Automatic" auto)
                 (const :tag "Ghostel terminal" ghostel)
                 (const :tag "Ordinary process (external editor only)" process))
  :group 'majutsu-diff-editor)

;;; Arguments

(defun majutsu-diff-editor--inline-tool-value (arg)
  "Return the tool value embedded in ARG, or nil when there is none."
  (when (and (stringp arg) (string-prefix-p "--tool=" arg))
    (substring arg (length "--tool="))))

(defun majutsu-diff-editor--tool-option-p (arg)
  "Return non-nil when ARG is a supported explicit tool option."
  (or (equal arg "--tool")
      (majutsu-diff-editor--inline-tool-value arg)))

(defun majutsu-diff-editor-interactive-arguments-p (args)
  "Return non-nil when ARGS request jj's diff-editor.

Recognize `-i', `--interactive', and `--tool' spellings.
Options after `--' are filesets, not command options."
  (catch 'interactive
    (dolist (arg args)
      (cond
       ((equal arg "--") (throw 'interactive nil))
       ((or (member arg '("-i" "--interactive"))
            (majutsu-diff-editor--tool-option-p arg))
        (throw 'interactive t))))
    nil))

(defun majutsu-diff-editor-strip-interactive-arguments (args)
  "Return ARGS without jj diff-editor options.

Remove `-i', `--interactive', `--tool VALUE', and `--tool=VALUE' before
`--'.  Preserve the separator and every following fileset exactly."
  (let (result filesets)
    (while args
      (let ((arg (pop args)))
        (cond
         (filesets
          (push arg result))
         ((equal arg "--")
          (setq filesets t)
          (push arg result))
         ((member arg '("-i" "--interactive")))
         ((equal arg "--tool")
          ;; Do not swallow the fileset separator when the option is malformed.
          (when (and args (not (equal (car args) "--")))
            (pop args)))
         ((majutsu-diff-editor--inline-tool-value arg))
         (t
          (push arg result)))))
    (nreverse result)))

(defun majutsu-diff-editor-tool-from-arguments (args)
  "Return the last explicit jj diff-editor tool in ARGS, or nil.

Ignore tokens after `--'."
  (let (tool filesets)
    (while args
      (let ((arg (pop args)))
        (cond
         (filesets)
         ((equal arg "--")
          (setq filesets t))
         ((equal arg "--tool")
          (when (and args (not (equal (car args) "--")))
            (setq tool (pop args))))
         ((let ((value (majutsu-diff-editor--inline-tool-value arg)))
            (when value
              (setq tool value)))))))
    tool))

(defun majutsu-diff-editor--missing-tool-value-p (args)
  "Return non-nil when ARGS has a malformed explicit tool option."
  (let (missing filesets)
    (while (and args (not missing))
      (let ((arg (pop args)))
        (cond
         (filesets)
         ((equal arg "--")
          (setq filesets t))
         ((equal arg "--tool")
          (if (and args (not (equal (car args) "--")))
              (pop args)
            (setq missing t)))
         ((let ((value (majutsu-diff-editor--inline-tool-value arg)))
            (and value (string-empty-p value)))
          (setq missing t)))))
    missing))

;;; Host selection

(defun majutsu-diff-editor-ghostel-available-p ()
  "Return non-nil when Ghostel's public execution API is available.

Ghostel remains an optional dependency, so it is only required when a
diff-editor session needs it."
  (and (require 'ghostel nil t)
       (fboundp 'ghostel-exec)
       (fboundp 'ghostel-mode)))

(defun majutsu-diff-editor--config-overrides (args)
  "Return global config overrides in ARGS, before its fileset separator."
  (let (result filesets)
    (while args
      (let ((arg (pop args)))
        (cond
         (filesets)
         ((equal arg "--")
          (setq filesets t))
         ((member arg '("--config" "--config-file"))
          (push arg result)
          (when (and args (not (equal (car args) "--")))
            (push (pop args) result)))
         ((and (stringp arg)
               (or (string-prefix-p "--config=" arg)
                   (string-prefix-p "--config-file=" arg)))
          (push arg result)))))
    (nreverse result)))

(defun majutsu-diff-editor--configured-tool (args)
  "Return the configured `ui.diff-editor' value for ARGS, or nil.

`majutsu-jj-string' itself prepends `majutsu-jj-global-arguments'; repeat only
the invocation-specific config overrides here so the probe has the same order
as the eventual jj invocation.
A missing setting deliberately returns nil: jj then defaults to `:builtin'."
  (condition-case nil
      (when-let* ((value
                   (apply #'majutsu-jj-string
                          (append (majutsu-diff-editor--config-overrides args)
                                  '("--ignore-working-copy"
                                    "config" "get" "ui.diff-editor")))))
        (string-trim value))
    (error nil)))

(defun majutsu-diff-editor--effective-tool (args)
  "Return the known effective diff-editor tool for ARGS.

An explicit tool wins over `ui.diff-editor'; absent or unreadable config
has jj's documented `:builtin' default."
  (or (majutsu-diff-editor-tool-from-arguments args)
      (majutsu-diff-editor--configured-tool args)
      ":builtin"))

(defun majutsu-diff-editor--builtin-tool-p (tool)
  "Return non-nil when TOOL is jj's built-in terminal editor."
  (equal tool ":builtin"))

(defvar majutsu-diff-editor--process-fallback-noticed nil
  "Whether this Emacs session has explained the external process fallback.")

(defun majutsu-diff-editor--note-process-fallback ()
  "Explain the limitations of an ordinary process fallback once."
  (unless majutsu-diff-editor--process-fallback-noticed
    (setq majutsu-diff-editor--process-fallback-noticed t)
    (message (concat "Using an ordinary process for the external jj diff "
                     "editor. Terminal editors require Ghostel."))))

(defun majutsu-diff-editor-select-host (args)
  "Return the host symbol to use for a jj diff-editor session with ARGS.

Signal `user-error' rather than starting a terminal editor in a pipe.
The return value is either `ghostel' or `process'."
  (when (majutsu-diff-editor--missing-tool-value-p args)
    (user-error "jj --tool requires a tool name"))
  (pcase majutsu-diff-editor-host
    ('ghostel
     (if (majutsu-diff-editor-ghostel-available-p)
         'ghostel
       (user-error "Ghostel is required for the selected diff-editor host")))
    ('auto
     (let ((tool (majutsu-diff-editor--effective-tool args)))
       (cond
        ;; Ghostel cannot bridge with-editor's sleeping-editor protocol over
        ;; TRAMP.  An external tool can still use Majutsu's ordinary remote
        ;; process path; the built-in recorder cannot.
        ((file-remote-p default-directory)
         (if (majutsu-diff-editor--builtin-tool-p tool)
             (user-error (concat "jj's :builtin diff editor needs a terminal host; "
                                 "Ghostel sessions over TRAMP are unsupported, so "
                                 "configure an external editor"))
           (majutsu-diff-editor--note-process-fallback)
           'process))
        ((and (majutsu-diff-editor-ghostel-available-p)
              with-editor-emacsclient-executable)
         'ghostel)
        ((majutsu-diff-editor--builtin-tool-p tool)
         (user-error (concat "jj's :builtin diff editor requires Ghostel and a "
                             "working emacsclient; configure an external editor "
                             "or set `with-editor-emacsclient-executable'")))
        (t
         (majutsu-diff-editor--note-process-fallback)
         'process))))
    ('process
     (if (majutsu-diff-editor--builtin-tool-p
          (majutsu-diff-editor--effective-tool args))
         (user-error (concat "jj's :builtin diff editor requires a terminal host; "
                             "select Ghostel instead"))
       'process))
    (_
     (user-error "Invalid `majutsu-diff-editor-host': %S"
                 majutsu-diff-editor-host))))

;;; Sessions

(cl-defstruct (majutsu-diff-editor-session
               (:constructor majutsu-diff-editor-session-create))
  "A running jj diff-editor session."
  command args filesets origin-buffer repository-root
  host terminal-buffer process
  operation-id-before selection-context started-at completed-p)

(defvar-local majutsu-diff-editor--session nil
  "The `majutsu-diff-editor-session' associated with this terminal buffer.")

(defvar majutsu-diff-editor--live-sessions (make-hash-table :test 'equal)
  "Majutsu-owned diff-editor sessions indexed by repository root.")

(defun majutsu-diff-editor--session-active-p (_session)
  "Return non-nil while SESSION still owns its repository interaction slot."
  ;; Do not use `process-live-p' here.  A child may have exited while its
  ;; zero-delay completion timer has not yet run; releasing the slot in that
  ;; window would allow a second history rewrite to overlap completion.
  t)

(defun majutsu-diff-editor--register-session (session)
  "Reserve SESSION's repository slot, or signal if another session owns it."
  (let* ((root (majutsu-diff-editor-session-repository-root session))
         (existing (gethash root majutsu-diff-editor--live-sessions)))
    (when existing
      (if (majutsu-diff-editor--session-active-p existing)
          (user-error "A jj diff-editor session is already active for this repository")
        (remhash root majutsu-diff-editor--live-sessions)))
    (puthash root session majutsu-diff-editor--live-sessions)))

(defun majutsu-diff-editor--unregister-session (session)
  "Release SESSION's repository slot if it is still its owner."
  (let ((root (majutsu-diff-editor-session-repository-root session)))
    (when (eq (gethash root majutsu-diff-editor--live-sessions) session)
      (remhash root majutsu-diff-editor--live-sessions))))

(defun majutsu-diff-editor--session-lifecycle-process (session)
  "Return SESSION's known lifecycle process, if one has been created."
  (or (majutsu-diff-editor-session-process session)
      (when-let* ((buffer (majutsu-diff-editor-session-terminal-buffer session))
                  ((buffer-live-p buffer)))
        ;; This is a public Emacs association.  It also covers a non-local
        ;; exit from `ghostel-exec' after the PTY process exists but before it
        ;; could return its lifecycle object to us.
        (get-buffer-process buffer))))

(defun majutsu-diff-editor--cleanup-unstarted-terminal-buffer (session)
  "Remove SESSION bookkeeping and kill its terminal buffer before spawn.

The function is deliberately quiet and idempotent so it is safe from the
inner Ghostel startup cleanup and the outer session-start cleanup."
  (when-let* ((buffer (majutsu-diff-editor-session-terminal-buffer session))
              ((buffer-live-p buffer)))
    (let ((inhibit-quit t))
      (with-current-buffer buffer
        (setq-local majutsu-diff-editor--session nil)
        (remove-hook 'kill-buffer-hook
                     #'majutsu-diff-editor--terminal-buffer-killed t)
        (remove-hook 'ghostel-exit-functions
                     #'majutsu-diff-editor--ghostel-exit t))
      (ignore-errors (kill-buffer buffer)))))

(defun majutsu-diff-editor--abort-session-start (session)
  "Clean up SESSION after a non-local exit during startup.

If a session-owned child exists, retain its repository slot until completion:
it may have exited just before its deferred callback runs.  A child that never
received a completion owner is stopped and treated as an unknown outcome."
  (let ((process (majutsu-diff-editor--session-lifecycle-process session)))
    (when (and process (not (majutsu-diff-editor-session-process session)))
      (setf (majutsu-diff-editor-session-process session) process))
    (cond
     ;; No child exists, so this is a pure setup failure.  The user selection
     ;; remains valid and the slot can be released immediately.
     ((not process)
      (let ((inhibit-quit t))
        (majutsu-diff-editor--cleanup-unstarted-terminal-buffer session)
        (majutsu-diff-editor--unregister-session session)))
     ;; Ghostel installs its exit hook before spawning.  Even a dead lifecycle
     ;; process may already have queued that hook, so retain the slot.  Queue a
     ;; duplicate-safe fallback only after the reaper is no longer live.
     ((eq (majutsu-diff-editor-session-host session) 'ghostel)
      (unless (and (processp process) (process-live-p process))
        (run-at-time 0 nil #'majutsu-diff-editor--complete-session session)))
     ;; The ordinary runner's callback is installed before its sentinel.  Keep
     ;; its slot even after the child exits: that callback alone has the real
     ;; exit status needed to distinguish failure from a clean editor cancel.
     ((and (processp process) (process-get process 'finish-callback))
      nil)
     ;; A child was created but setup was interrupted before it gained a
     ;; completion owner.  It could have mutated the repo, so make selection
     ;; state conservative before freeing the slot.
     (t
      (let ((inhibit-quit t))
        (when (and (processp process) (process-live-p process))
          (ignore-errors (delete-process process)))
        (majutsu-diff-editor--cleanup-unstarted-terminal-buffer session)
        (majutsu-diff-editor--abort-session-completion session)
        (majutsu-diff-editor--unregister-session session))))))

(defvar majutsu-diff-editor-session-exit-hook nil
  "Functions called after a Ghostel diff-editor session ends.

Each function receives SESSION and Ghostel's EVENT string.  It runs via a
zero-delay timer, outside Ghostel's process sentinel.")

(defun majutsu-diff-editor--operation-id (root)
  "Return ROOT's current jj operation id without snapshotting its working copy.

Return nil when jj cannot provide an id.  Callers must treat that result as an
unknown session outcome rather than as success or cancellation."
  (condition-case nil
      (let ((default-directory root))
        (when-let* ((id (majutsu-jj-string
                         "--ignore-working-copy" "operation" "log"
                         "--no-graph" "-n" "1" "-T" "id")))
          (unless (string-empty-p id)
            id)))
    (error nil)))

(defun majutsu-diff-editor--invalidate-origin-selection (session)
  "Invalidate SESSION's origin-buffer patch selection, if it is still live."
  (when-let* ((buffer (majutsu-diff-editor-session-origin-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when (fboundp 'majutsu-interactive-invalidate)
        (majutsu-interactive-invalidate)))))

(defun majutsu-diff-editor--refresh-origin (session)
  "Refresh SESSION's live origin buffer after a repository state change."
  (when-let* ((buffer (majutsu-diff-editor-session-origin-buffer session))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((default-directory
             (majutsu-diff-editor-session-repository-root session)))
        (when (derived-mode-p 'majutsu-mode)
          (majutsu-refresh))))))

(defun majutsu-diff-editor--abort-session-completion (session)
  "Conservatively invalidate SESSION when its completion is interrupted."
  ;; A quit during the operation-id probe leaves the repository outcome
  ;; unknown.  Never leave position-based selections usable in that state.
  (let ((inhibit-quit t))
    (majutsu-diff-editor--invalidate-origin-selection session)
    ;; Refresh later, outside the interrupted synchronous probe.  The origin
    ;; may already be gone; `majutsu-diff-editor--refresh-origin' handles it.
    (run-at-time 0 nil #'majutsu-diff-editor--refresh-origin session)))

(defun majutsu-diff-editor--complete-session (session &optional event)
  "Complete SESSION after EVENT, conservatively handling unknown outcomes.

Ghostel's lifecycle process does not expose jj's child exit status.  The
operation id therefore determines whether rendered selections remain safe.
For process-hosted sessions it also avoids treating a zero-status cancel as a
successful history rewrite."
  (unless (majutsu-diff-editor-session-completed-p session)
    (setf (majutsu-diff-editor-session-completed-p session) t)
    (let (outcome completed-normally)
      ;; Include the operation-id probe in the protected form.  `quit' is not
      ;; an `error', and must not leave a completed session registered forever.
      (unwind-protect
          (let* ((before (majutsu-diff-editor-session-operation-id-before session))
                 (after (majutsu-diff-editor--operation-id
                         (majutsu-diff-editor-session-repository-root session))))
            (setq outcome
                  (cond
                   ((and before after (equal before after)) 'unchanged)
                   ((and before after) 'changed)
                   (t 'unknown)))
            (pcase outcome
              ('unchanged
               (message "jj diff editor ended without a repository operation"))
              ('changed
               (majutsu-diff-editor--invalidate-origin-selection session)
               (majutsu-diff-editor--refresh-origin session))
              ('unknown
               ;; A failed probe is not evidence that the old buffer positions
               ;; are still valid.  Clear and refresh rather than risking the
               ;; wrong patch.
               (majutsu-diff-editor--invalidate-origin-selection session)
               (majutsu-diff-editor--refresh-origin session)
               (message "jj diff editor ended; could not verify its repository result")))
            (setq completed-normally t)
            outcome)
        (unless completed-normally
          (majutsu-diff-editor--abort-session-completion session))
        (majutsu-diff-editor--unregister-session session))
      ;; Hooks are observers, not part of the transactional completion path.
      ;; In particular they may immediately start another session for ROOT.
      (when event
        (run-hook-with-args 'majutsu-diff-editor-session-exit-hook
                            session event))
      outcome)))

(defun majutsu-diff-editor--finish-process-session (session exit-code)
  "Finish process-hosted SESSION after EXIT-CODE outside its sentinel."
  (if (and (integerp exit-code) (zerop exit-code))
      (majutsu-diff-editor--complete-session session)
    ;; The process runner has reported the failure.  jj's transaction is
    ;; expected to be atomic, so retain the user's patch selection.
    (majutsu-diff-editor--unregister-session session)))

(defun majutsu-diff-editor--session-jj-arguments (session)
  "Return jj argv, after the executable, for SESSION."
  (cons (majutsu-diff-editor-session-command session)
        (majutsu-jj-append-filesets
         (majutsu-diff-editor-session-args session)
         (majutsu-diff-editor-session-filesets session))))

(defun majutsu-diff-editor--terminal-buffer-name (session)
  "Return a fresh Ghostel terminal buffer name for SESSION."
  (format "*majutsu %s diff editor: %s*"
          (majutsu-diff-editor-session-command session)
          (abbreviate-file-name
           (directory-file-name
            (majutsu-diff-editor-session-repository-root session)))))

(defun majutsu-diff-editor--finish-ghostel-session (session event)
  "Run SESSION's Ghostel exit hook after EVENT outside its sentinel."
  (majutsu-diff-editor--complete-session session event))

(defun majutsu-diff-editor--ghostel-exit (buffer event)
  "Defer completion of BUFFER's diff-editor session after Ghostel EVENT."
  (when (buffer-live-p buffer)
    (when-let* ((session
                 (buffer-local-value 'majutsu-diff-editor--session buffer)))
      (run-at-time 0 nil #'majutsu-diff-editor--finish-ghostel-session
                   session event))))

(defun majutsu-diff-editor--terminal-buffer-killed ()
  "Finish the session associated with a manually killed Ghostel buffer."
  (when majutsu-diff-editor--session
    (run-at-time 0 nil #'majutsu-diff-editor--finish-terminal-kill
                 majutsu-diff-editor--session)))

(defun majutsu-diff-editor--finish-terminal-kill (session)
  "Complete SESSION after its Ghostel lifecycle process has stopped.

Killing the terminal buffer first closes its display.  Ghostel's native
reaper may still be waiting for jj to finish and record an operation, so do
not compare operation ids until that lifecycle process has exited."
  (let ((process (majutsu-diff-editor-session-process session)))
    (if (and (processp process) (process-live-p process))
        (run-at-time 0.05 nil #'majutsu-diff-editor--finish-terminal-kill
                     session)
      (majutsu-diff-editor--complete-session session))))

(defun majutsu-diff-editor--assert-ghostel-editor-support (root)
  "Signal unless Ghostel can support jj's later `JJ_EDITOR' invocation.

Ghostel's public execution API has no with-editor sleeping-editor filter.  A
remote session, or a local Emacs without emacsclient, would render the control
packet but leave jj waiting forever."
  (when (file-remote-p root)
    (user-error (concat "Ghostel jj diff-editor sessions over TRAMP are not yet "
                        "supported; use an external terminal instead")))
  (unless with-editor-emacsclient-executable
    (user-error (concat "Ghostel jj diff-editor sessions require emacsclient for "
                        "JJ_EDITOR; configure `with-editor-emacsclient-executable'"))))

(defun majutsu-diff-editor--start-ghostel (session)
  "Start SESSION through Ghostel's public API and return SESSION."
  (let ((root (majutsu-diff-editor-session-repository-root session)))
    (majutsu-diff-editor--assert-ghostel-editor-support root)
    (let* ((buffer (generate-new-buffer
                    (majutsu-diff-editor--terminal-buffer-name session)))
           (command (majutsu-diff-editor--session-jj-arguments session))
           (args (let ((default-directory root))
                   (majutsu-process-jj-arguments command)))
           (program (let ((default-directory root))
                      (majutsu-jj--executable))))
      (let ((started nil))
        (unwind-protect
            (progn
              (setf (majutsu-diff-editor-session-terminal-buffer session) buffer)
              (with-current-buffer buffer
                (setq default-directory root)
                ;; `ghostel-exec' initializes a non-Ghostel buffer by enabling
                ;; its major mode.  Major-mode activation clears ordinary
                ;; buffer-local variables, so do that public initialization
                ;; first; the session hook and retained transcript settings must
                ;; be installed afterwards.
                (ghostel-mode)
                (setq default-directory root)
                (setq-local majutsu-diff-editor--session session)
                (add-hook 'kill-buffer-hook
                          #'majutsu-diff-editor--terminal-buffer-killed nil t)
                ;; Preserve the transcript, and install the hook before the child exists.
                (setq-local ghostel-kill-buffer-on-exit nil)
                (add-hook 'ghostel-exit-functions
                          #'majutsu-diff-editor--ghostel-exit nil t))
              ;; `ghostel-exec' uses an undisplayed buffer's 80x24 fallback,
              ;; which is unsuitable for jj's full-screen built-in editor.
              (majutsu-display-buffer buffer)
              (let ((default-directory root))
                (majutsu-with-editor
                  ;; `majutsu-with-editor' has installed JJ_EDITOR in the dynamic
                  ;; environment.  Build Majutsu's normal jj environment after
                  ;; that, so Ghostel inherits both JJ_EDITOR and user overrides.
                  (let ((process-environment (majutsu-process-environment args)))
                    (setf (majutsu-diff-editor-session-process session)
                          (ghostel-exec buffer program args))))
                (setq started t)
                session))
          (unless started
            (majutsu-diff-editor--abort-session-start session)))))))

(defun majutsu-diff-editor--start-process (session)
  "Start SESSION in Majutsu's ordinary asynchronous process buffer."
  (let ((args (majutsu-diff-editor--session-jj-arguments session))
        (root (majutsu-diff-editor-session-repository-root session)))
    (setf (majutsu-diff-editor-session-process session)
          ;; The process API records these properties before installing its
          ;; sentinel.  The callback then decides refresh from the operation
          ;; id, so a normal editor cancel cannot discard Emacs selections.
          (let ((default-directory root)
                (majutsu-process--start-created-callback
                 (lambda (process)
                   (setf (majutsu-diff-editor-session-process session) process))))
            (if (fboundp 'majutsu-start-jj-with-editor)
                (majutsu-start-jj-with-editor
                 args nil
                 (lambda (_process exit-code)
                   ;; Defer both the jj probe and refresh out of the process
                   ;; sentinel.  A failed process leaves the selection intact.
                   (run-at-time 0 nil
                                #'majutsu-diff-editor--finish-process-session
                                session exit-code))
                 t)
              (majutsu-run-jj-with-editor args))))
    session))

;;;###autoload
(cl-defun majutsu-diff-editor-start
    (command args filesets &key origin-buffer selection-context)
  "Start jj COMMAND's configured diff-editor session.

ARGS are command options and FILESETS are already separated fileset values.
When ORIGIN-BUFFER is supplied it supplies the repository root; otherwise the
current buffer does.  SELECTION-CONTEXT is retained for a later session owner
to validate on completion.  Return a `majutsu-diff-editor-session'."
  (unless (and (stringp command) (not (string-empty-p command)))
    (user-error "A jj diff-editor command is required"))
  (let* ((origin-buffer (or origin-buffer (current-buffer)))
         (root (if (buffer-live-p origin-buffer)
                   (with-current-buffer origin-buffer
                     (or (majutsu--buffer-root origin-buffer)
                         (majutsu--toplevel-safe default-directory)))
                 (majutsu--toplevel-safe default-directory)))
         (session
          (majutsu-diff-editor-session-create
           :command command
           :args (copy-sequence (or args '()))
           :filesets (copy-sequence (or filesets '()))
           :origin-buffer origin-buffer
           :repository-root (file-name-as-directory root)
           :host (let ((default-directory (file-name-as-directory root)))
                   (majutsu-diff-editor-select-host args))
           :operation-id-before
           (majutsu-diff-editor--operation-id (file-name-as-directory root))
           :selection-context selection-context
           :started-at (current-time))))
    (majutsu-diff-editor--register-session session)
    (let ((started nil))
      (unwind-protect
          (prog1
              (pcase (majutsu-diff-editor-session-host session)
                ('ghostel (majutsu-diff-editor--start-ghostel session))
                ('process (majutsu-diff-editor--start-process session)))
            (setq started t))
        (unless started
          (majutsu-diff-editor--abort-session-start session))))))

;;; _
(provide 'majutsu-diff-editor)
;;; majutsu-diff-editor.el ends here
