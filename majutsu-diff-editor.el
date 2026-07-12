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
(require 'majutsu-terminal-session)

(declare-function ghostel-exec "ghostel" (buffer program &optional args))
(declare-function majutsu-start-jj-with-editor "majutsu-process"
                  (args &optional success-msg finish-callback inhibit-refresh))
(declare-function majutsu-process-completion-owned-p "majutsu-process"
                  (process))
(declare-function majutsu-process-track-with-editor-output "majutsu-process"
                  (process output))
(declare-function majutsu-interactive-complete-repository-operation
                  "majutsu-interactive"
                  (root origin-buffer operation-before
                        &optional unchanged-message))

(defvar majutsu-process--start-created-callback)

;;; Customization

(defgroup majutsu-diff-editor nil
  "Host jj diff-editor sessions in Majutsu."
  :group 'majutsu)

(defcustom majutsu-diff-editor-host 'auto
  "How Majutsu hosts jj diff-editor sessions.

`auto' uses Ghostel when it is available.  Local commands which can later
invoke jj's description editor also need a working `emacsclient'; remote
Ghostel sessions use with-editor's TRAMP sleeping-editor bridge.
Without a suitable terminal host, `auto' permits an ordinary process only
for a known external editor.  `ghostel' requires Ghostel.  `process' always
uses an ordinary process, which cannot host jj's built-in terminal editor."
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

(defun majutsu-diff-editor--tool-value-token-p (arg)
  "Return non-nil when ARG can be the value following `--tool'."
  (and (stringp arg)
       (not (string-prefix-p "-" arg))))

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
          ;; Do not swallow another option when the tool value is malformed.
          (when (and args
                     (majutsu-diff-editor--tool-value-token-p (car args)))
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
          (when (and args
                     (majutsu-diff-editor--tool-value-token-p (car args)))
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
          (if (and args
                   (majutsu-diff-editor--tool-value-token-p (car args)))
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
  (majutsu-terminal-session-available-p))

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

(defun majutsu-diff-editor--argument-present-p (args options)
  "Return non-nil when ARGS contain a member of OPTIONS before `--'."
  (catch 'present
    (dolist (arg args)
      (when (equal arg "--")
        (throw 'present nil))
      (when (member arg options)
        (throw 'present t)))
    nil))

(defun majutsu-diff-editor--description-editor-required-p (command args)
  "Return non-nil when jj COMMAND with ARGS may invoke a text editor.

Restore cannot edit a description.  Split with an explicit message and Squash
with an explicit message policy avoid that phase unless `--editor' is set."
  (let ((editor (majutsu-diff-editor--argument-present-p
                 args '("--editor"))))
    (pcase command
      ("restore" nil)
      ("split"
       (or editor
           (null (majutsu-jj-option-values args "--message" "-m"))))
      ("squash"
       (or editor
           (not (or (majutsu-jj-option-values args "--message" "-m")
                    (majutsu-diff-editor--argument-present-p
                     args '("--use-destination-message" "-u"))))))
      (_ t))))

(defun majutsu-diff-editor--ghostel-editor-supported-p (command args root)
  "Return non-nil when Ghostel can host COMMAND with ARGS under ROOT.

Remote processes use with-editor's sleeping-editor bridge.  A local command
only needs emacsclient when it can later request a description editor."
  (or (file-remote-p root)
      (not (majutsu-diff-editor--description-editor-required-p command args))
      with-editor-emacsclient-executable))

(defvar majutsu-diff-editor--process-fallback-noticed nil
  "Whether this Emacs session has explained the external process fallback.")

(defun majutsu-diff-editor--note-process-fallback ()
  "Explain the limitations of an ordinary process fallback once."
  (unless majutsu-diff-editor--process-fallback-noticed
    (setq majutsu-diff-editor--process-fallback-noticed t)
    (message (concat "Using an ordinary process for the external jj diff "
                     "editor. Terminal editors require Ghostel."))))

(defun majutsu-diff-editor-select-host (command args)
  "Return the host symbol for jj COMMAND's diff-editor session with ARGS.

Signal `user-error' rather than starting a terminal editor in a pipe.
The return value is either `ghostel' or `process'."
  (when (majutsu-diff-editor--missing-tool-value-p args)
    (user-error "The jj --tool option requires a tool name"))
  (pcase majutsu-diff-editor-host
    ('ghostel
     (if (majutsu-diff-editor-ghostel-available-p)
         'ghostel
       (user-error "Ghostel is required for the selected diff-editor host")))
    ('auto
     (let ((tool (majutsu-diff-editor--effective-tool args))
           (remote-p (file-remote-p default-directory))
           (ghostel-p (majutsu-diff-editor-ghostel-available-p)))
       (cond
        ;; Remote Ghostel runs through Emacs `make-process' with a TRAMP file
        ;; handler.  with-editor observes that path and installs its sleeping
        ;; editor filter, so unlike the local native PTY path it needs no
        ;; emacsclient.
        ((and ghostel-p
              (majutsu-diff-editor--ghostel-editor-supported-p
               command args default-directory))
         'ghostel)
        ((majutsu-diff-editor--builtin-tool-p tool)
         (user-error
          (cond
           (remote-p
            (concat "jj's :builtin diff editor requires Ghostel for this "
                    "TRAMP repository; install Ghostel or configure an "
                    "external editor"))
           (ghostel-p
            (concat "jj " command " may invoke a description editor after its "
                    ":builtin diff editor; configure a working emacsclient or "
                    "an external editor"))
           (t
            (concat "jj's :builtin diff editor requires Ghostel; install "
                    "Ghostel or configure an external editor")))))
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
               (:include majutsu-terminal-session)
               (:constructor majutsu-diff-editor-session-create))
  "A running jj diff-editor session."
  command args filesets host)

;; Keep the old private registry name as an alias while the terminal lifecycle
;; lives in one shared implementation.  Dynamic test isolation therefore still
;; isolates Arrange and diff-editor sessions together.
(defvaralias 'majutsu-diff-editor--live-sessions
  'majutsu-terminal-session--live-sessions)

(defun majutsu-diff-editor--register-session (session)
  "Reserve SESSION's workspace slot, or signal if another session owns it."
  (majutsu-terminal-session-register
   (majutsu-diff-editor-session-repository-root session) session))

(defun majutsu-diff-editor--unregister-session (session)
  "Release SESSION's workspace slot if it is still its owner."
  (majutsu-terminal-session-unregister
   (majutsu-diff-editor-session-repository-root session) session))

(defun majutsu-diff-editor--session-lifecycle-process (session)
  "Return SESSION's known lifecycle process, if one has been created."
  (majutsu-terminal-session--lifecycle-process session))

(defun majutsu-diff-editor--abort-session-start (session)
  "Clean up SESSION after a non-local exit during startup.

If a session-owned child exists, retain its workspace slot until completion:
it may have exited just before its deferred callback runs.  A child that never
received a completion owner is stopped and treated as an unknown outcome."
  (if (eq (majutsu-diff-editor-session-host session) 'ghostel)
      (majutsu-terminal-session--abort-start session)
    (let ((process (majutsu-diff-editor--session-lifecycle-process session)))
      (when (and process (not (majutsu-diff-editor-session-process session)))
        (setf (majutsu-diff-editor-session-process session) process))
      (cond
       ((not process)
        (majutsu-diff-editor--unregister-session session))
       ((and (processp process)
             (majutsu-process-completion-owned-p process)))
       (t
        (let ((inhibit-quit t))
          (when (and (processp process) (process-live-p process))
            (ignore-errors (delete-process process)))
          (majutsu-diff-editor--invalidate-unowned-session session)
          (majutsu-diff-editor--unregister-session session)))))))

(defvar majutsu-diff-editor-session-exit-hook nil
  "Functions called after a Ghostel diff-editor session ends.

Each function receives SESSION and Ghostel's EVENT string.  It runs via a
zero-delay timer, outside Ghostel's process sentinel.")

(defun majutsu-diff-editor--invalidate-unowned-session (session)
  "Conservatively invalidate SESSION after unowned process setup fails."
  (majutsu-terminal-session--invalidate-unowned session))

(defun majutsu-diff-editor--notify-exit (session event)
  "Run diff-editor observers for SESSION after Ghostel EVENT."
  (run-hook-with-args 'majutsu-diff-editor-session-exit-hook session event))

(defun majutsu-diff-editor--complete-session
    (session &optional event unchanged-message)
  "Complete SESSION after EVENT using repository freshness.

UNCHANGED-MESSAGE is displayed by the shared completion helper only when the
repository operation id is unchanged."
  (setf (majutsu-diff-editor-session-unchanged-message session)
        unchanged-message)
  (unless (or (majutsu-diff-editor-session-exit-function session)
              (null event))
    (setf (majutsu-diff-editor-session-exit-function session)
          #'majutsu-diff-editor--notify-exit))
  (majutsu-terminal-session--complete session event))

(defun majutsu-diff-editor--finish-process-session (session exit-code)
  "Finish process-hosted SESSION after EXIT-CODE outside its sentinel."
  (majutsu-diff-editor--complete-session
   session nil
   (and (integerp exit-code)
        (zerop exit-code)
        "jj diff editor ended without a repository operation")))

(defun majutsu-diff-editor--session-jj-arguments (session)
  "Return jj argv, after the executable, for SESSION."
  (cons (majutsu-diff-editor-session-command session)
        (majutsu-jj-append-filesets
         (majutsu-diff-editor-session-args session)
         (majutsu-diff-editor-session-filesets session))))

(defun majutsu-diff-editor--assert-ghostel-editor-support (root command args)
  "Signal unless Ghostel can support COMMAND with ARGS under ROOT.

Remote Ghostel sessions use Emacs `make-process' with a TRAMP file handler.
with-editor observes that path and supplies its sleeping-editor protocol.
Local native Ghostel PTYs require emacsclient only if COMMAND may invoke jj's
description editor."
  (unless (majutsu-diff-editor--ghostel-editor-supported-p command args root)
    (user-error
     (concat "jj " command " may invoke a description editor after its diff "
             "editor; configure `with-editor-emacsclient-executable'"))))

(defun majutsu-diff-editor--remote-with-editor-filter (filter root)
  "Return a process filter which tracks OPEN packets before FILTER under ROOT."
  (lambda (process output)
    ;; with-editor normally publishes this after `make-process' returns.  A
    ;; remote child can produce its first packet during process creation, so
    ;; make the workspace identity available at the start of the filter too.
    (process-put process 'default-dir root)
    (majutsu-process-track-with-editor-output process output)
    (funcall filter process output)))

(defun majutsu-diff-editor--install-remote-with-editor-tracker (process root)
  "Track remote with-editor packets from PROCESS for workspace ROOT.

Ghostel's filter must continue to receive every byte.  The wrapper records a
sleeping-editor OPEN packet before with-editor visits jj's temporary
description file, then delegates unchanged to Ghostel's existing composite
filter."
  (when (and (processp process)
             (not (process-get process 'majutsu-with-editor-tracker-installed)))
    ;; with-editor sets this itself for a remote `make-process', but it is the
    ;; protocol's authoritative workspace context, so retain it explicitly.
    (process-put process 'default-dir root)
    (when-let* ((filter (process-filter process)))
      (set-process-filter
       process (majutsu-diff-editor--remote-with-editor-filter filter root))
      (process-put process 'majutsu-with-editor-tracker-installed t))))

(defun majutsu-diff-editor--remote-ghostel-exec (buffer program args root)
  "Run remote PROGRAM with ARGS in Ghostel BUFFER under ROOT.

Ghostel currently exposes the lifecycle process only after `ghostel-exec'
returns.  Temporarily decorate the one remote PTY `make-process' call targeting
BUFFER so a fast JJ_EDITOR request is associated with workspace ROOT before
with-editor handles it.  Other process creation, including TRAMP connection
processes, is delegated untouched."
  (let ((make-process-function (symbol-function 'make-process)))
    (cl-letf
        (((symbol-function 'make-process)
          (lambda (&rest keys)
            (if (and (eq (plist-get keys :buffer) buffer)
                     (eq (plist-get keys :connection-type) 'pty)
                     (plist-get keys :file-handler)
                     (functionp (plist-get keys :filter)))
                (let* ((filter (plist-get keys :filter))
                       (tracked-filter
                        (majutsu-diff-editor--remote-with-editor-filter
                         filter root))
                       (process
                        (apply make-process-function
                               (plist-put keys :filter tracked-filter))))
                  (when (processp process)
                    (process-put process 'default-dir root)
                    (process-put process
                                 'majutsu-with-editor-tracker-installed t))
                  process)
              (apply make-process-function keys)))))
      (ghostel-exec buffer program args))))

(defun majutsu-diff-editor--ghostel-exec (session buffer program args)
  "Run PROGRAM with ARGS in Ghostel BUFFER for SESSION.

Use Ghostel's returned public lifecycle process.  Its existing filter already
combines Ghostel rendering with with-editor, so wrapping that filter preserves
their order while putting Majutsu's tracker first."
  (let* ((root (majutsu-diff-editor-session-repository-root session))
         (remote (file-remote-p root))
         (process (if remote
                      (majutsu-diff-editor--remote-ghostel-exec
                       buffer program args root)
                    (ghostel-exec buffer program args))))
    (when remote
      ;; This is a fallback for a Ghostel implementation whose public spawn
      ;; path did not use the expected remote PTY process constructor.
      (majutsu-diff-editor--install-remote-with-editor-tracker process root))
    process))

(defun majutsu-diff-editor--terminal-exec (session buffer program args)
  "Run diff-editor SESSION in BUFFER with PROGRAM and ARGS.

Install with-editor before constructing Majutsu's process environment so both
JJ_EDITOR and the user's jj environment reach the Ghostel child."
  (let ((root (majutsu-diff-editor-session-repository-root session)))
    (let ((default-directory root))
      (majutsu-with-editor
        (let ((process-environment (majutsu-process-environment args)))
          (majutsu-diff-editor--ghostel-exec
           session buffer program args))))))

(defun majutsu-diff-editor--start-ghostel (session)
  "Start SESSION through Ghostel's public API and return SESSION."
  (let ((root (majutsu-diff-editor-session-repository-root session)))
    (majutsu-diff-editor--assert-ghostel-editor-support
     root
     (majutsu-diff-editor-session-command session)
     (majutsu-diff-editor-session-args session))
    (setf (majutsu-diff-editor-session-argv session)
          (majutsu-diff-editor--session-jj-arguments session)
          (majutsu-diff-editor-session-title session)
          (format "%s diff editor"
                  (majutsu-diff-editor-session-command session))
          (majutsu-diff-editor-session-unchanged-message session)
          "jj diff editor ended without a repository operation"
          (majutsu-diff-editor-session-exec-function session)
          #'majutsu-diff-editor--terminal-exec
          (majutsu-diff-editor-session-exit-function session)
          #'majutsu-diff-editor--notify-exit
          (majutsu-diff-editor-session-terminal-p session) t)
    (majutsu-terminal-session--start session)))

(defun majutsu-diff-editor--start-process (session)
  "Start SESSION in Majutsu's ordinary asynchronous process buffer."
  (let ((args (majutsu-diff-editor--session-jj-arguments session))
        (root (majutsu-diff-editor-session-repository-root session)))
    (setf (majutsu-diff-editor-session-process session)
          ;; The creation callback retains the child during setup.  The process
          ;; layer publishes completion ownership only after installing both
          ;; its filter and sentinel.
          (let ((default-directory root)
                (majutsu-process--start-created-callback
                 (lambda (process)
                   (setf (majutsu-diff-editor-session-process session) process))))
            (majutsu-start-jj-with-editor
             args nil
             (lambda (_process exit-code)
               ;; Defer the operation-id probe and any refresh out of the
               ;; process sentinel.  Every exit status needs a freshness check.
               (run-at-time 0 nil
                            #'majutsu-diff-editor--finish-process-session
                            session exit-code))
             t)))
    session))

;;;###autoload
(cl-defun majutsu-diff-editor-start
    (command args filesets &key origin-buffer)
  "Start jj COMMAND's configured diff-editor session.

ARGS are command options and FILESETS are already separated fileset values.
When ORIGIN-BUFFER is supplied it supplies the repository root; otherwise the
current buffer does.  Return a `majutsu-diff-editor-session'."
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
                   (majutsu-diff-editor-select-host command args)))))
    (majutsu-diff-editor--register-session session)
    (let ((started nil))
      (unwind-protect
          (progn
            (setf (majutsu-diff-editor-session-operation-id-before session)
                  (majutsu-jj-operation-id
                   (majutsu-diff-editor-session-repository-root session)))
            (prog1
                (pcase (majutsu-diff-editor-session-host session)
                  ('ghostel (majutsu-diff-editor--start-ghostel session))
                  ('process (majutsu-diff-editor--start-process session)))
              (setq started t)))
        (unless started
          (majutsu-diff-editor--abort-session-start session))))))

;;; _
(provide 'majutsu-diff-editor)
;;; majutsu-diff-editor.el ends here
