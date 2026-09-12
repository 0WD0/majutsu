;;; majutsu-diff-editor-test.el --- Tests for jj diff-editor sessions  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for diff-editor argument normalization and host routing.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-diff-editor)

(ert-deftest majutsu-diff-editor-interactive-arguments-p/recognizes-options ()
  "Recognize every supported diff-editor option spelling before `--'."
  (dolist (args '(("-i")
                  ("--interactive")
                  ("--tool" "meld")
                  ("--tool=meld")))
    (should (majutsu-diff-editor-interactive-arguments-p args)))
  ;; `-t' belongs to command-specific destination options (e.g. squash), not
  ;; jj's diff editor.  Never reinterpret it as a tool request.
  (should-not (majutsu-diff-editor-interactive-arguments-p '("-t" "target")))
  (should-not (majutsu-diff-editor-interactive-arguments-p '("-t=target")))
  (should-not
   (majutsu-diff-editor-interactive-arguments-p
    '("--from=@" "--" "-i" "--tool" ":builtin"))))

(ert-deftest majutsu-diff-editor-strip-interactive-arguments/removes-options-only ()
  "Strip editor options but preserve filesets and their `--' separator."
  (let ((args '("--from=@" "-i" "--interactive"
                "--tool" "meld" "--tool=vimdiff"
                "-t" "legacy" "-t=older"
                "--" "-i" "--tool" "literal")))
    (should
     (equal (majutsu-diff-editor-strip-interactive-arguments args)
            '("--from=@" "-t" "legacy" "-t=older"
              "--" "-i" "--tool" "literal")))
    (should (equal args
                   '("--from=@" "-i" "--interactive"
                     "--tool" "meld" "--tool=vimdiff"
                     "-t" "legacy" "-t=older"
                     "--" "-i" "--tool" "literal")))))

(ert-deftest majutsu-diff-editor-tool-from-arguments/uses-last-tool-before-filesets ()
  "Return the effective explicit tool and ignore fileset text."
  (should
   (equal (majutsu-diff-editor-tool-from-arguments
           '("--tool" "meld" "-t=vimdiff" "--tool=:builtin"
             "--" "--tool" "literal"))
          ":builtin"))
  (should-not
   (majutsu-diff-editor-tool-from-arguments
    '("--tool" "--" ":builtin")))
  (should-not
   (majutsu-diff-editor-tool-from-arguments '("-t" "destination"))))

(ert-deftest majutsu-diff-editor-tool-arguments/do-not-consume-another-option ()
  "A missing `--tool' value must leave the following option parseable."
  (should-not
   (majutsu-diff-editor-tool-from-arguments
    '("--tool" "--interactive")))
  (should-not
   (majutsu-diff-editor-strip-interactive-arguments
    '("--tool" "--interactive"))))

(ert-deftest majutsu-diff-editor-missing-tool-value-p/rejects-only-malformed-tools ()
  "A fileset separator cannot supply `--tool'."
  (should (majutsu-diff-editor--missing-tool-value-p '("--tool")))
  (should (majutsu-diff-editor--missing-tool-value-p
           '("--tool" "--" "literal")))
  (should (majutsu-diff-editor--missing-tool-value-p
           '("--tool" "--interactive")))
  (should (majutsu-diff-editor--missing-tool-value-p '("--tool=")))
  (should-not (majutsu-diff-editor--missing-tool-value-p
               '("--tool" "meld" "--" "literal"))))

(ert-deftest majutsu-diff-editor-select-host/auto-prefers-ghostel ()
  "Automatic mode uses Ghostel whenever it is available."
  (let ((majutsu-diff-editor-host 'auto)
        (with-editor-emacsclient-executable "emacsclient"))
    (cl-letf (((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-diff-editor--effective-tool)
               (lambda (_args) ":builtin")))
      (should (eq (majutsu-diff-editor-select-host
                   "split" '("--interactive"))
                  'ghostel)))))

(ert-deftest majutsu-diff-editor-select-host/auto-falls-back-without-emacsclient ()
  "External tools use the process bridge when Ghostel lacks `JJ_EDITOR' support."
  (let ((majutsu-diff-editor-host 'auto)
        (with-editor-emacsclient-executable nil))
    (cl-letf (((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-diff-editor--effective-tool)
               (lambda (_args) "meld"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (should (eq (majutsu-diff-editor-select-host
                   "split" '("--tool" "meld"))
                  'process)))))

(ert-deftest majutsu-diff-editor-select-host/auto-rejects-builtin-without-emacsclient ()
  "The built-in recorder cannot use Ghostel without `JJ_EDITOR' support."
  (let ((majutsu-diff-editor-host 'auto)
        (with-editor-emacsclient-executable nil))
    (cl-letf (((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-diff-editor--effective-tool)
               (lambda (_args) ":builtin")))
      (should-error
       (majutsu-diff-editor-select-host "split" '("--interactive"))
       :type 'user-error))))

(ert-deftest majutsu-diff-editor-select-host/auto-restores-without-emacsclient ()
  "Restore uses Ghostel because it cannot invoke a description editor."
  (let ((majutsu-diff-editor-host 'auto)
        (with-editor-emacsclient-executable nil))
    (cl-letf (((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-diff-editor--effective-tool)
               (lambda (_args) ":builtin")))
      (should (eq (majutsu-diff-editor-select-host
                   "restore" '("--interactive"))
                  'ghostel)))))

(ert-deftest majutsu-diff-editor-select-host/explicit-message-needs-no-emacsclient ()
  "Description-free Split and Squash paths can use a local Ghostel TUI."
  (let ((majutsu-diff-editor-host 'auto)
        (with-editor-emacsclient-executable nil))
    (cl-letf (((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-diff-editor--effective-tool)
               (lambda (_args) ":builtin")))
      (should (eq (majutsu-diff-editor-select-host
                   "split" '("--message=first" "--interactive"))
                  'ghostel))
      (should (eq (majutsu-diff-editor-select-host
                   "squash" '("--use-destination-message" "--interactive"))
                  'ghostel))
      (should-error
       (majutsu-diff-editor-select-host
        "split" '("--message=first" "--editor" "--interactive"))
       :type 'user-error))))

(ert-deftest majutsu-diff-editor-select-host/auto-falls-back-for-external-tool ()
  "Automatic mode permits the ordinary process host for external tools."
  (let ((majutsu-diff-editor-host 'auto))
    (cl-letf (((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () nil))
              ((symbol-function 'majutsu-diff-editor--effective-tool)
               (lambda (_args) "meld"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (should (eq (majutsu-diff-editor-select-host
                   "split" '("--tool" "meld"))
                  'process)))))

(ert-deftest majutsu-diff-editor-select-host/auto-remote-prefers-ghostel ()
  "TRAMP sessions use Ghostel without requiring a local emacsclient."
  (let ((majutsu-diff-editor-host 'auto)
        (with-editor-emacsclient-executable nil))
    (cl-letf (((symbol-function 'file-remote-p) (lambda (&rest _) t))
              ((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-diff-editor--effective-tool)
               (lambda (_args) "meld")))
      (should (eq (majutsu-diff-editor-select-host
                   "split" '("--tool" "meld"))
                  'ghostel)))))

(ert-deftest majutsu-diff-editor-select-host/auto-remote-hosts-builtin ()
  "TRAMP Ghostel sessions support jj's built-in recorder."
  (let ((majutsu-diff-editor-host 'auto)
        (with-editor-emacsclient-executable nil))
    (cl-letf (((symbol-function 'file-remote-p) (lambda (&rest _) t))
              ((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-diff-editor--effective-tool)
               (lambda (_args) ":builtin")))
      (should (eq (majutsu-diff-editor-select-host
                   "split" '("--interactive"))
                  'ghostel)))))

(ert-deftest majutsu-diff-editor-select-host/auto-remote-falls-back-without-ghostel ()
  "A remote external editor still has a process fallback without Ghostel."
  (let ((majutsu-diff-editor-host 'auto)
        (with-editor-emacsclient-executable nil))
    (cl-letf (((symbol-function 'file-remote-p) (lambda (&rest _) t))
              ((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () nil))
              ((symbol-function 'majutsu-diff-editor--effective-tool)
               (lambda (_args) "meld"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (should (eq (majutsu-diff-editor-select-host
                   "split" '("--tool" "meld"))
                  'process)))))

(ert-deftest majutsu-diff-editor-configured-tool/preserves-global-then-local-order ()
  "The config probe must match jj's global-then-invocation argv order."
  (let ((majutsu-jj-global-arguments
         '("--config" "ui.diff-editor=:builtin"))
        seen)
    (cl-letf (((symbol-function 'majutsu-jj--executable) (lambda () "jj"))
              ((symbol-function 'majutsu-process-file)
               (lambda (_program _infile _destination _display &rest args)
                 (setq seen args)
                 (insert "meld\n")
                 0)))
      (should (equal (majutsu-diff-editor--configured-tool
                      '("--config" "ui.diff-editor=meld"))
                     "meld"))
      (should (equal seen
                     '("--color=never"
                       "--config" "ui.diff-editor=:builtin"
                       "--config" "ui.diff-editor=meld"
                       "--ignore-working-copy" "config" "get"
                       "ui.diff-editor"))))))

(ert-deftest majutsu-diff-editor-select-host/rejects-builtin-without-terminal ()
  "A process buffer must never receive jj's built-in terminal editor."
  (dolist (host '(auto process))
    (let ((majutsu-diff-editor-host host))
      (cl-letf (((symbol-function 'majutsu-diff-editor-ghostel-available-p)
                 (lambda () nil))
                ((symbol-function 'majutsu-diff-editor--effective-tool)
                 (lambda (_args) ":builtin")))
        (should-error
         (majutsu-diff-editor-select-host
          "split" '("--tool=:builtin"))
         :type 'user-error)))))

(ert-deftest majutsu-diff-editor-select-host/explicit-ghostel-needs-package ()
  "The explicit Ghostel setting reports an unavailable optional package."
  (let ((majutsu-diff-editor-host 'ghostel))
    (cl-letf (((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () nil)))
      (should-error
       (majutsu-diff-editor-select-host "split" '("--tool" "meld"))
       :type 'user-error))))

(ert-deftest majutsu-diff-editor-start/ghostel-rejects-sleeping-editor-before-buffer-creation ()
  "Do not leak a terminal buffer when Ghostel cannot support `JJ_EDITOR'."
  (let ((majutsu-diff-editor-host 'ghostel)
        (with-editor-emacsclient-executable nil)
        (before (buffer-list)))
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-jj-operation-id)
               (lambda (&rest _) "before")))
      (should-error
       (majutsu-diff-editor-start "split" '("--tool=:builtin") nil)
       :type 'user-error)
      (should-not
       (seq-some (lambda (buffer)
                   (and (not (memq buffer before))
                        (string-prefix-p "*majutsu split diff editor:" (buffer-name buffer))))
                 (buffer-list))))))

(ert-deftest majutsu-diff-editor-assert-ghostel-editor-support/remote-needs-no-emacsclient ()
  "The remote with-editor bridge uses its sleeping editor."
  (let ((with-editor-emacsclient-executable nil))
    (cl-letf (((symbol-function 'file-remote-p) (lambda (&rest _) t)))
      (should-not
       (majutsu-diff-editor--assert-ghostel-editor-support
        "/ssh:example:/repo/" "split" nil)))))

(ert-deftest majutsu-diff-editor-assert-ghostel-editor-support/restore-needs-no-emacsclient ()
  "Local restore cannot invoke jj's description editor."
  (let ((with-editor-emacsclient-executable nil))
    (cl-letf (((symbol-function 'file-remote-p) (lambda (&rest _) nil)))
      (should-not
       (majutsu-diff-editor--assert-ghostel-editor-support
        "/repo/" "restore" nil)))))

(ert-deftest majutsu-diff-editor-ghostel-exec/tracks-open-during-remote-spawn ()
  "Track a fast JJ_EDITOR OPEN packet before `ghostel-exec' returns."
  (let ((majutsu-process--with-editor-file-roots (make-hash-table :test #'equal))
        (root "/ssh:example:/repo/")
        spawn-returned
        seen-before-return
        seen-root)
    (with-temp-buffer
      (let* ((buffer (current-buffer))
             (session (majutsu-diff-editor-session-create :repository-root root))
             (proc (make-process :name "majutsu-test"
                                 :buffer buffer
                                 :command (list "cat"))))
        (unwind-protect
            (let ((packet
                   (format (concat "WITH-EDITOR: 123 OPEN "
                                   "/tmp/editor.jjdescription%c%c IN /repo\n")
                           ?\x1f ?\x1f)))
              (cl-letf
                  (((symbol-function 'make-process)
                    (lambda (&rest keys)
                      (set-process-filter proc (plist-get keys :filter))
                      ;; Model output delivered synchronously by a TRAMP file
                      ;; handler before its `make-process' call returns.
                      (funcall (process-filter proc) proc packet)
                      proc))
                   ((symbol-function 'ghostel-exec)
                    (lambda (target-buffer _program _args)
                      (should (eq target-buffer buffer))
                      (prog1
                          (make-process
                           :name "ghostel"
                           :buffer target-buffer
                           :command '("/bin/sh" "-c" "exec jj split -i")
                           :connection-type 'pty
                           :file-handler "/ssh:example:"
                           :filter
                           (lambda (&rest _)
                             (setq seen-before-return (not spawn-returned)
                                   seen-root
                                   (majutsu-process-with-editor-file-root
                                    (concat "/ssh:example:"
                                            "/tmp/editor.jjdescription")))))
                        (setq spawn-returned t)))))
                (should (eq (majutsu-diff-editor--ghostel-exec
                             session buffer "jj" '("split"))
                            proc)))
              (should seen-before-return)
              (should (equal seen-root root)))
          (delete-process proc))))))

(ert-deftest majutsu-diff-editor-remote-with-editor-advice/composes-the-tracker ()
  "The real with-editor advice preserves Ghostel after Majutsu tracks output."
  (let ((majutsu-process--with-editor-file-roots (make-hash-table :test #'equal))
        (root "/ssh:example:/repo/")
        (track-output-function
         (symbol-function 'majutsu-process-track-with-editor-output))
        command order seen-root)
    (with-temp-buffer
      (let ((terminal (current-buffer))
            (proc (let ((default-directory temporary-file-directory))
                    (make-process :name "majutsu-test"
                                  :buffer (current-buffer)
                                  :command (list "cat")))))
        (unwind-protect
            (let ((default-directory root)
                  (with-editor-emacsclient-executable nil))
              (cl-letf (((symbol-function 'with-editor-process-filter)
                         (lambda (_process _output &optional no-default-filter)
                           (push (if no-default-filter 'with-editor :unexpected)
                                 order)
                           (setq seen-root
                                 (majutsu-process-with-editor-file-root
                                  "/ssh:example:/tmp/editor.jjdescription"))))
                        ((symbol-function 'majutsu-process-track-with-editor-output)
                         (lambda (process output)
                           (push 'tracker order)
                           (funcall track-output-function process output))))
                (majutsu-with-editor
                  (let ((process
                         (make-process@with-editor-process-filter
                          (lambda (&rest keys)
                            (setq command (plist-get keys :command))
                            (set-process-filter proc (plist-get keys :filter))
                            proc)
                          :name "ghostel"
                          :buffer terminal
                          :command '("/bin/sh" "-c" "exec jj split -i")
                          :connection-type 'pty
                          :filter (lambda (&rest _) (push 'ghostel order))
                          :file-handler "/ssh:example:")))
                    (majutsu-diff-editor--install-remote-with-editor-tracker
                     process root)
                    (funcall (process-filter process) process
                             (format (concat "WITH-EDITOR: 123 OPEN "
                                             "/tmp/editor.jjdescription%c%c IN /repo\n")
                                     ?\x1f ?\x1f)))))
              (should (equal (car command) "env"))
              (should (string-prefix-p "JJ_EDITOR=" (cadr command)))
              (should (equal (nreverse order)
                             '(tracker ghostel with-editor)))
              (should (equal seen-root root)))
          (delete-process proc))))))

(ert-deftest majutsu-diff-editor-start/ghostel-displays-and-prepares-buffer ()
  "Ghostel starts only after its session buffer is displayed and prepared."
  (let ((majutsu-diff-editor-host 'ghostel)
        (with-editor-emacsclient-executable "emacsclient")
        (majutsu-jj-environment '("MAJUTSU_DIFF_EDITOR_TEST=enabled"))
        (majutsu-diff-editor--live-sessions (make-hash-table :test 'equal))
        terminal
        seen)
    (save-window-excursion
      (with-temp-buffer
        (let ((origin (current-buffer)))
          (cl-letf (((symbol-function 'majutsu--toplevel-safe)
                     (lambda (&optional _directory) "/repo/"))
                    ((symbol-function 'majutsu-diff-editor-ghostel-available-p)
                     (lambda () t))
                    ;; Major-mode activation clears ordinary buffer locals.
                    ;; The session hook must therefore be installed after it.
                    ((symbol-function 'ghostel-mode)
                     (lambda ()
                       (kill-all-local-variables)
                       (setq major-mode 'ghostel-mode)
                       (setq-local ghostel-kill-buffer-on-exit t)
                       (setq-local ghostel-exit-functions nil)))
                    ((symbol-function 'majutsu-jj--executable)
                     (lambda () "jj"))
                    ((symbol-function 'majutsu-jj-operation-id)
                     (lambda (&rest _) "before"))
                    ((symbol-function 'majutsu-process-jj-arguments)
                     (lambda (args) (cons "--global" args)))
                    ((symbol-function 'majutsu-display-buffer)
                     (lambda (buffer)
                       (set-window-buffer (selected-window) buffer)))
                    ((symbol-function 'ghostel-exec)
                     (lambda (buffer program args)
                       (with-current-buffer buffer
                         (setq terminal buffer
                               seen
                               (list :displayed (get-buffer-window buffer t)
                                     :program program
                                     :args args
                                     :directory default-directory
                                     :environment process-environment
                                     :kill-on-exit ghostel-kill-buffer-on-exit
                                     :exit-hook
                                     (memq #'majutsu-diff-editor--ghostel-exit
                                           ghostel-exit-functions))))
                       'ghostel-process)))
            (let ((session
                   (majutsu-diff-editor-start
                    "split" '("--tool=:builtin") '("src/a.el")
                    :origin-buffer origin)))
              (should (majutsu-diff-editor-session-p session))
              (should (eq (majutsu-diff-editor-session-host session) 'ghostel))
              (should (eq (majutsu-diff-editor-session-process session)
                          'ghostel-process))
              (should (buffer-live-p terminal))
              (should (plist-get seen :displayed))
              (should (equal (plist-get seen :program) "jj"))
              (should (equal (plist-get seen :args)
                             '("--global" "split" "--tool=:builtin"
                               "--" "src/a.el")))
              (should (equal (plist-get seen :directory) "/repo/"))
              (should (member "MAJUTSU_DIFF_EDITOR_TEST=enabled"
                              (plist-get seen :environment)))
              (should-not (plist-get seen :kill-on-exit))
              (should (plist-get seen :exit-hook))
              (with-current-buffer terminal
                (should (eq majutsu-diff-editor--session session)))))
          (when (buffer-live-p terminal)
            (with-current-buffer terminal
              (setq-local majutsu-diff-editor--session nil)
              (remove-hook 'kill-buffer-hook
                           #'majutsu-diff-editor--terminal-buffer-killed t))
            (kill-buffer terminal)))))))

(ert-deftest majutsu-diff-editor-start/process-uses-session-aware-runner ()
  "Use the process worker's session-aware API when it is available."
  (let ((majutsu-diff-editor-host 'process)
        (majutsu-diff-editor--live-sessions (make-hash-table :test 'equal))
        called host-directory process-directory)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
              (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-diff-editor-select-host)
               (lambda (_command _args)
                 (setq host-directory default-directory)
                 'process))
              ((symbol-function 'majutsu-jj-operation-id)
               (lambda (&rest _) "before"))
              ((symbol-function 'majutsu-start-jj-with-editor)
               (lambda (&rest args)
                 (setq called args
                       process-directory default-directory)
                 'process)))
      (let ((session
             (majutsu-diff-editor-start
              "restore" '("--tool" "meld") '("src/a.el"))))
        (should (eq (majutsu-diff-editor-session-host session) 'process))
        (should (eq (majutsu-diff-editor-session-process session) 'process))
        (should (equal (car called)
                       '("restore" "--tool" "meld" "--" "src/a.el")))
        (should-not (nth 1 called))
        (should (functionp (nth 2 called)))
        (should (eq (nth 3 called) t))
        (should (equal host-directory "/repo/"))
        (should (equal process-directory "/repo/"))))))

(ert-deftest majutsu-diff-editor-start/allows-one-live-session-per-workspace ()
  "Reject concurrent rewrite sessions, then release the slot at completion."
  (let ((majutsu-diff-editor-host 'process)
        (majutsu-diff-editor--live-sessions (make-hash-table :test 'equal))
        session)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-diff-editor-select-host)
               (lambda (&rest _) 'process))
              ((symbol-function 'majutsu-jj-operation-id)
               (lambda (&rest _) "before"))
              ((symbol-function 'majutsu-start-jj-with-editor)
               (lambda (&rest _) 'fake-process))
              ((symbol-function 'majutsu-interactive-complete-repository-operation)
               (lambda (&rest _) 'unchanged)))
      (setq session
            (majutsu-diff-editor-start "split" '("--tool" "meld") nil))
      (should-error
       (majutsu-diff-editor-start "restore" '("--tool" "meld") nil)
       :type 'user-error)
      (should (eq (majutsu-diff-editor--complete-session session) 'unchanged))
      (should
       (majutsu-diff-editor-start "restore" '("--tool" "meld") nil)))))

(ert-deftest majutsu-diff-editor-start/quit-releases-an-unstarted-session ()
  "C-g during startup must not leave a workspace session lock behind."
  (let ((majutsu-diff-editor--live-sessions (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-diff-editor-select-host)
               (lambda (&rest _) 'ghostel))
              ((symbol-function 'majutsu-jj-operation-id)
               (lambda (&rest _) "before"))
              ((symbol-function 'majutsu-diff-editor--start-ghostel)
               (lambda (&rest _) (signal 'quit nil))))
      (let (quit)
        (condition-case nil
            (majutsu-diff-editor-start "split" '("--tool=:builtin") nil)
          (quit (setq quit t)))
        (should quit))
      (should-not (gethash "/repo/" majutsu-diff-editor--live-sessions)))))

(ert-deftest majutsu-diff-editor-abort-session-start/waits-for-process-callback ()
  "A dead ordinary child still owns its slot until its exit callback runs."
  (let* ((majutsu-diff-editor--live-sessions (make-hash-table :test 'equal))
         (session (majutsu-diff-editor-session-create
                   :repository-root "/repo/" :host 'process :process 'child))
         scheduled)
    (puthash "/repo/" session majutsu-diff-editor--live-sessions)
    (cl-letf (((symbol-function 'processp) (lambda (_process) t))
              ((symbol-function 'process-live-p) (lambda (_process) nil))
              ((symbol-function 'majutsu-process-completion-owned-p)
               (lambda (_process) t))
              ((symbol-function 'run-at-time)
               (lambda (&rest args) (setq scheduled args))))
      (majutsu-diff-editor--abort-session-start session)
      (should (eq (gethash "/repo/" majutsu-diff-editor--live-sessions)
                  session))
      ;; ERT itself may schedule an undo-boundary timer while this dynamic
      ;; mock is active; only a session-completion timer would be a regression.
      (should-not (and scheduled
                       (eq (nth 2 scheduled)
                           #'majutsu-diff-editor--complete-session))))))

(ert-deftest majutsu-diff-editor-start/process-setup-failures-release-slot ()
  "Filter and sentinel setup failures cannot strand the workspace slot."
  (dolist (phase '(filter sentinel))
    (let ((majutsu-diff-editor--live-sessions (make-hash-table :test 'equal))
          (with-editor-emacsclient-executable nil)
          (real-set-process-filter (symbol-function 'set-process-filter))
          (real-set-process-sentinel (symbol-function 'set-process-sentinel))
          (root (file-name-as-directory temporary-file-directory))
          process invalidated scheduled)
      (unwind-protect
          (with-temp-buffer
            (let ((process-buffer (current-buffer))
                  (default-directory root))
              (cl-letf (((symbol-function 'majutsu--toplevel-safe)
                         (lambda (&optional _directory) root))
                        ((symbol-function 'majutsu-diff-editor-select-host)
                         (lambda (&rest _) 'process))
                        ((symbol-function 'majutsu-jj-operation-id)
                         (lambda (&rest _) "before"))
                        ((symbol-function 'majutsu-jj--executable)
                         (lambda () "jj"))
                        ((symbol-function 'majutsu-process-jj-arguments)
                         (lambda (args) args))
                        ((symbol-function 'majutsu-process-buffer)
                         (lambda (&optional _nodisplay) process-buffer))
                        ((symbol-function 'majutsu--process-insert-section)
                         (lambda (&rest _args)
                           (insert "\n")
                           'not-a-section))
                        ((symbol-function 'start-file-process)
                         (lambda (name buffer _program &rest _args)
                           (setq process
                                 (make-process
                                  :name (format "%s-diff-editor-setup-test"
                                                name)
                                  :buffer buffer :command '("cat")
                                  :noquery t))))
                        ((symbol-function 'majutsu--process-install-filter)
                         (lambda (child)
                           (if (eq phase 'filter)
                               (error "filter setup failed")
                             (funcall real-set-process-filter child #'ignore))))
                        ((symbol-function 'set-process-sentinel)
                         (lambda (child sentinel)
                           (if (eq phase 'sentinel)
                               (error "sentinel setup failed")
                             (funcall real-set-process-sentinel
                                      child sentinel))))
                        ((symbol-function 'majutsu-interactive-invalidate-repository)
                         (lambda (root) (setq invalidated root)))
                        ((symbol-function 'run-at-time)
                         (lambda (&rest args) (setq scheduled args))))
                (should-error
                 (majutsu-diff-editor-start
                  "restore" '("--tool" "meld") nil))
                (should (process-get process 'finish-callback))
                (should-not (majutsu-process-completion-owned-p process))
                (should-not
                 (gethash root majutsu-diff-editor--live-sessions))
                (should (equal invalidated root))
                (should (equal (nth 2 scheduled)
                               #'majutsu-diff-editor--refresh-origin)))))
        (when (and (processp process) (process-live-p process))
          (delete-process process))))))

(ert-deftest majutsu-diff-editor-finish-terminal-kill/waits-for-live-reaper ()
  "Do not compare operation ids until Ghostel's reaper has exited."
  (let ((session (majutsu-diff-editor-session-create :process 'reaper))
        scheduled completed)
    (cl-letf (((symbol-function 'processp) (lambda (_process) t))
              ((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'run-at-time)
               (lambda (&rest args) (setq scheduled args)))
              ((symbol-function 'majutsu-diff-editor--complete-session)
               (lambda (&rest _) (setq completed t))))
      (majutsu-diff-editor--finish-terminal-kill session)
      (should-not completed)
      (should (equal scheduled
                     (list 0.05 nil
                           #'majutsu-diff-editor--finish-terminal-kill
                           session))))))

(ert-deftest majutsu-diff-editor-finish-terminal-kill/completes-after-reaper ()
  "A stopped lifecycle process completes the session immediately."
  (let ((session (majutsu-diff-editor-session-create :process 'reaper))
        completed)
    (cl-letf (((symbol-function 'processp) (lambda (_process) t))
              ((symbol-function 'process-live-p) (lambda (_process) nil))
              ((symbol-function 'majutsu-diff-editor--complete-session)
               (lambda (value &rest _) (setq completed value))))
      (majutsu-diff-editor--finish-terminal-kill session)
      (should (eq completed session)))))

(ert-deftest majutsu-diff-editor-ghostel-exit/defers-event-without-exit-status ()
  "Ghostel completion uses its event and never interprets a process status."
  (let* ((buffer (generate-new-buffer " *majutsu-ghostel-exit*"))
         (session (majutsu-diff-editor-session-create
                   :repository-root "/repo/"))
         scheduled
         completed)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local majutsu-diff-editor--session session))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest args) (setq scheduled args))))
            (majutsu-diff-editor--ghostel-exit buffer "finished\n"))
          (should (equal scheduled
                         (list 0 nil
                               #'majutsu-diff-editor--finish-ghostel-session
                               session "finished\n")))
          (cl-letf (((symbol-function 'process-exit-status)
                     (lambda (&rest _)
                       (ert-fail "Ghostel completion read process status")))
                    ((symbol-function 'majutsu-diff-editor--complete-session)
                     (lambda (&rest args) (setq completed args))))
            (apply (nth 2 scheduled) (nthcdr 3 scheduled)))
          (should (equal completed
                         (list session "finished\n"
                               "jj diff editor ended without a repository operation"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest majutsu-diff-editor-complete-session/delegates-repository-freshness ()
  "Use the shared repository-wide completion contract."
  (let* ((origin (generate-new-buffer " *majutsu-diff-editor-origin*"))
         (session (majutsu-diff-editor-session-create
                   :origin-buffer origin :repository-root "/repo/"
                   :operation-id-before "before"))
         called)
    (unwind-protect
        (cl-letf (((symbol-function 'majutsu-interactive-complete-repository-operation)
                   (lambda (&rest args)
                     (setq called args)
                     'changed)))
          (should
           (eq (majutsu-diff-editor--complete-session
                session nil "unchanged message")
               'changed))
          (should (equal called
                         (list "/repo/" origin "before"
                               "unchanged message"))))
      (kill-buffer origin))))

(ert-deftest majutsu-diff-editor-finish-process-session/checks-every-exit-code ()
  "Failures still probe freshness, but an unchanged failure stays quiet."
  (dolist (case '((0 "jj diff editor ended without a repository operation")
                  (1 nil)
                  (nil nil)))
    (let* ((session (majutsu-diff-editor-session-create
                     :repository-root "/repo/"
                     :operation-id-before "before"))
           called)
      (cl-letf (((symbol-function 'majutsu-interactive-complete-repository-operation)
                 (lambda (&rest args)
                   (setq called args)
                   'unchanged)))
        (should (eq (majutsu-diff-editor--finish-process-session
                     session (car case))
                    'unchanged))
        (should (equal called
                       (list "/repo/" nil "before" (cadr case))))))))

(ert-deftest majutsu-diff-editor-complete-session/quit-releases-slot ()
  "C-g in shared completion leaves no workspace session lock."
  (let* ((majutsu-diff-editor--live-sessions (make-hash-table :test 'equal))
         (session (majutsu-diff-editor-session-create
                   :repository-root "/repo/" :operation-id-before "before"))
         shared-completion-ran)
    (puthash "/repo/" session majutsu-diff-editor--live-sessions)
    (cl-letf (((symbol-function 'majutsu-interactive-complete-repository-operation)
               (lambda (&rest _)
                 (setq shared-completion-ran t)
                 (signal 'quit nil))))
      (let (quit)
        (condition-case nil
            (majutsu-diff-editor--complete-session session)
          (quit (setq quit t)))
        (should quit))
      (should shared-completion-ran)
      (should-not (gethash "/repo/" majutsu-diff-editor--live-sessions)))))

(ert-deftest majutsu-diff-editor-complete-session/releases-before-exit-hook ()
  "An observer may start another session after the prior slot is released."
  (let* ((majutsu-diff-editor--live-sessions (make-hash-table :test 'equal))
         (session (majutsu-diff-editor-session-create
                   :repository-root "/repo/" :operation-id-before "before"))
         hook-saw-slot)
    (puthash "/repo/" session majutsu-diff-editor--live-sessions)
    (let ((majutsu-diff-editor-session-exit-hook
           (list (lambda (&rest _)
                   (setq hook-saw-slot
                         (gethash "/repo/" majutsu-diff-editor--live-sessions))))))
      (cl-letf (((symbol-function 'majutsu-interactive-complete-repository-operation)
                 (lambda (&rest _) 'unchanged)))
        (should (eq (majutsu-diff-editor--complete-session session "finished")
                    'unchanged))
        (should-not hook-saw-slot)))))

(provide 'majutsu-diff-editor-test)
;;; majutsu-diff-editor-test.el ends here
