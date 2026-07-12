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

(ert-deftest majutsu-diff-editor-missing-tool-value-p/rejects-only-malformed-tools ()
  "A fileset separator cannot supply `--tool'."
  (should (majutsu-diff-editor--missing-tool-value-p '("--tool")))
  (should (majutsu-diff-editor--missing-tool-value-p
           '("--tool" "--" "literal")))
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
      (should (eq (majutsu-diff-editor-select-host '("--interactive"))
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
      (should (eq (majutsu-diff-editor-select-host '("--tool" "meld"))
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
       (majutsu-diff-editor-select-host '("--interactive"))
       :type 'user-error))))

(ert-deftest majutsu-diff-editor-select-host/auto-falls-back-for-external-tool ()
  "Automatic mode permits the ordinary process host for external tools."
  (let ((majutsu-diff-editor-host 'auto))
    (cl-letf (((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () nil))
              ((symbol-function 'majutsu-diff-editor--effective-tool)
               (lambda (_args) "meld"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (should (eq (majutsu-diff-editor-select-host '("--tool" "meld"))
                  'process)))))

(ert-deftest majutsu-diff-editor-select-host/auto-remote-external-uses-process ()
  "TRAMP external editors use the ordinary process path, even with Ghostel."
  (let ((majutsu-diff-editor-host 'auto))
    (cl-letf (((symbol-function 'file-remote-p) (lambda (&rest _) t))
              ((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-diff-editor--effective-tool)
               (lambda (_args) "meld"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (should (eq (majutsu-diff-editor-select-host '("--tool" "meld"))
                  'process)))))

(ert-deftest majutsu-diff-editor-select-host/auto-remote-rejects-builtin ()
  "TRAMP cannot host jj's built-in recorder through Ghostel."
  (let ((majutsu-diff-editor-host 'auto))
    (cl-letf (((symbol-function 'file-remote-p) (lambda (&rest _) t))
              ((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () t))
              ((symbol-function 'majutsu-diff-editor--effective-tool)
               (lambda (_args) ":builtin")))
      (should-error
       (majutsu-diff-editor-select-host '("--interactive"))
       :type 'user-error))))

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
         (majutsu-diff-editor-select-host '("--tool=:builtin"))
         :type 'user-error)))))

(ert-deftest majutsu-diff-editor-select-host/explicit-ghostel-needs-package ()
  "The explicit Ghostel setting reports an unavailable optional package."
  (let ((majutsu-diff-editor-host 'ghostel))
    (cl-letf (((symbol-function 'majutsu-diff-editor-ghostel-available-p)
               (lambda () nil)))
      (should-error
       (majutsu-diff-editor-select-host '("--tool" "meld"))
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
              ((symbol-function 'majutsu-diff-editor--operation-id)
               (lambda (&rest _) "before")))
      (should-error
       (majutsu-diff-editor-start "split" '("--tool=:builtin") nil)
       :type 'user-error)
      (should-not
       (seq-some (lambda (buffer)
                   (and (not (memq buffer before))
                        (string-prefix-p "*majutsu split diff editor:" (buffer-name buffer))))
                 (buffer-list))))))

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
                    ((symbol-function 'majutsu-diff-editor--operation-id)
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
               (lambda (_args)
                 (setq host-directory default-directory)
                 'process))
              ((symbol-function 'majutsu-diff-editor--operation-id)
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

(ert-deftest majutsu-diff-editor-start/allows-one-live-session-per-repository ()
  "Reject concurrent rewrite sessions, then release the slot at completion."
  (let ((majutsu-diff-editor-host 'process)
        (majutsu-diff-editor--live-sessions (make-hash-table :test 'equal))
        session)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-diff-editor-select-host)
               (lambda (&rest _) 'process))
              ((symbol-function 'majutsu-diff-editor--operation-id)
               (lambda (&rest _) "before"))
              ((symbol-function 'majutsu-start-jj-with-editor)
               (lambda (&rest _) 'fake-process))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (setq session
            (majutsu-diff-editor-start "split" '("--tool" "meld") nil))
      (should-error
       (majutsu-diff-editor-start "restore" '("--tool" "meld") nil)
       :type 'user-error)
      (should (eq (majutsu-diff-editor--complete-session session) 'unchanged))
      (should
       (majutsu-diff-editor-start "restore" '("--tool" "meld") nil)))))

(ert-deftest majutsu-diff-editor-start/quit-releases-an-unstarted-session ()
  "C-g during startup must not leave a repository session lock behind."
  (let ((majutsu-diff-editor--live-sessions (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-diff-editor-select-host)
               (lambda (&rest _) 'ghostel))
              ((symbol-function 'majutsu-diff-editor--operation-id)
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
              ((symbol-function 'process-get)
               (lambda (_process property)
                 (and (eq property 'finish-callback) #'ignore)))
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

(ert-deftest majutsu-diff-editor-complete-session/refreshes-only-after-operation-change ()
  "Keep a cancelled editor's selection; clear stale selections after a change."
  (let ((origin (generate-new-buffer " *majutsu-diff-editor-origin*"))
        invalidated refreshed ids)
    (unwind-protect
        (with-current-buffer origin
          (majutsu-diff-mode)
          (cl-letf (((symbol-function 'majutsu-diff-editor--operation-id)
                     (lambda (&rest _)
                       (pop ids)))
                    ((symbol-function 'majutsu-interactive-invalidate)
                     (lambda () (setq invalidated (1+ (or invalidated 0)))))
                    ((symbol-function 'majutsu-refresh)
                     (lambda () (setq refreshed (1+ (or refreshed 0)))))
                    ((symbol-function 'message) (lambda (&rest _) nil)))
            (setq ids '("before"))
            (should
             (eq (majutsu-diff-editor--complete-session
                  (majutsu-diff-editor-session-create
                   :origin-buffer origin :repository-root default-directory
                   :operation-id-before "before"))
                 'unchanged))
            (should-not invalidated)
            (should-not refreshed)
            (setq ids '("after"))
            (should
             (eq (majutsu-diff-editor--complete-session
                  (majutsu-diff-editor-session-create
                   :origin-buffer origin :repository-root default-directory
                   :operation-id-before "before"))
                 'changed))
            (should (= invalidated 1))
            (should (= refreshed 1))
            (setq ids '(nil))
            (should
             (eq (majutsu-diff-editor--complete-session
                  (majutsu-diff-editor-session-create
                   :origin-buffer origin :repository-root default-directory
                   :operation-id-before "before"))
                 'unknown))
            (should (= invalidated 2))
            (should (= refreshed 2))))
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest majutsu-diff-editor-complete-session/quit-releases-and-invalidates ()
  "C-g during the operation probe leaves no lock or reusable patch selection."
  (let* ((majutsu-diff-editor--live-sessions (make-hash-table :test 'equal))
         (session (majutsu-diff-editor-session-create
                   :repository-root "/repo/" :operation-id-before "before"))
         invalidated scheduled)
    (puthash "/repo/" session majutsu-diff-editor--live-sessions)
    (cl-letf (((symbol-function 'majutsu-diff-editor--operation-id)
               (lambda (&rest _) (signal 'quit nil)))
              ((symbol-function 'majutsu-diff-editor--invalidate-origin-selection)
               (lambda (_session) (setq invalidated t)))
              ((symbol-function 'run-at-time)
               (lambda (&rest args) (setq scheduled args))))
      (let (quit)
        (condition-case nil
            (majutsu-diff-editor--complete-session session)
          (quit (setq quit t)))
        (should quit))
      (should invalidated)
      (should scheduled)
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
      (cl-letf (((symbol-function 'majutsu-diff-editor--operation-id)
                 (lambda (&rest _) "before"))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (should (eq (majutsu-diff-editor--complete-session session "finished")
                    'unchanged))
        (should-not hook-saw-slot)))))

(provide 'majutsu-diff-editor-test)
;;; majutsu-diff-editor-test.el ends here
