;;; majutsu-terminal-session-test.el --- Tests for jj terminal sessions  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for Ghostel hosting, repository locking, and completion.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-terminal-session)
(require 'majutsu-diff-editor)

(ert-deftest majutsu-terminal-session-struct/exposes-lifecycle-state ()
  "The generic session records all state needed by terminal adapters."
  (let* ((origin (current-buffer))
         (exec #'ignore)
         (exit #'ignore)
         (session
          (majutsu-terminal-session-create
           :argv '("arrange" "mine()")
           :origin-buffer origin
           :repository-root "/repo/"
           :title "arrange"
           :terminal-buffer origin
           :process 'process
           :operation-id-before "before"
           :completed-p t
           :unchanged-message "unchanged"
           :exec-function exec
           :exit-function exit
           :terminal-p t)))
    (should (equal (majutsu-terminal-session-argv session)
                   '("arrange" "mine()")))
    (should (eq (majutsu-terminal-session-origin-buffer session) origin))
    (should (equal (majutsu-terminal-session-repository-root session)
                   "/repo/"))
    (should (equal (majutsu-terminal-session-title session) "arrange"))
    (should (eq (majutsu-terminal-session-terminal-buffer session) origin))
    (should (eq (majutsu-terminal-session-process session) 'process))
    (should (equal (majutsu-terminal-session-operation-id-before session)
                   "before"))
    (should (majutsu-terminal-session-completed-p session))
    (should (equal (majutsu-terminal-session-unchanged-message session)
                   "unchanged"))
    (should (eq (majutsu-terminal-session-exec-function session) exec))
    (should (eq (majutsu-terminal-session-exit-function session) exit))
    (should (majutsu-terminal-session-terminal-p session))))

(ert-deftest majutsu-terminal-session-available-p/requires-public-ghostel-api ()
  "Availability depends on Ghostel and both public entry points."
  (cl-letf (((symbol-function 'require)
             (lambda (feature &optional _filename _noerror)
               (eq feature 'ghostel)))
            ((symbol-function 'ghostel-exec) #'ignore)
            ((symbol-function 'ghostel-mode) #'ignore))
    (should (majutsu-terminal-session-available-p)))
  (cl-letf (((symbol-function 'require) (lambda (&rest _) nil)))
    (should-not (majutsu-terminal-session-available-p))))

(ert-deftest majutsu-terminal-session-start-jj/rejects-before-repository-probe ()
  "Missing Ghostel fails before probing jj or creating a terminal buffer."
  (let ((before (buffer-list)))
    (cl-letf (((symbol-function 'majutsu-terminal-session-available-p)
               (lambda () nil))
              ((symbol-function 'majutsu--toplevel-safe)
               (lambda (&rest _)
                 (ert-fail "Repository root was probed")))
              ((symbol-function 'majutsu-jj-operation-id)
               (lambda (&rest _)
                 (ert-fail "Operation id was probed"))))
      (should-error
       (majutsu-terminal-session-start-jj '("arrange"))
       :type 'user-error))
    (should (equal (buffer-list) before))))

(ert-deftest majutsu-terminal-session-start-jj/rejects-invalid-argv ()
  "Only a nonempty proper list of jj argument strings is accepted."
  (dolist (argv '(nil "arrange" ("arrange" 1) ("arrange" . "@")))
    (should-error
     (majutsu-terminal-session-start-jj argv)
     :type 'user-error)))

(ert-deftest majutsu-terminal-session-start-jj/preserves-tramp-root-and-argv ()
  "Remote roots stay remote and jj arguments remain separate tokens."
  (let ((root "/ssh:guix:/tmp/majutsu-arrange/")
        (majutsu-terminal-session--live-sessions
         (make-hash-table :test 'equal))
        terminal session seen)
    (unwind-protect
        (with-temp-buffer
          (let ((origin (current-buffer))
                (default-directory "/ssh:guix:/tmp/majutsu-arrange/subdir/"))
            (cl-letf
                (((symbol-function 'majutsu-terminal-session-available-p)
                  (lambda () t))
                 ((symbol-function 'majutsu--buffer-root)
                  (lambda (&optional _buffer) root))
                 ((symbol-function 'majutsu-jj-operation-id)
                  (lambda (directory &optional _snapshot)
                    (should (equal directory root))
                    (should
                     (gethash root majutsu-terminal-session--live-sessions))
                    "before"))
                 ((symbol-function 'majutsu-jj--executable)
                  (lambda () "jj"))
                 ((symbol-function 'majutsu-process-jj-arguments)
                  (lambda (args) (cons "--global" args)))
                 ((symbol-function 'ghostel-mode)
                  (lambda ()
                    (kill-all-local-variables)
                    (setq major-mode 'ghostel-mode)
                    (setq-local ghostel-exit-functions nil)
                    (setq-local ghostel-kill-buffer-on-exit t)))
                 ((symbol-function 'majutsu-display-buffer) #'ignore)
                 ((symbol-function 'processp)
                  (lambda (value) (eq value 'ghostel-process))))
              (setq
               session
               (majutsu-terminal-session-start-jj
                '("arrange" "roots(foo)" "bar | baz")
                :origin-buffer origin
                :title "arrange"
                :unchanged-message "unchanged"
                :exec-function
                (lambda (value buffer program args)
                  (setq terminal buffer
                        seen (list value program args default-directory))
                  'ghostel-process)))
              (should (eq (car seen) session))
              (should (equal (nth 1 seen) "jj"))
              (should (equal (nth 2 seen)
                             '("--global" "arrange" "roots(foo)"
                               "bar | baz")))
              (should (equal (nth 3 seen) root))
              (should (equal (majutsu-terminal-session-argv session)
                             '("arrange" "roots(foo)" "bar | baz")))
              (should (equal
                       (majutsu-terminal-session-repository-root session)
                       root))
              (should (equal
                       (buffer-local-value 'default-directory terminal)
                       root))
              (should (equal
                       (majutsu-terminal-session-operation-id-before session)
                       "before"))
              (should (eq (majutsu-terminal-session-process session)
                          'ghostel-process)))))
      (when session
        (majutsu-terminal-session-unregister root session))
      (when (buffer-live-p terminal)
        (with-current-buffer terminal
          (setq-local majutsu-terminal-session--session nil)
          (setq-local kill-buffer-hook nil)
          (setq-local ghostel-exit-functions nil))
        (kill-buffer terminal)))))

(ert-deftest majutsu-terminal-session-default-exec/does-not-install-jj-editor ()
  "A generic TUI inherits Majutsu's environment without with-editor setup."
  (let (seen-environment seen-args)
    (cl-letf (((symbol-function 'majutsu-process-environment)
               (lambda (args)
                 (should (equal args '("arrange")))
                 '("INSIDE_EMACS=test" "TERM=xterm-ghostel")))
              ((symbol-function 'ghostel-exec)
               (lambda (_buffer _program args)
                 (setq seen-environment process-environment
                       seen-args args)
                 'process)))
      (should
       (eq (majutsu-terminal-session--default-exec
            (majutsu-terminal-session-create)
            (current-buffer) "jj" '("arrange"))
           'process))
      (should (equal seen-args '("arrange")))
      (should (equal seen-environment
                     '("INSIDE_EMACS=test" "TERM=xterm-ghostel")))
      (should-not
       (seq-some (lambda (entry) (string-prefix-p "JJ_EDITOR=" entry))
                 seen-environment)))))

(ert-deftest majutsu-terminal-session-start-jj/default-path-does-not-install-jj-editor ()
  "The public generic start path does not opt into the editor protocol."
  (let ((majutsu-terminal-session--live-sessions
         (make-hash-table :test 'equal))
        terminal session seen-environment)
    (unwind-protect
        (with-temp-buffer
          (let ((origin (current-buffer)))
            (cl-letf
                (((symbol-function 'majutsu-terminal-session-available-p)
                  (lambda () t))
                 ((symbol-function 'majutsu--buffer-root)
                  (lambda (&optional _buffer) "/repo/"))
                 ((symbol-function 'majutsu-jj-operation-id)
                  (lambda (&rest _) "before"))
                 ((symbol-function 'majutsu-jj--executable)
                  (lambda () "jj"))
                 ((symbol-function 'majutsu-process-jj-arguments)
                  (lambda (args) args))
                 ((symbol-function 'majutsu-process-environment)
                  (lambda (_args)
                    '("INSIDE_EMACS=test" "TERM=xterm-ghostel")))
                 ((symbol-function 'ghostel-mode)
                  (lambda ()
                    (kill-all-local-variables)
                    (setq major-mode 'ghostel-mode)
                    (setq-local ghostel-exit-functions nil)
                    (setq-local ghostel-kill-buffer-on-exit t)))
                 ((symbol-function 'majutsu-display-buffer) #'ignore)
                 ((symbol-function 'ghostel-exec)
                  (lambda (buffer _program _args)
                    (setq terminal buffer
                          seen-environment process-environment)
                    'ghostel-process))
                 ((symbol-function 'processp)
                  (lambda (value) (eq value 'ghostel-process))))
              (setq session
                    (majutsu-terminal-session-start-jj
                     '("arrange") :origin-buffer origin))
              (should (equal seen-environment
                             '("INSIDE_EMACS=test" "TERM=xterm-ghostel")))
              (should-not
               (seq-some
                (lambda (entry) (string-prefix-p "JJ_EDITOR=" entry))
                seen-environment)))))
      (when session
        (majutsu-terminal-session-unregister "/repo/" session))
      (when (buffer-live-p terminal)
        (with-current-buffer terminal
          (setq-local majutsu-terminal-session--session nil)
          (setq-local kill-buffer-hook nil)
          (setq-local ghostel-exit-functions nil))
        (kill-buffer terminal)))))

(ert-deftest majutsu-terminal-session-register/is-an-owner-aware-workspace-mutex ()
  "Only the current owner can release a workspace's shared terminal slot."
  (let ((majutsu-terminal-session--live-sessions
         (make-hash-table :test 'equal))
        (first (list 'first))
        (second (list 'second)))
    (majutsu-terminal-session-register "/repo/" first)
    (should-error
     (majutsu-terminal-session-register "/repo/" second)
     :type 'user-error)
    (majutsu-terminal-session-unregister "/repo/" second)
    (should (eq (gethash "/repo/"
                         majutsu-terminal-session--live-sessions)
                first))
    (majutsu-terminal-session-unregister "/repo/" first)
    (should-not (gethash "/repo/"
                         majutsu-terminal-session--live-sessions))))

(ert-deftest majutsu-terminal-session-register/is-shared-with-diff-editor ()
  "Arrange and diff-editor sessions contend for the same workspace slot."
  (let ((majutsu-terminal-session--live-sessions
         (make-hash-table :test 'equal))
        ;; If the legacy table still exists, isolate it so this test cannot
        ;; accidentally pass due to state left by another test.
        (majutsu-diff-editor--live-sessions
         (make-hash-table :test 'equal))
        (terminal-owner (list 'arrange))
        (diff-owner
         (majutsu-diff-editor-session-create :repository-root "/repo/")))
    (majutsu-terminal-session-register "/repo/" terminal-owner)
    (should-error
     (majutsu-diff-editor--register-session diff-owner)
     :type 'user-error)
    (majutsu-terminal-session-unregister "/repo/" terminal-owner)
    (majutsu-diff-editor--register-session diff-owner)
    (should (eq (gethash "/repo/"
                         majutsu-terminal-session--live-sessions)
                diff-owner))
    (majutsu-diff-editor--unregister-session diff-owner)
    (should-not (gethash "/repo/"
                         majutsu-terminal-session--live-sessions))))

(ert-deftest majutsu-terminal-session-complete/is-atomic-and-releases-before-observers ()
  "Freshness is checked once and observers run after mutex release."
  (let* ((majutsu-terminal-session--live-sessions
          (make-hash-table :test 'equal))
         (completion-count 0)
         (exit-count 0)
         observer-owner
         (session
          (majutsu-terminal-session-create
           :repository-root "/repo/"
           :operation-id-before "before"
           :unchanged-message "unchanged"
           :exit-function
           (lambda (_session _event)
             (setq exit-count (1+ exit-count)
                   observer-owner
                   (gethash "/repo/"
                            majutsu-terminal-session--live-sessions))))))
    (majutsu-terminal-session-register "/repo/" session)
    (cl-letf (((symbol-function
                'majutsu-interactive-complete-repository-operation)
               (lambda (&rest args)
                 (setq completion-count (1+ completion-count))
                 (should (equal args
                                '("/repo/" nil "before" "unchanged")))
                 'unchanged)))
      (should (eq (majutsu-terminal-session--complete session "finished")
                  'unchanged))
      (should-not (majutsu-terminal-session--complete session "duplicate")))
    (should (= completion-count 1))
    (should (= exit-count 1))
    (should-not observer-owner)
    (should (majutsu-terminal-session-completed-p session))
    (should-not (gethash "/repo/"
                         majutsu-terminal-session--live-sessions))))

(ert-deftest majutsu-terminal-session-complete/notifies-terminal-kill-once ()
  "Terminal observers run once even when completion has no Ghostel event."
  (let* ((majutsu-terminal-session--live-sessions
          (make-hash-table :test 'equal))
         exit-events hook-events
         (session
          (majutsu-terminal-session-create
           :repository-root "/repo/"
           :terminal-p t
           :exit-function
           (lambda (_session event) (push event exit-events)))))
    (majutsu-terminal-session-register "/repo/" session)
    (let ((majutsu-terminal-session-exit-hook
           (list (lambda (_session event) (push event hook-events)))))
      (cl-letf (((symbol-function
                  'majutsu-interactive-complete-repository-operation)
                 (lambda (&rest _) 'unchanged)))
        (should (eq (majutsu-terminal-session--complete session nil)
                    'unchanged))
        (should-not (majutsu-terminal-session--complete session nil))))
    (should (equal exit-events '(nil)))
    (should (equal hook-events '(nil)))))

(ert-deftest majutsu-terminal-session-start-jj/rejects-missing-lifecycle-process ()
  "A broken exec adapter cannot strand its buffer or workspace mutex."
  (let ((majutsu-terminal-session--live-sessions
         (make-hash-table :test 'equal))
        terminal)
    (with-temp-buffer
      (let ((origin (current-buffer)))
        (cl-letf (((symbol-function 'majutsu-terminal-session-available-p)
                   (lambda () t))
                  ((symbol-function 'majutsu--buffer-root)
                   (lambda (&optional _buffer) "/repo/"))
                  ((symbol-function 'majutsu-jj-operation-id)
                   (lambda (&rest _) "before"))
                  ((symbol-function 'majutsu-jj--executable)
                   (lambda () "jj"))
                  ((symbol-function 'majutsu-process-jj-arguments) #'identity)
                  ((symbol-function 'ghostel-mode)
                   (lambda ()
                     (kill-all-local-variables)
                     (setq major-mode 'ghostel-mode)
                     (setq-local ghostel-exit-functions nil)))
                  ((symbol-function 'majutsu-display-buffer)
                   (lambda (buffer) (setq terminal buffer))))
          (should-error
           (majutsu-terminal-session-start-jj
            '("arrange") :origin-buffer origin
            :exec-function (lambda (&rest _) nil)))
          (should-not (gethash "/repo/"
                               majutsu-terminal-session--live-sessions))
          (should-not (buffer-live-p terminal)))))))

(ert-deftest majutsu-terminal-session-finish-after-kill/waits-for-live-reaper ()
  "Do not compare operation ids until Ghostel's reaper has exited."
  (let ((session (majutsu-terminal-session-create :process 'reaper))
        scheduled completed)
    (cl-letf (((symbol-function 'processp) (lambda (_process) t))
              ((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'run-at-time)
               (lambda (&rest args) (setq scheduled args)))
              ((symbol-function 'majutsu-terminal-session--finish)
               (lambda (&rest _) (setq completed t))))
      (majutsu-terminal-session--finish-after-kill session)
      (should-not completed)
      (should (equal scheduled
                     (list 0.05 nil
                           #'majutsu-terminal-session--finish-after-kill
                           session))))))

(ert-deftest majutsu-terminal-session-finish-after-kill/completes-after-reaper ()
  "A stopped lifecycle process completes the session immediately."
  (let ((session (majutsu-terminal-session-create :process 'reaper))
        completed)
    (cl-letf (((symbol-function 'processp) (lambda (_process) t))
              ((symbol-function 'process-live-p) (lambda (_process) nil))
              ((symbol-function 'majutsu-terminal-session--finish)
               (lambda (value &rest _) (setq completed value))))
      (majutsu-terminal-session--finish-after-kill session)
      (should (eq completed session)))))

(ert-deftest majutsu-terminal-session-ghostel-exit/defers-without-exit-status ()
  "Ghostel completion uses its event and never interprets a process status."
  (let* ((buffer (generate-new-buffer " *majutsu-ghostel-exit*"))
         (session (majutsu-terminal-session-create
                   :repository-root "/repo/"))
         scheduled
         completed)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local majutsu-terminal-session--session session))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest args) (setq scheduled args))))
            (majutsu-terminal-session--ghostel-exit buffer "finished\n"))
          (should (equal scheduled
                         (list 0 nil
                               #'majutsu-terminal-session--finish
                               session "finished\n")))
          (cl-letf (((symbol-function 'process-exit-status)
                     (lambda (&rest _)
                       (ert-fail "Ghostel completion read process status")))
                    ((symbol-function 'majutsu-terminal-session--complete)
                     (lambda (&rest args) (setq completed args))))
            (apply (nth 2 scheduled) (nthcdr 3 scheduled)))
          (should (equal completed (list session "finished\n"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'majutsu-terminal-session-test)
;;; majutsu-terminal-session-test.el ends here
