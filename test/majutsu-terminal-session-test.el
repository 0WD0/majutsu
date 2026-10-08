;;; majutsu-terminal-session-test.el --- Tests for jj terminal sessions  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for Ghostel hosting, repository locking, and completion.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-terminal-session)
(require 'majutsu-diff-editor)

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
        (majutsu-process--interactive-sessions
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
                     (gethash root majutsu-process--interactive-sessions))
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
        (majutsu-process-unregister-interactive-session
         (majutsu-terminal-session-repository-root session) session))
      (when (buffer-live-p terminal)
        (with-current-buffer terminal
          (setq-local majutsu-terminal-session--session nil)
          (setq-local kill-buffer-hook nil)
          (setq-local ghostel-exit-functions nil))
        (kill-buffer terminal)))))

(ert-deftest majutsu-terminal-session-start-jj/default-path-does-not-install-jj-editor ()
  "The public generic start path does not opt into the editor protocol."
  (let ((majutsu-process--interactive-sessions
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
        (majutsu-process-unregister-interactive-session
         (majutsu-terminal-session-repository-root session) session))
      (when (buffer-live-p terminal)
        (with-current-buffer terminal
          (setq-local majutsu-terminal-session--session nil)
          (setq-local kill-buffer-hook nil)
          (setq-local ghostel-exit-functions nil))
        (kill-buffer terminal)))))

(ert-deftest majutsu-terminal-session-complete/is-atomic-and-releases-before-observers ()
  "Freshness is checked once and observers run after mutex release."
  (let* ((majutsu-process--interactive-sessions
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
                            majutsu-process--interactive-sessions))))))
    (majutsu-process-register-interactive-session
     (majutsu-terminal-session-repository-root session) session)
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
                         majutsu-process--interactive-sessions))))

(ert-deftest majutsu-terminal-session-complete/notifies-terminal-kill-once ()
  "Terminal observers run once even when completion has no Ghostel event."
  (let* ((majutsu-process--interactive-sessions
          (make-hash-table :test 'equal))
         exit-events hook-events
         (session
          (majutsu-terminal-session-create
           :repository-root "/repo/"
           :terminal-p t
           :exit-function
           (lambda (_session event) (push event exit-events)))))
    (majutsu-process-register-interactive-session
     (majutsu-terminal-session-repository-root session) session)
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
  (let ((majutsu-process--interactive-sessions
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
                               majutsu-process--interactive-sessions))
          (should-not (buffer-live-p terminal)))))))

(ert-deftest majutsu-terminal-session-finish-after-kill/waits-for-reaper ()
  "Completion waits for the live reaper, then runs after it stops."
  (let ((session (majutsu-terminal-session-create :process 'reaper))
        (live t)
        scheduled completed)
    (cl-letf (((symbol-function 'processp) (lambda (_process) t))
              ((symbol-function 'process-live-p) (lambda (_process) live))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat callback &rest args)
                 (setq scheduled (cons callback args))))
              ((symbol-function 'majutsu-terminal-session--finish)
               (lambda (value &rest _) (setq completed value))))
      (majutsu-terminal-session--finish-after-kill session)
      (should-not completed)
      (should scheduled)
      (setq live nil)
      (apply (car scheduled) (cdr scheduled))
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

(ert-deftest majutsu-terminal-session-complete/observers-survive-probe-failure ()
  "Exit function and hook run once even when the completion probe errors."
  (let ((majutsu-process--interactive-sessions (make-hash-table :test 'equal))
        (exit-calls 0)
        (hook-calls 0)
        (session (majutsu-terminal-session-create
                  :repository-root "/tmp/repo/"
                  :origin-buffer (current-buffer)
                  :operation-id-before "op"
                  :terminal-p t)))
    (majutsu-process-register-interactive-session "/tmp/repo/" session)
    (setf (majutsu-terminal-session-exit-function session)
          (lambda (_session _event) (cl-incf exit-calls)))
    (cl-letf (((symbol-function 'majutsu-interactive-complete-repository-operation)
               (lambda (&rest _) (error "probe failed"))))
      (let ((majutsu-terminal-session-exit-hook
             (list (lambda (_session _event) (cl-incf hook-calls)))))
        (should-error (majutsu-terminal-session--complete session nil))
        (should-not (gethash "/tmp/repo/" majutsu-process--interactive-sessions))
        (should (= exit-calls 1))
        (should (= hook-calls 1))
        ;; The completed-p latch keeps later finish paths idempotent.
        (should-not (majutsu-terminal-session--complete session nil))
        (should (= exit-calls 1))
        (should (= hook-calls 1))))))

(provide 'majutsu-terminal-session-test)
;;; majutsu-terminal-session-test.el ends here
