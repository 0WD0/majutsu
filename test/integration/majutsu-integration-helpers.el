;;; majutsu-integration-helpers.el --- Helpers for real jj integration tests  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Helpers for opt-in integration tests that run a real jj executable in
;; isolated temporary repositories.  Set MAJUTSU_TEST_JJ to choose a forked or
;; otherwise non-default executable.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'majutsu)
(require 'majutsu-jj)

(defconst majutsu-integration-test-enabled-env "MAJUTSU_TEST_REAL_JJ"
  "Environment variable that enables real jj integration tests.")

(defconst majutsu-integration-test-keep-env "MAJUTSU_TEST_KEEP_REPOS"
  "Environment variable that keeps temporary integration repositories.")

(defconst majutsu-integration-test-jj-env "MAJUTSU_TEST_JJ"
  "Environment variable that selects the jj executable for integration tests.")

(defun majutsu-integration-test-enabled-p ()
  "Return non-nil when real jj integration tests should run."
  (let ((value (getenv majutsu-integration-test-enabled-env)))
    (and value (not (string-empty-p value)) (not (equal value "0")))))

(defun majutsu-integration-jj-executable ()
  "Return the jj executable selected for integration tests."
  (or (getenv majutsu-integration-test-jj-env)
      majutsu-jj-executable))

(defun majutsu-integration-jj-available-p ()
  "Return non-nil when the integration jj executable is available."
  (and (executable-find (majutsu-integration-jj-executable)) t))

(defun majutsu-integration-skip-unless-enabled ()
  "Skip the current test unless real jj integration tests are enabled."
  (unless (majutsu-integration-test-enabled-p)
    (ert-skip (format "Set %s=1 to run real jj integration tests"
                      majutsu-integration-test-enabled-env)))
  (unless (majutsu-integration-jj-available-p)
    (ert-skip (format "Cannot find jj executable: %s"
                      (majutsu-integration-jj-executable)))))

(defmacro majutsu-integration-with-temp-dir (&rest body)
  "Run BODY in an isolated temporary directory bound to `default-directory'."
  (declare (indent 0) (debug (body)))
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (file-name-as-directory (make-temp-file "majutsu-integration-" t))))
       (unwind-protect
           (condition-case err
               (let ((default-directory ,dir)
                     (majutsu-jj-executable (majutsu-integration-jj-executable))
                     (majutsu-jj-global-arguments '("--no-pager" "--color=never"))
                     (process-environment
                      (append '("JJ_CONFIG=/dev/null"
                                "JJ_USER=Majutsu Test"
                                "JJ_EMAIL=majutsu-test@example.invalid"
                                "NO_COLOR=1")
                              process-environment)))
                 ,@body)
             (error
              (message "Keeping Majutsu integration test directory: %s" ,dir)
              (signal (car err) (cdr err))))
         (unless (getenv majutsu-integration-test-keep-env)
           (ignore-errors (delete-directory ,dir t)))))))

(defun majutsu-integration-run-jj (&rest args)
  "Run real jj with ARGS in `default-directory' and return stdout.
Signal a test failure when jj exits non-zero."
  (with-temp-buffer
    (let* ((err-file (make-temp-file "majutsu-integration-jj-err"))
           (exit (unwind-protect
                     (let ((coding-system-for-read 'utf-8-unix)
                           (coding-system-for-write 'utf-8-unix))
                       (apply #'process-file
                              (majutsu-jj--executable)
                              nil
                              (list t err-file)
                              nil
                              (append majutsu-jj-global-arguments args)))
                   nil))
           (stdout (buffer-string))
           (stderr (with-temp-buffer
                     (ignore-errors (insert-file-contents err-file))
                     (buffer-string))))
      (ignore-errors (delete-file err-file))
      (unless (zerop exit)
        (ert-fail (format "jj %S exited %s\nstdout:\n%s\nstderr:\n%s"
                          args exit stdout stderr)))
      stdout)))

(defmacro majutsu-integration-with-jj-repo (&rest body)
  "Run BODY inside a newly initialized real jj repository."
  (declare (indent 0) (debug (body)))
  (let ((repo (make-symbol "repo")))
    `(progn
       (majutsu-integration-skip-unless-enabled)
       (majutsu-integration-with-temp-dir
         (let ((,repo (file-name-as-directory
                       (expand-file-name "repo" default-directory))))
           (make-directory ,repo)
           (let ((default-directory ,repo))
             (majutsu-integration-run-jj "git" "init" ".")
             ,@body))))))

(defun majutsu-integration-write-file (file content)
  "Write CONTENT to FILE below `default-directory'."
  (let ((path (expand-file-name file default-directory)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert content))))

(defun majutsu-integration-create-change (name &optional file content)
  "Create a new jj change named NAME, optionally writing FILE CONTENT."
  (majutsu-integration-run-jj "new" "-m" name)
  (when file
    (majutsu-integration-write-file file (or content (concat name "\n"))))
  (string-trim (majutsu-integration-run-jj "log" "-r" "@" "--no-graph" "-T" "change_id.shortest(8)")))

(provide 'majutsu-integration-helpers)
;;; majutsu-integration-helpers.el ends here
