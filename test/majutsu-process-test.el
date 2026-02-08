;;; majutsu-process-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'majutsu-process)

(ert-deftest test-majutsu-process-error-summary-from-string/error ()
  (should (equal (majutsu--process-error-summary-from-string "Error: something went wrong\n")
                 "something went wrong")))

(ert-deftest test-majutsu-process-error-summary-from-string/error-lowercase ()
  (should (equal (majutsu--process-error-summary-from-string "error: nope\n")
                 "nope")))

(ert-deftest test-majutsu-process-error-summary-from-string/fatal ()
  (should (equal (majutsu--process-error-summary-from-string "fatal: bad\n")
                 "bad")))

(ert-deftest test-majutsu-process-error-summary-from-string/multi-line ()
  (let ((out (string-join '("some output"
                            "warning: ignore this"
                            "Error: actual issue"
                            "")
                          "\n")))
    (should (equal (majutsu--process-error-summary-from-string out)
                   "actual issue"))))

(ert-deftest test-majutsu-process-error-summary-from-string/empty ()
  (should-not (majutsu--process-error-summary-from-string "")))

(ert-deftest test-majutsu-process-filter/calls-magit-prompt ()
  (let ((called nil))
    (cl-letf (((symbol-function 'magit-process-password-prompt)
               (lambda (_proc _string)
                 (setq called 'password))))
      (with-temp-buffer
        (let ((proc (make-process :name "majutsu-test"
                                  :buffer (current-buffer)
                                  :command (list "cat"))))
          (set-marker (process-mark proc) (point))
          (majutsu--process-filter proc "Password: ")
          (delete-process proc)))
      (should (eq called 'password)))))

(ert-deftest majutsu-process-test-start-jj-binds-root-cwd ()
  "`majutsu-start-jj' should run from repo root."
  (let ((default-directory "/repo/sub/")
        seen-root)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-start-process)
               (lambda (&rest _args)
                  (setq seen-root default-directory)
                  'dummy-process)))
      (should (eq (majutsu-start-jj '("status")) 'dummy-process))
      (should (equal seen-root "/repo/")))))

(ert-deftest majutsu-process-test-call-jj-runs-at-root ()
  "`majutsu-call-jj' should execute jj in repo root.
The process section should use root as command directory."
  (let ((default-directory "/repo/sub/")
        seen-process-cwd
        seen-finish-root
        seen-prefix-pwd)
    (with-temp-buffer
      (let ((process-buf (current-buffer)))
        (setq default-directory "/repo/")
        (cl-letf (((symbol-function 'majutsu--toplevel-safe)
                   (lambda (&optional _directory) "/repo/"))
                  ((symbol-function 'majutsu-process-jj-arguments)
                   (lambda (args) args))
                  ((symbol-function 'majutsu-process-buffer)
                   (lambda (&optional _nodisplay)
                     process-buf))
                  ((symbol-function 'majutsu--process-insert-section)
                   (lambda (pwd _program _args &optional _err _errlog _face)
                     (setq seen-prefix-pwd pwd)
                     (insert "\n")
                     'dummy-section))
                  ((symbol-function 'process-file)
                   (lambda (&rest _args)
                     (setq seen-process-cwd default-directory)
                     0))
                  ((symbol-function 'majutsu-process-finish)
                   (lambda (exit _process-buf _command-buf default-dir _section)
                     (setq seen-finish-root default-dir)
                     exit)))
          (should (= 0 (majutsu-call-jj "status"))))))
    (should (equal seen-prefix-pwd "/repo/"))
    (should (equal seen-process-cwd "/repo/"))
    (should (equal seen-finish-root "/repo/"))))

;;; majutsu-process-test.el ends here
