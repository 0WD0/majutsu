;;; majutsu-process-test.el -*- lexical-binding: t; -*-

(require 'ert)
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

;;; majutsu-process-test.el ends here

