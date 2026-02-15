;;; majutsu-revert-test.el --- Tests for revert transient  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for revert argument parsing and command assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-revert)

(ert-deftest majutsu-revert-arguments-use-transient-args ()
  "Use transient args when revert transient is active."
  (let ((transient-current-command 'majutsu-revert))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (&rest _) '("--revisions=abc123" "--onto=@"))))
      (should (equal (majutsu-revert-arguments)
                     '("--revisions=abc123" "--onto=@"))))))

(ert-deftest majutsu-revert-arguments-no-implicit-destination-in-transient ()
  "Do not auto-fill destination from point when transient is active."
  (let ((transient-current-command 'majutsu-revert))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (&rest _) '("--revisions=abc123")))
              ((symbol-function 'magit-section-value-if)
               (lambda (&rest _) "kxzptvry")))
      (should (equal (majutsu-revert-arguments)
                     '("--revisions=abc123"))))))

(ert-deftest majutsu-revert-arguments-default-from-point ()
  "Outside transient, default source and destination from point."
  (let ((transient-current-command nil))
    (cl-letf (((symbol-function 'magit-section-value-if)
               (lambda (&rest _) "kxzptvry")))
      (should (equal (majutsu-revert-arguments)
                     '("--revisions=kxzptvry" "--insert-after=kxzptvry"))))))

(ert-deftest majutsu-revert-arguments-keep-existing-destination ()
  "Do not add default insert-after when destination already exists."
  (let ((transient-current-command 'majutsu-revert))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (&rest _) '("--revisions=abc123" "--onto=main")))
              ((symbol-function 'magit-section-value-if)
               (lambda (&rest _) "ignored")))
      (should (equal (majutsu-revert-arguments)
                     '("--revisions=abc123" "--onto=main"))))))

(ert-deftest majutsu-revert-execute-runs-jj-revert ()
  "Execute revert by dispatching to `majutsu-run-jj'."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (majutsu-revert-execute '("--revisions=abc123" "--onto=main"))
      (should (equal called '("revert" "--revisions=abc123" "--onto=main"))))))

(ert-deftest majutsu-revert-execute-requires-revision ()
  "Do not run command without source revisions."
  (let (seen-message)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest _)
                 (ert-fail "should not execute jj revert without revisions")))
              ((symbol-function 'majutsu--message-with-log)
               (lambda (fmt &rest args)
                 (setq seen-message (apply #'format fmt args)))))
      (majutsu-revert-execute '("--onto=main"))
      (should (string-match-p "source revisions" seen-message)))))

(ert-deftest majutsu-revert-execute-requires-destination ()
  "Do not run command without destination placement."
  (let (seen-message)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest _)
                 (ert-fail "should not execute jj revert without destination")))
              ((symbol-function 'majutsu--message-with-log)
               (lambda (fmt &rest args)
                 (setq seen-message (apply #'format fmt args)))))
      (majutsu-revert-execute '("--revisions=abc123"))
      (should (string-match-p "destination" seen-message)))))

(provide 'majutsu-revert-test)
;;; majutsu-revert-test.el ends here
