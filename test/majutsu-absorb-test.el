;;; majutsu-absorb-test.el --- Tests for absorb transient  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'majutsu-absorb)

(ert-deftest majutsu-absorb-default-args-from-diff-revisions ()
  "Use diff --revisions context as default --from for absorb."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=abc123"))
    (let ((transient--original-buffer (current-buffer)))
      (should (equal (majutsu-absorb--default-args)
                     '("--from=abc123"))))))

(ert-deftest majutsu-absorb-default-args-from-diff-from ()
  "Keep --from from diff range when available."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--from=main" "--to=@"))
    (let ((transient--original-buffer (current-buffer)))
      (should (equal (majutsu-absorb--default-args)
                     '("--from=main"))))))

(ert-deftest majutsu-absorb-arguments-use-transient-args ()
  "Use current transient args when absorb transient is active."
  (let ((transient-current-command 'majutsu-absorb))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (&rest _) '("--from=@" "--into=mutable()"))))
      (should (equal (majutsu-absorb-arguments)
                     '("--from=@" "--into=mutable()"))))))

(ert-deftest majutsu-absorb-arguments-default-from-point ()
  "Outside transient, default --from to commit at point."
  (let ((transient-current-command nil))
    (cl-letf (((symbol-function 'magit-section-value-if)
               (lambda (&rest _) "kxzptvry")))
      (should (equal (majutsu-absorb-arguments)
                     '("--from=kxzptvry"))))))

(ert-deftest majutsu-absorb-execute-runs-jj-absorb ()
  "Execute absorb by dispatching to `majutsu-run-jj'."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (majutsu-absorb-execute '("--from=@" "--into=mutable()"))
      (should (equal called
                     '("absorb" "--from=@" "--into=mutable()"))))))

(provide 'majutsu-absorb-test)
;;; majutsu-absorb-test.el ends here
