;;; majutsu-simplify-parents-test.el --- Tests for simplify-parents transient  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for simplify-parents argument handling and command assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-simplify-parents)

(ert-deftest majutsu-simplify-parents-execute-runs-jj-with-args ()
  "Execute simplify-parents by dispatching to `majutsu-run-jj'."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (majutsu-simplify-parents-execute
       '("--source=main | dev" "--revision=mutable()"))
      (should (equal called
                     '("simplify-parents"
                       "--source=main | dev"
                       "--revision=mutable()"))))))

(ert-deftest majutsu-simplify-parents-execute-adds-dwim-args ()
  "Execute simplify-parents with DWIM target args."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'magit-region-values)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-thing-at-point)
               (lambda (_thing &optional _no-properties) "kxzptvry"))
              ((symbol-function 'majutsu-revision-at-point)
               (lambda () nil))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (majutsu-simplify-parents-execute nil)
      (should (equal called
                     '("simplify-parents" "--revision=kxzptvry"))))))

(ert-deftest majutsu-simplify-parents-execute-does-not-override-explicit-targets ()
  "Explicit source or revision targets should suppress DWIM args."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'magit-region-values)
               (lambda (&rest _) '("ignored")))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (majutsu-simplify-parents-execute
       '("--ignore-immutable" "--source=main | dev"))
      (should (equal called
                     '("simplify-parents" "--ignore-immutable"
                       "--source=main | dev"))))))

(ert-deftest majutsu-simplify-parents-dwim-args/defaults-to-at ()
  "DWIM args should fall back to @ when no target is available."
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) nil))
            ((symbol-function 'majutsu-thing-at-point)
             (lambda (&rest _) nil))
            ((symbol-function 'majutsu-revision-at-point)
             (lambda () nil)))
    (should (equal (majutsu-simplify-parents--dwim-args)
                   '("--revision=@")))))

(ert-deftest majutsu-simplify-parents-dwim-args/defaults-from-point ()
  "DWIM args should prefer the literal revision at point."
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) nil))
            ((symbol-function 'majutsu-thing-at-point)
             (lambda (_thing &optional _no-properties) "main@origin"))
            ((symbol-function 'majutsu-revision-at-point)
             (lambda () "context")))
    (should (equal (majutsu-simplify-parents--dwim-args)
                   '("--revision=main@origin")))))

(ert-deftest majutsu-simplify-parents-dwim-args/defaults-from-region ()
  "DWIM args should use selected region commits."
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) '("a" "b")))
            ((symbol-function 'majutsu-thing-at-point)
             (lambda (&rest _) "ignored"))
            ((symbol-function 'majutsu-revision-at-point)
             (lambda () "ignored")))
    (should (equal (majutsu-simplify-parents--dwim-args)
                   '("--revision=a" "--revision=b")))))

(ert-deftest majutsu-simplify-parents-transient-uses-canonical-revision-option ()
  "The simplify-parents transient should expose jj's canonical --revision option."
  (let ((obj (get 'majutsu-simplify-parents:--revision 'transient--suffix)))
    (should obj)
    (should (equal (oref obj argument) "--revision="))))

(provide 'majutsu-simplify-parents-test)
;;; majutsu-simplify-parents-test.el ends here
