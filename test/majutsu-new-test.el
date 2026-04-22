;;; majutsu-new-test.el --- Tests for majutsu-new  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for new-command DWIM target selection.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-new)

(ert-deftest majutsu-new-test-dwim-uses-target-revision-at-point ()
  "DWIM new should prefer the literal revision/ref under point."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu-target-revision-at-point)
               (lambda () "main@origin"))
              ((symbol-function 'majutsu-new--run-command)
               (lambda (args)
                 (setq captured args))))
      (majutsu-new-dwim nil)
      (should (equal captured '("new" "main@origin"))))))

(ert-deftest majutsu-new-test-dwim-without-target-falls-back-to-plain-new ()
  "DWIM new should keep the old plain `jj new' fallback."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu-target-revision-at-point)
               (lambda () nil))
              ((symbol-function 'majutsu-new--run-command)
               (lambda (args)
                 (setq captured args))))
      (majutsu-new-dwim nil)
      (should (equal captured '("new"))))))

(ert-deftest majutsu-new-test-with-after-uses-target-revision-at-point ()
  "`majutsu-new-with-after' should prefer the literal revision/ref under point."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu-target-revision-at-point)
               (lambda () "main@origin"))
              ((symbol-function 'majutsu-new--run-command)
               (lambda (args)
                 (setq captured args))))
      (majutsu-new-with-after)
      (should (equal captured '("new" "--insert-after" "main@origin"))))))

(ert-deftest majutsu-new-test-with-before-uses-target-revision-at-point ()
  "`majutsu-new-with-before' should prefer the literal revision/ref under point."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu-target-revision-at-point)
               (lambda () "main@origin"))
              ((symbol-function 'majutsu-new--run-command)
               (lambda (args)
                 (setq captured args))))
      (majutsu-new-with-before)
      (should (equal captured '("new" "--insert-before" "main@origin"))))))

(provide 'majutsu-new-test)
;;; majutsu-new-test.el ends here
