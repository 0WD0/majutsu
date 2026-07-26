;;; majutsu-duplicate-test.el --- Tests for duplicate transient -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for duplicate argument assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-duplicate)

(ert-deftest majutsu-duplicate-arguments/region-values-restricted-to-commits ()
  "Region defaults must only pick up commit sections, never other values."
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (condition &optional multiple)
               (should (eq condition 'jj-commit))
               (should multiple)
               nil))
            ((symbol-function 'magit-section-value-if)
             (lambda (_) nil)))
    (let ((transient-current-command nil))
      (should (equal (majutsu-duplicate-arguments) '("-r=@"))))))

(ert-deftest majutsu-duplicate-arguments/uses-region-commits ()
  "Commit sections in the region become -r arguments."
  (cl-letf (((symbol-function 'magit-region-values)
             (lambda (&rest _) '("abc" "def"))))
    (let ((transient-current-command nil))
      (should (equal (sort (majutsu-duplicate-arguments) #'string<)
                     '("-r=abc" "-r=def"))))))

(ert-deftest majutsu-duplicate-execute/passes-arguments-through ()
  "Execute passes the assembled arguments to jj duplicate."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args) (setq called args) 0)))
      (majutsu-duplicate-execute '("-r=abc"))
      (should (equal called '("duplicate" "-r=abc"))))))

(provide 'majutsu-duplicate-test)
;;; majutsu-duplicate-test.el ends here
