;;; majutsu-edit-test.el --- Tests for majutsu-edit  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for changeset edit helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-edit)

(ert-deftest majutsu-edit-test-edit-changeset-prefers-literal-revision-at-point ()
  "Edit changeset should prefer the literal revision/ref under point."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu-thing-at-point)
               (lambda (_thing &optional _no-properties)
                 "main@origin"))
              ((symbol-function 'majutsu-revision-at-point)
               (lambda () "context"))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq captured args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-edit-changeset)
      (should (equal captured '("edit" "main@origin"))))))

(provide 'majutsu-edit-test)
;;; majutsu-edit-test.el ends here
