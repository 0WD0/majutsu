;;; majutsu-edit-test.el --- Tests for majutsu-edit  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for revision edit helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-edit)

(ert-deftest majutsu-edit-revision/runs-explicit-target ()
  "Explicit revision editing should be reusable by completion actions."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq captured args)
                 0))
              ((symbol-function 'message) #'ignore))
      (should (majutsu-edit-revision "main"))
      (should (equal captured '("edit" "main")))
      (should (majutsu-edit-revision "main" '(4)))
      (should (equal captured '("edit" "main" "--ignore-immutable"))))))

(provide 'majutsu-edit-test)
;;; majutsu-edit-test.el ends here
