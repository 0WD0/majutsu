;;; majutsu-diff-test.el --- Tests for diff section rendering  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Version: 1.0.0
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for diff section rendering.

;;; Code:

(require 'ert)
(require 'majutsu)

(ert-deftest majutsu-diff-inserts-toggleable-sections ()
  "Diff sections should create headings so users can toggle them."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (diff (string-join
                  '("diff --git a/foo b/foo"
                    "index 1234567..89abcde 100644"
                    "--- a/foo"
                    "+++ b/foo"
                    "@@ -1 +1 @@"
                    "-foo"
                    "+bar")
                  "\n")))
      (magit-insert-section (root-section (majutsu-diff-section))
        (magit-insert-heading "Diff")
        (majutsu--insert-diff-hunks diff))
      (let* ((root magit-root-section)
             (file-section (car (oref root children)))
             (hunk-section (car (oref file-section children))))
        (should (eieio-object-p file-section))
        (should (oref file-section content))
        (should (eieio-object-p hunk-section))
        (should (oref hunk-section content))))))

(provide 'majutsu-diff-test)

;;; majutsu-diff-test.el ends here
