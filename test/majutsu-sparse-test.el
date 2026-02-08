;;; majutsu-sparse-test.el --- Tests for .jjsparse editing  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for .jjsparse comment handling and font-lock.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-sparse)

(defun majutsu-test--faces-at (pos)
  "Return a list of font-lock faces at POS."
  (let ((font-lock-face (get-text-property pos 'font-lock-face))
        (face (get-text-property pos 'face)))
    (cl-remove-duplicates
     (append
      (cond
       ((listp font-lock-face) font-lock-face)
       ((symbolp font-lock-face) (list font-lock-face))
       (t nil))
      (cond
       ((listp face) face)
       ((symbolp face) (list face))
       (t nil)))
     :test #'eq)))

(ert-deftest majutsu-sparse-jjsparse-comment-vars ()
  "jjsparse mode configures JJ comment variables."
  (with-temp-buffer
    (majutsu-jjsparse-mode)
    (should (equal comment-start "JJ:"))
    (should (equal comment-start-skip "^JJ:[ \t]*"))
    (should (equal comment-end ""))
    (should (eq comment-use-syntax nil))))

(ert-deftest majutsu-sparse-jjsparse-font-lock-comments ()
  "Only JJ: lines are highlighted as comments."
  (with-temp-buffer
    (majutsu-jjsparse-mode)
    (insert "JJ: keep this as a comment\n")
    (insert "# not a comment\n")
    (insert "src/\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (memq 'font-lock-comment-face
                  (majutsu-test--faces-at (point))))
    (search-forward "# not a comment")
    (beginning-of-line)
    (should-not (memq 'font-lock-comment-face
                      (majutsu-test--faces-at (point))))))

(ert-deftest majutsu-sparse-directory-candidates ()
  "Directory candidates include top-level and nested paths."
  (cl-letf (((symbol-function 'majutsu-sparse--available-paths)
             (lambda () '("src/a.el" "lib/b.el" "dir/sub/c.el" "README.md"))))
    (let* ((candidates (majutsu-sparse--directory-candidates))
           (sorted (sort (copy-sequence candidates) #'string<)))
      (should (equal sorted
                     (sort (copy-sequence '("src" "lib" "dir/sub" "."))
                           #'string<))))))

(provide 'majutsu-sparse-test)

;;; majutsu-sparse-test.el ends here
