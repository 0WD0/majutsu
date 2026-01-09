;;; majutsu-interactive-test.el --- Tests for interactive patch operations  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Version: 1.0.0
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for interactive patch operations.

;;; Code:

(require 'ert)
(require 'majutsu-interactive)

(ert-deftest majutsu-interactive-reverse-patch-handles-double-minus ()
  "Reverse patch should handle lines starting with -- (removed line with - content)."
  (let* ((patch (concat "--- a/test.txt\n"
                        "+++ b/test.txt\n"
                        "@@ -1,2 +1,2 @@\n"
                        " context\n"
                        "--removed line starting with minus\n"
                        "+added line\n"))
         (reversed (majutsu-interactive--reverse-patch patch)))
    ;; The -- line should become +-removed...
    (should (string-match-p "^\\+-removed line starting with minus$"
                            reversed))
    ;; The + line should become -added...
    (should (string-match-p "^-added line$" reversed))))

(ert-deftest majutsu-interactive-reverse-patch-handles-double-plus ()
  "Reverse patch should handle lines starting with ++ (added line with + content)."
  (let* ((patch (concat "--- a/test.txt\n"
                        "+++ b/test.txt\n"
                        "@@ -1,2 +1,2 @@\n"
                        " context\n"
                        "-removed line\n"
                        "++added line starting with plus\n"))
         (reversed (majutsu-interactive--reverse-patch patch)))
    ;; The ++ line should become -+added...
    (should (string-match-p "^-\\+added line starting with plus$"
                            reversed))
    ;; The - line should become +removed...
    (should (string-match-p "^\\+removed line$" reversed))))

(ert-deftest majutsu-interactive-reverse-patch-preserves-file-headers ()
  "Reverse patch should swap --- a/ and +++ b/ headers correctly."
  (let* ((patch (concat "--- a/foo.txt\n"
                        "+++ b/foo.txt\n"
                        "@@ -1 +1 @@\n"
                        "-old\n"
                        "+new\n"))
         (reversed (majutsu-interactive--reverse-patch patch)))
    ;; Headers should be swapped
    (should (string-match-p "^--- a/foo.txt$" reversed))
    (should (string-match-p "^\\+\\+\\+ b/foo.txt$" reversed))))

(ert-deftest majutsu-interactive-reverse-patch-handles-empty-removed-line ()
  "Reverse patch should handle empty removed lines (just -)."
  (let* ((patch (concat "--- a/test.txt\n"
                        "+++ b/test.txt\n"
                        "@@ -1,2 +1 @@\n"
                        " context\n"
                        "-\n"))
         (reversed (majutsu-interactive--reverse-patch patch)))
    ;; Empty - line should become empty + line
    (should (string-match-p "^\\+$" reversed))))

(ert-deftest majutsu-interactive-reverse-patch-handles-empty-added-line ()
  "Reverse patch should handle empty added lines (just +)."
  (let* ((patch (concat "--- a/test.txt\n"
                        "+++ b/test.txt\n"
                        "@@ -1 +1,2 @@\n"
                        " context\n"
                        "+\n"))
         (reversed (majutsu-interactive--reverse-patch patch)))
    ;; Empty + line should become empty - line
    (should (string-match-p "^-$" reversed))))

(provide 'majutsu-interactive-test)

;;; majutsu-interactive-test.el ends here
