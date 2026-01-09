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

(ert-deftest majutsu-interactive-reverse-patch-handles-new-file ()
  "Reverse patch should handle new file with --- /dev/null header."
  (let* ((patch (concat "diff --git a/new.txt b/new.txt\n"
                        "new file mode 100644\n"
                        "index 0000000..abc1234\n"
                        "--- /dev/null\n"
                        "+++ b/new.txt\n"
                        "@@ -0,0 +1,2 @@\n"
                        "+line1\n"
                        "+line2\n"))
         (reversed (majutsu-interactive--reverse-patch patch)))
    ;; /dev/null headers should be swapped
    (should (string-match-p "^\\+\\+\\+ /dev/null$" reversed))
    (should (string-match-p "^--- a/new.txt$" reversed))
    ;; Mode/index should reflect deletion
    (should (string-match-p "^deleted file mode 100644$" reversed))
    (should (string-match-p "^index abc1234\\.\\.0000000$" reversed))
    ;; Added lines should become removed
    (should (string-match-p "^-line1$" reversed))))

(ert-deftest majutsu-interactive-reverse-patch-handles-deleted-file ()
  "Reverse patch should handle deleted file with +++ /dev/null header."
  (let* ((patch (concat "diff --git a/old.txt b/old.txt\n"
                        "deleted file mode 100644\n"
                        "index abc1234..0000000\n"
                        "--- a/old.txt\n"
                        "+++ /dev/null\n"
                        "@@ -1,2 +0,0 @@\n"
                        "-line1\n"
                        "-line2\n"))
         (reversed (majutsu-interactive--reverse-patch patch)))
    ;; /dev/null headers should be swapped
    (should (string-match-p "^--- /dev/null$" reversed))
    (should (string-match-p "^\\+\\+\\+ b/old.txt$" reversed))
    ;; Mode/index should reflect creation
    (should (string-match-p "^new file mode 100644$" reversed))
    (should (string-match-p "^index 0000000\\.\\.abc1234$" reversed))
    ;; Removed lines should become added
    (should (string-match-p "^\\+line1$" reversed))))

(provide 'majutsu-interactive-test)

;;; majutsu-interactive-test.el ends here
