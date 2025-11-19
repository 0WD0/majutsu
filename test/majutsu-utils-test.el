;;; majutsu-utils-test.el --- Tests for majutsu utility functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Version: 1.0.0
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for majutsu misc utility functions.

;;; Code:

(require 'ert)
(require 'majutsu)

(ert-deftest majutsu-utils-normalize-id-value ()
  "Test ID normalization for various input types."
  (should (equal (majutsu--normalize-id-value "abc") "abc"))
  (should (equal (majutsu--normalize-id-value 'abc) "abc"))
  (should (equal (majutsu--normalize-id-value 123) "123"))
  (should (equal (majutsu--normalize-id-value nil) nil))
  (let ((s "prop"))
    (put-text-property 0 1 'face 'bold s)
    (should (equal (majutsu--normalize-id-value s) "prop"))
    (should-not (text-properties-at 0 (majutsu--normalize-id-value s)))))

(ert-deftest majutsu-utils-diff-file-status ()
  "Test file status inference from diff lines."
  (should (equal (majutsu--diff-file-status '("new file mode 100644")) "new file"))
  (should (equal (majutsu--diff-file-status '("deleted file mode 100644")) "deleted"))
  (should (equal (majutsu--diff-file-status '("rename from old.txt" "rename to new.txt")) "renamed"))
  (should (equal (majutsu--diff-file-status '("copy from source.txt" "copy to dest.txt")) "copied"))
  (should (equal (majutsu--diff-file-status '("index 832...912 100644")) "modified")))

(ert-deftest majutsu-utils-extract-bookmark-names ()
  "Test bookmark name extraction from command output."
  (should (equal (majutsu--extract-bookmark-names "bookmark: main") '("main")))
  (should (equal (majutsu--extract-bookmark-names "bookmark: feature-1\nbookmark: feature-2") '("feature-1" "feature-2")))
  (should (equal (majutsu--extract-bookmark-names "no bookmarks here") nil))
  (should (equal (majutsu--extract-bookmark-names "bookmark: dev, bookmark: test") '("dev" "test"))))

(provide 'majutsu-utils-test)
