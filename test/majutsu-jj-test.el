;; -*- lexical-binding: t; -*-
;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Version: 1.0.0
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for majutsu-jj helpers.

;;; Code:

(require 'ert)
(require 'majutsu-jj)

(ert-deftest majutsu-jj-fileset-quote-single-quote ()
  "Single quotes should be preserved inside fileset strings."
  (should (equal (majutsu-jj-fileset-quote "test'file")
                 "file:\"test'file\"")))

(ert-deftest majutsu-jj-fileset-quote-escapes-specials ()
  "Double quotes, backslashes, and newlines should be escaped."
  (let* ((input "a\"b\\c\n")
         (expected "file:\"a\\\"b\\\\c\\n\""))
    (should (equal (majutsu-jj-fileset-quote input) expected))))

(provide 'majutsu-jj-test)
