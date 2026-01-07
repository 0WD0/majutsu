;; -*- lexical-binding: t; -*-
;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Version: 1.0.0
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for majutsu file helpers.

;;; Code:

(require 'ert)
(require 'majutsu-file)

(ert-deftest majutsu-file-revset-for-files-quotes-path ()
  "Paths with single quotes should be fileset-quoted."
  (should (equal (majutsu-file--revset-for-files "rev" "test'file" 'prev)
                 "::rev-&files(file:\"test'file\")")))

(provide 'majutsu-file-test)
