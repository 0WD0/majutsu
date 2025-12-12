;;; majutsu-workspace-test.el --- Tests for majutsu workspace helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for parsing and small helpers in majutsu-workspace.el.

;;; Code:

(require 'ert)
(require 'majutsu-workspace)

(ert-deftest majutsu-workspace-parse-list-output/basic ()
  "Parse structured `jj workspace list -T ...` output."
  (let* ((sep "\x1e")
         (output (concat
                  "@" sep "default" sep "wnurqwps" sep "6acd46b7" sep "Main wc" "\n"
                  ""  sep "w2"      sep "lvolzxkz" sep "32e07e11" sep "" "\n"))
         (entries (majutsu-workspace-parse-list-output output)))
    (should (equal (length entries) 2))
    (should (equal (plist-get (nth 0 entries) :name) "default"))
    (should (equal (plist-get (nth 0 entries) :current) t))
    (should (equal (plist-get (nth 0 entries) :change-id) "wnurqwps"))
    (should (equal (plist-get (nth 0 entries) :commit-id) "6acd46b7"))
    (should (equal (plist-get (nth 0 entries) :desc) "Main wc"))
    (should (equal (plist-get (nth 1 entries) :name) "w2"))
    (should-not (plist-get (nth 1 entries) :current))
    (should (equal (plist-get (nth 1 entries) :desc) ""))))

(ert-deftest majutsu-workspace-visit/binds-root-cache ()
  "Ensure visiting another workspace clears cached `majutsu--repo-root'."
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-log)
               (lambda ()
                 (setq seen (list default-directory majutsu--repo-root)))))
      (let ((majutsu--repo-root "/tmp/old/")
            (default-directory "/tmp/old/"))
        (majutsu-workspace-visit "/tmp/new/")))
    (should (equal (car seen) (file-name-as-directory (expand-file-name "/tmp/new/"))))
    (should (equal (cadr seen) nil))))

(provide 'majutsu-workspace-test)
;;; majutsu-workspace-test.el ends here
