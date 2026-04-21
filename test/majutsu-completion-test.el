;;; majutsu-completion-test.el --- Tests for completion helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for shared completion table helpers.

;;; Code:

(require 'ert)
(require 'majutsu-completion)

(ert-deftest majutsu-completion-parse-annotated-line/basic ()
  "Parse candidate/help lines emitted by jj's fish completion."
  (should (equal (majutsu-completion-parse-annotated-line "log\tShow revision history")
                 '("log" . "Show revision history")))
  (should (equal (majutsu-completion-parse-annotated-line "log")
                 '("log" . nil)))
  (should-not (majutsu-completion-parse-annotated-line "")))

(ert-deftest majutsu-completion-table/exposes-category-annotations-and-default ()
  "Completion table should expose shared metadata consistently."
  (let* ((table (majutsu-completion-table
                 '(("main" . "Main bookmark") "dev")
                 'majutsu-revision
                 "@"))
         (metadata (funcall table "" nil 'metadata))
         (annotation (cdr (assq 'annotation-function (cdr metadata)))))
    (should (equal (all-completions "" table) '("@" "main" "dev")))
    (should (eq (cdr (assq 'category (cdr metadata))) 'majutsu-revision))
    (should (equal (funcall annotation "main") " Main bookmark"))
    (should-not (funcall annotation "dev"))
    (should-not (funcall annotation "@"))))

(ert-deftest majutsu-completion-payload-items/uses-annotation-hash ()
  "Structured payloads should become annotated completion items."
  (let ((annotations (make-hash-table :test #'equal)))
    (puthash "main" "Main bookmark" annotations)
    (should (equal (majutsu-completion-payload-items
                    (list :candidates '("main" "dev")
                          :annotations annotations))
                   '(("main" . "Main bookmark") "dev")))))


(provide 'majutsu-completion-test)
;;; majutsu-completion-test.el ends here
