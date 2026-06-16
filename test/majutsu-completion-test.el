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

(ert-deftest majutsu-completion-payload-table/exposes-affixation-function ()
  "Structured payload tables should expose explicit suffix functions."
  (let ((majutsu-completion-separator "  "))
    (let* ((table (majutsu-completion-payload-table
                   (list :category 'majutsu-bookmark
                         :candidates '("main")
                         :annotation-suffix-function
                         (lambda (_candidate)
                           (majutsu-completion-annotation
                            (majutsu-completion-field "bookmark" 'majutsu-completion-key)
                            (majutsu-completion-field "tracked@origin" 'success))))))
           (metadata (funcall table "" nil 'metadata))
           (affixation (cdr (assq 'affixation-function (cdr metadata))))
           (suffix (nth 2 (car (funcall affixation '("main"))))))
      (should (functionp affixation))
      (should (string-match-p "bookmark" suffix))
      (should (string-match-p "tracked@origin" suffix))
      (should (text-property-any 0 (length suffix)
                                 'face 'majutsu-completion-key
                                 suffix))
      (should (text-property-any 0 (length suffix)
                                 'face 'success
                                 suffix)))))

(ert-deftest majutsu-completion-entry-suffix-function/returns-nil-without-entry ()
  "Entry-backed suffix helpers should stay silent for missing entries."
  (let ((entries (make-hash-table :test #'equal)))
    (puthash "main" '(:note "tracked") entries)
    (let ((suffix-function
           (majutsu-completion-entry-suffix-function
            entries
            (lambda (entry)
              (majutsu-completion-string-suffix (plist-get entry :note))))))
      (should (string-match-p "tracked" (funcall suffix-function "main")))
      (should-not (funcall suffix-function "dev")))))

(ert-deftest majutsu-completion-payload-table/uses-annotation-hash-for-affixation ()
  "Structured payload annotations should also produce aligned affixation."
  (let ((annotations (make-hash-table :test #'equal)))
    (puthash "alpha" "topic (2 open)" annotations)
    (let* ((table (majutsu-completion-payload-table
                   (list :category 'majutsu-gerrit-topic
                         :candidates '("alpha")
                         :annotations annotations)))
           (metadata (funcall table "" nil 'metadata))
           (affixation (cdr (assq 'affixation-function (cdr metadata))))
           (suffix (nth 2 (car (funcall affixation '("alpha"))))))
      (should (functionp affixation))
      (should (string-match-p "topic (2 open)" suffix)))))

(provide 'majutsu-completion-test)
;;; majutsu-completion-test.el ends here
