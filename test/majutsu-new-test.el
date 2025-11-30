;;; majutsu-new-test.el --- Tests for jj new transient helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Version: 1.0.0
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Tests for jj new transient helpers.

;;; Code:

(require 'ert)
(require 'majutsu)

(ert-deftest majutsu-new-build-args-defaults ()
  "Defaults to using @ when no parents are provided."
  (let ((result (majutsu-new--build-args
                 :parents nil
                 :after '()
                 :before '()
                 :message nil
                 :no-edit nil)))
    (should (equal result '("new")))))

(ert-deftest majutsu-new-build-args-with-options ()
  "Includes all configured options in the expected order."
  (let ((result (majutsu-new--build-args
                 :parents (list (majutsu-section-create-revision :change-id "P1")
                                (majutsu-section-create-revision :change-id "P2"))
                 :after (list (majutsu-section-create-revision :change-id "A1"))
                 :before (list (majutsu-section-create-revision :change-id "B1")
                               (majutsu-section-create-revision :change-id "B2"))
                 :message "msg"
                 :no-edit t)))
    (should (equal result '("new"
                           "--after" "A1"
                           "--before" "B1" "--before" "B2"
                           "--message" "msg"
                           "--no-edit"
                           "P1" "P2")))))

(ert-deftest majutsu-new-build-args-filters-empty-values ()
  "Filters out empty revsets and messages before building the command."
  (let ((result (majutsu-new--build-args
                 :parents (list (majutsu-section-create-revision :change-id "")
                                (majutsu-section-create-revision :change-id "P3"))
                 :after (list nil (majutsu-section-create-revision :change-id "A2"))
                 :before (list (majutsu-section-create-revision :change-id "B3") "")
                 :message ""
                 :no-edit nil)))
    (should (equal result '("new"
                           "--after" "A2"
                           "--before" "B3"
                           "P3")))))

(ert-deftest majutsu-new-action-summary-reflects-state ()
  "Summaries list active selections in order."
  (let ((majutsu-new-parents (list (majutsu-section-create-revision :change-id "P1")))
        (majutsu-new-after (list (majutsu-section-create-revision :change-id "A1")))
        (majutsu-new-before nil)
        (majutsu-new-message "hello")
        (majutsu-new-no-edit t))
    (should (equal (majutsu-new--action-summary)
                   "Parents: P1 | After: A1 | Message: hello | --no-edit"))))

(provide 'majutsu-new-test)

;;; majutsu-new-test.el ends here
