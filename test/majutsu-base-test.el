;;; majutsu-base-test.el --- Tests for Majutsu base readers  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for core completion reader semantics.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-base)

(ert-deftest majutsu-base-registers-bookmark-and-tag-section-types ()
  (should (eq (alist-get 'jj-bookmark magit--section-type-alist)
              'majutsu-bookmark-section))
  (should (eq (alist-get 'jj-tag magit--section-type-alist)
              'majutsu-tag-section)))

(ert-deftest majutsu-completing-read/empty-optional-returns-nil ()
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args)
               "")))
    (should-not (majutsu-completing-read "Value" '("a" "b")))))

(ert-deftest majutsu-completing-read/any-requires-nonempty ()
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest args)
               (should-not (nth 3 args))
               "")))
    (should-error (majutsu-completing-read "Value" '("a" "b") nil 'any)
                  :type 'user-error)))

(ert-deftest majutsu-completing-read-multiple/any-requires-nonempty ()
  (cl-letf (((symbol-function 'completing-read-multiple)
             (lambda (&rest args)
               (should-not (nth 3 args))
               nil)))
    (should-error (majutsu-completing-read-multiple "Values" '("a" "b") nil 'any)
                  :type 'user-error)))

(ert-deftest majutsu-completing-read-payload/uses-payload-category-and-prewarm ()
  (let (seen-category seen-history seen-prewarm)
    (cl-letf (((symbol-function 'majutsu-completion-prewarm-payload)
               (lambda (&rest args)
                 (setq seen-prewarm args)))
              ((symbol-function 'completing-read)
               (lambda (_prompt table _predicate require-match _initial hist _default)
                 (should-not require-match)
                 (setq seen-history hist)
                 (let ((metadata (funcall table "" nil 'metadata)))
                   (setq seen-category (cdr (assq 'category (cdr metadata)))))
                 "origin")))
      (should (equal (majutsu-completing-read-payload
                      "Remote"
                      '(:category majutsu-remote :candidates ("origin"))
                      nil 'any nil 'majutsu-remote-name-history nil nil 'ctx "/tmp/repo/")
                     "origin"))
      (should (eq seen-category 'majutsu-remote))
      (should (eq seen-history 'majutsu-remote-name-history))
      (should (equal seen-prewarm
                     '((:category majutsu-remote :candidates ("origin"))
                       nil ctx "/tmp/repo/"))))))

(ert-deftest majutsu-completing-read-multiple-payload/uses-payload-category ()
  (let (seen-category seen-history)
    (cl-letf (((symbol-function 'majutsu-completion-prewarm-payload)
               (lambda (&rest _args) nil))
              ((symbol-function 'completing-read-multiple)
               (lambda (_prompt table _predicate require-match _initial hist _default)
                 (should-not require-match)
                 (setq seen-history hist)
                 (let ((metadata (funcall table "" nil 'metadata)))
                   (setq seen-category (cdr (assq 'category (cdr metadata)))))
                 '("ws-a"))))
      (should (equal (majutsu-completing-read-multiple-payload
                      "Workspace"
                      '(:category majutsu-workspace :candidates ("ws-a"))
                      nil 'any nil 'majutsu-workspace-name-history)
                     '("ws-a")))
      (should (eq seen-category 'majutsu-workspace))
      (should (eq seen-history 'majutsu-workspace-name-history)))))

(provide 'majutsu-base-test)
;;; majutsu-base-test.el ends here
