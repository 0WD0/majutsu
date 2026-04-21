;;; majutsu-base-test.el --- Tests for Majutsu base readers  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for core completion reader semantics.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-base)

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

(provide 'majutsu-base-test)
;;; majutsu-base-test.el ends here
