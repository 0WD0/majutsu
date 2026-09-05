;;; majutsu-bookmark-test-utils.el --- Bookmark row fixtures -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shared row fixtures for bookmark and Embark tests.

;;; Code:

(require 'cl-lib)
(require 'majutsu-row)

(cl-defun majutsu-bookmark-test--row
    (heading name remote tracked conflict-details removed-ids added-ids &optional
             (newline t newline-supplied-p))
  "Return one raw bookmark-list row record.
When NEWLINE is non-nil or omitted, append a trailing newline."
  (concat majutsu-row-start-token
          heading
          majutsu-row-tail-token
          majutsu-row-body-token
          (replace-regexp-in-string
           "\n" majutsu-row-field-line-separator (or conflict-details "") t t)
          majutsu-row-meta-token
          (string-join (list name (or remote "")
                             (if tracked "t" "")
                             (string-join (or removed-ids nil)
                                          majutsu-row-field-line-separator)
                             (string-join (or added-ids nil)
                                          majutsu-row-field-line-separator))
                       majutsu-row-field-separator)
          majutsu-row-end-token
          (if (or (not newline-supplied-p) newline) "\n" "")))

(cl-defun majutsu-bookmark-test--ref
    (name remote tracked heading &optional conflict-details removed-ids added-ids
          (newline t newline-supplied-p))
  "Return one bookmark-list ref row record."
  (majutsu-bookmark-test--row
   heading name remote tracked conflict-details removed-ids added-ids
   (if newline-supplied-p newline t)))

(provide 'majutsu-bookmark-test-utils)
;;; majutsu-bookmark-test-utils.el ends here
