;;; majutsu-completion.el --- Completion helpers for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shared completion helpers used by Majutsu readers.  This layer only handles
;; generic Emacs completion tables and candidate metadata; providers such as jj
;; native completion or structured bookmark/tag/workspace providers live in the
;; domain modules that know how to fetch those candidates.

;;; Code:

(require 'subr-x)


(defun majutsu-completion-parse-annotated-line (line)
  "Parse LINE as CANDIDATE<TAB>ANNOTATION.
Return (CANDIDATE . ANNOTATION).  Return nil for empty lines."
  (when (string-match "\\`\\([^\t\n]+\\)\\(?:\t\\(.*\\)\\)?\\'" line)
    (cons (match-string 1 line)
          (match-string 2 line))))

(defun majutsu-completion--item-candidate (item)
  "Return completion candidate from ITEM.
ITEM may be a string or a cons cell (CANDIDATE . ANNOTATION)."
  (if (consp item) (car item) item))

(defun majutsu-completion--item-annotation (item)
  "Return completion annotation from ITEM, if any."
  (and (consp item) (cdr item)))

(defun majutsu-completion--add-default (items default)
  "Return ITEMS with DEFAULT prepended when appropriate."
  (if (and default
           (stringp default)
           (not (string-empty-p default))
           (not (member default (mapcar #'majutsu-completion--item-candidate items))))
      (cons default items)
    items))

(defun majutsu-completion-table (items &optional category default)
  "Return a completion table for ITEMS.
ITEMS may contain strings or (CANDIDATE . ANNOTATION) pairs.  CATEGORY,
when non-nil, is exposed in completion metadata.  DEFAULT, when non-empty
and absent from ITEMS, is prepended without annotation."
  (let* ((items (majutsu-completion--add-default items default))
         (candidates (mapcar #'majutsu-completion--item-candidate items))
         (annotations (make-hash-table :test #'equal)))
    (dolist (item items)
      (when-let* ((annotation (majutsu-completion--item-annotation item)))
        (puthash (majutsu-completion--item-candidate item)
                 (if (string-prefix-p " " annotation)
                     annotation
                   (concat " " annotation))
                 annotations)))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          `(metadata
            (display-sort-function . identity)
            ,@(and category `((category . ,category)))
            (annotation-function
             . ,(lambda (candidate)
                  (gethash candidate annotations))))
        (complete-with-action action candidates string predicate)))))

(defun majutsu-completion-payload-category (payload &optional category)
  "Return completion CATEGORY or PAYLOAD's :category."
  (or category (plist-get payload :category)))

(defun majutsu-completion-payload-items (payload)
  "Return completion items from structured PAYLOAD.
PAYLOAD should contain :candidates and may contain :annotations, a hash
mapping candidates to annotation strings."
  (let ((annotations (plist-get payload :annotations)))
    (mapcar (lambda (candidate)
              (if (and (hash-table-p annotations)
                       (gethash candidate annotations))
                  (cons candidate (gethash candidate annotations))
                candidate))
            (plist-get payload :candidates))))

(defun majutsu-completion-payload-table (payload &optional category default)
  "Return a completion table for structured PAYLOAD.
CATEGORY overrides PAYLOAD's :category when non-nil.  DEFAULT is passed to
`majutsu-completion-table'."
  (majutsu-completion-table
   (majutsu-completion-payload-items payload)
   (majutsu-completion-payload-category payload category)
   default))


(provide 'majutsu-completion)
;;; majutsu-completion.el ends here
