;;; majutsu-completion.el --- Completion helpers for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shared completion helpers used by Majutsu readers.
;;
;; Majutsu exposes normal Emacs completion metadata directly:
;; - `annotation-function' for plain suffix text
;; - `affixation-function' for richer structured suffixes
;;
;; Completion front ends can consume this metadata directly.

;;; Code:

(require 'compat)
(require 'subr-x)

(defgroup majutsu-completion nil
  "Completion helpers and styles used by Majutsu."
  :group 'tools)

(defcustom majutsu-completion-separator "  "
  "Separator inserted between Majutsu completion annotation fields."
  :type 'string
  :group 'majutsu-completion)

(defface majutsu-completion-key
  '((t :inherit font-lock-keyword-face))
  "Face used for primary completion labels such as kinds or categories."
  :group 'majutsu-completion)

(defface majutsu-completion-type
  '((t :inherit shadow))
  "Face used for secondary completion type metadata."
  :group 'majutsu-completion)

(defface majutsu-completion-documentation
  '((t :inherit completions-annotations))
  "Face used for completion documentation-style suffixes."
  :group 'majutsu-completion)

(defface majutsu-completion-number
  '((t :inherit font-lock-constant-face))
  "Face used for numeric completion metadata."
  :group 'majutsu-completion)

(defface majutsu-completion-file-name
  '((t :inherit majutsu-completion-documentation))
  "Face used for file-name completion metadata."
  :group 'majutsu-completion)

(defun majutsu-completion-properties (&optional category annotation-function affixation-function)
  "Return `completion-extra-properties' plist for CATEGORY."
  `(,@(and category `(:category ,category))
    :display-sort-function identity
    :cycle-sort-function identity
    ,@(and annotation-function `(:annotation-function ,annotation-function))
    ,@(and affixation-function `(:affixation-function ,affixation-function))))

(defun majutsu-completion--annotation-table (items)
  "Return candidate annotation table for ITEMS, or nil."
  (let (annotations)
    (dolist (item items)
      (when (consp item)
        (unless annotations
          (setq annotations (make-hash-table :test #'equal)))
        (puthash (car item) (cdr item) annotations)))
    annotations))

(defun majutsu-completion--annotation-function (annotations)
  "Return annotation function backed by ANNOTATIONS hash table."
  (when (and annotations (> (hash-table-count annotations) 0))
    (lambda (candidate)
      (when-let* ((annotation (gethash candidate annotations)))
        (if (string-prefix-p " " annotation)
            annotation
          (concat " " annotation))))))

(defun majutsu-completion-field (text &optional face)
  "Return TEXT formatted as one annotation field."
  (when (and text (not (string-empty-p text)))
    (if face
        (propertize (format "%s" text) 'face face)
      (format "%s" text))))

(defun majutsu-completion-column (text width &optional face)
  "Return TEXT as a fixed-width annotation column.
WIDTH is the target display width and FACE is applied to the whole column."
  (let* ((raw (format "%s" (or text "")))
         (truncated (truncate-string-to-width raw width 0 ?\s "…"))
         (padding (max 0 (- width (string-width truncated))))
         (value (concat truncated (make-string padding ?\s))))
    (if face
        (propertize value 'face face)
      value)))

(defun majutsu-completion-annotation (&rest fields)
  "Join FIELDS into one annotation string."
  (setq fields (delq nil fields))
  (when fields
    (concat majutsu-completion-separator
            (mapconcat #'identity fields majutsu-completion-separator))))

(defun majutsu-completion-string-suffix (annotation &optional face)
  "Return aligned suffix for ANNOTATION.
FACE defaults to `majutsu-completion-documentation'."
  (when-let* ((annotation (and (stringp annotation)
                               (string-trim-left annotation)))
              ((not (string-empty-p annotation))))
    (majutsu-completion-annotation
     (majutsu-completion-field annotation
                               (or face 'majutsu-completion-documentation)))))

(defun majutsu-completion-annotation-suffix-function (annotations &optional face)
  "Return suffix function backed by ANNOTATIONS hash table.
FACE is forwarded to `majutsu-completion-string-suffix'."
  (when (and annotations (> (hash-table-count annotations) 0))
    (lambda (candidate)
      (majutsu-completion-string-suffix (gethash candidate annotations) face))))

(defun majutsu-completion-entry-suffix-function (entries entry-suffix-function)
  "Return candidate suffix function backed by ENTRIES.
ENTRY-SUFFIX-FUNCTION is called with one entry from ENTRIES.  Candidates
without entries simply get no suffix."
  (when (and entries (> (hash-table-count entries) 0))
    (lambda (candidate)
      (when-let* ((entry (gethash candidate entries)))
        (funcall entry-suffix-function entry)))))

(defun majutsu-completion-affixation-function (suffix-function)
  "Return aligned affixation function backed by SUFFIX-FUNCTION.
SUFFIX-FUNCTION is called with a completion candidate and should return the
suffix string to display, or nil for no suffix."
  (when suffix-function
    (lambda (candidates)
      (let ((width (if candidates
                       (apply #'max 0 (mapcar #'string-width candidates))
                     0)))
        (mapcar (lambda (candidate)
                  (let ((suffix (funcall suffix-function candidate)))
                    (list candidate ""
                          (if (and suffix (> (length suffix) 0))
                              (concat (make-string (max 0 (- width (string-width candidate))) ?\s)
                                      suffix)
                            ""))))
                candidates)))))

(defun majutsu-completion-items-properties (items &optional category)
  "Return completion properties for static ITEMS."
  (let* ((annotations (majutsu-completion--annotation-table items))
         (annotation-function (majutsu-completion--annotation-function annotations))
         (suffix-function (majutsu-completion-annotation-suffix-function annotations))
         (affixation-function (majutsu-completion-affixation-function suffix-function)))
    (majutsu-completion-properties category annotation-function affixation-function)))

(defun majutsu-completion-table (items &optional category)
  "Return a completion table for ITEMS.
ITEMS may contain strings or (CANDIDATE . ANNOTATION) pairs.  CATEGORY,
when non-nil, is exposed in completion metadata."
  (completion-table-with-metadata
   items
   (majutsu-completion-payload-metadata
    (list :category category
          :annotations (majutsu-completion--annotation-table items)))))

(defun majutsu-completion--payload-functions (payload &optional category)
  "Return (CATEGORY ANNOTATION-FUNCTION AFFIXATION-FUNCTION) for PAYLOAD."
  (let* ((category (or category (plist-get payload :category)))
         (annotation-function (or (plist-get payload :annotation-function)
                                  (majutsu-completion--annotation-function
                                   (plist-get payload :annotations))))
         (suffix-function (or (plist-get payload :annotation-suffix-function)
                              (majutsu-completion-annotation-suffix-function
                               (plist-get payload :annotations))))
         (affixation-function (or (plist-get payload :affixation-function)
                                  (majutsu-completion-affixation-function
                                   suffix-function))))
    (list category annotation-function affixation-function)))

(defun majutsu-completion-payload-properties (payload &optional category)
  "Return `completion-extra-properties' plist for structured PAYLOAD."
  (apply #'majutsu-completion-properties
         (majutsu-completion--payload-functions payload category)))

(defun majutsu-completion-payload-metadata (payload &optional category)
  "Return completion metadata alist for structured PAYLOAD.
CATEGORY overrides PAYLOAD's :category when non-nil.

Besides standard completion metadata keys, PAYLOAD may provide the internal
key =:annotation-suffix-function=, a function from candidate string to suffix
string used to build an aligned `affixation-function'."
  (pcase-let ((`(,category ,annotation-function ,affixation-function)
               (majutsu-completion--payload-functions payload category)))
    `((display-sort-function . identity)
      (cycle-sort-function . identity)
      ,@(and category `((category . ,category)))
      ,@(and annotation-function `((annotation-function . ,annotation-function)))
      ,@(and affixation-function `((affixation-function . ,affixation-function))))))

(defun majutsu-completion-payload-table (payload &optional category)
  "Return a completion table for structured PAYLOAD.
CATEGORY overrides PAYLOAD's :category when non-nil.  PAYLOAD may carry
standard completion metadata plus Majutsu's internal
=:annotation-suffix-function= helper key."
  (completion-table-with-metadata
   (plist-get payload :candidates)
   (majutsu-completion-payload-metadata payload category)))

(provide 'majutsu-completion)
;;; majutsu-completion.el ends here
