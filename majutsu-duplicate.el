;;; majutsu-duplicate.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library implements jj duplicate commands and transients,
;; managing source and destination selections.

;;; Code:

(require 'majutsu)
(require 'majutsu-selection)

(defclass majutsu-duplicate-option (majutsu-selection-option)
  ((selection-key :initarg :selection-key :initform nil)))

(defclass majutsu-duplicate--toggle-option (majutsu-selection-toggle-option)
  ())

;;; Duplicate

(defun majutsu-duplicate-run-jj (args)
  "Execute jj duplicate with ARGS."
  (apply #'majutsu-run-jj "duplicate" args))

(defun majutsu-duplicate-execute (args)
  "Execute jj duplicate using transient selections."
  (interactive (list (transient-args 'majutsu-duplicate)))
  (majutsu-duplicate-run-jj args))

;;;###autoload
(defun majutsu-duplicate-dwim (arg)
  "Duplicate the changeset at point.
With prefix ARG, open the duplicate transient."
  (interactive "P")
  (if arg
      (call-interactively #'majutsu-duplicate)
    (let* ((revsets (or (magit-region-values nil t)
                        (and (magit-section-value-if 'jj-commit)
                             (list (magit-section-value-if 'jj-commit)))
                        (list "@"))))
      (majutsu-duplicate-run-jj revsets))))

;;; Duplicate Transient
(transient-define-argument majutsu-duplicate:-r ()
  :description "Source"
  :class 'majutsu-duplicate-option
  :selection-key 'source
  :selection-label "[SRC]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :selection-type 'multi
  :key "-r"
  :argument "-r"
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-duplicate:--onto ()
  :description "Onto"
  :class 'majutsu-duplicate-option
  :selection-key 'onto
  :selection-label "[ONTO]"
  :selection-face '(:background "dark green" :foreground "white")
  :selection-type 'multi
  :key "-o"
  :argument "--onto="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-duplicate:--after ()
  :description "After"
  :class 'majutsu-duplicate-option
  :selection-key 'after
  :selection-label "[AFTER]"
  :selection-face '(:background "dark blue" :foreground "white")
  :selection-type 'multi
  :key "-A"
  :argument "--insert-after="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-duplicate:--before ()
  :description "Before"
  :class 'majutsu-duplicate-option
  :selection-key 'before
  :selection-label "[BEFORE]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-type 'multi
  :key "-B"
  :argument "--insert-before="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-duplicate:source ()
  :description "Source (toggle at point)"
  :class 'majutsu-duplicate--toggle-option
  :selection-key 'source
  :selection-type 'multi
  :key "s"
  :argument "-r"
  :multi-value 'repeat)

(transient-define-argument majutsu-duplicate:onto ()
  :description "Onto (toggle at point)"
  :class 'majutsu-duplicate--toggle-option
  :selection-key 'onto
  :selection-type 'multi
  :key "o"
  :argument "--onto="
  :multi-value 'repeat)

(transient-define-argument majutsu-duplicate:after ()
  :description "After (toggle at point)"
  :class 'majutsu-duplicate--toggle-option
  :selection-key 'after
  :selection-type 'multi
  :key "a"
  :argument "--insert-after="
  :multi-value 'repeat)

(transient-define-argument majutsu-duplicate:before ()
  :description "Before (toggle at point)"
  :class 'majutsu-duplicate--toggle-option
  :selection-key 'before
  :selection-type 'multi
  :key "b"
  :argument "--insert-before="
  :multi-value 'repeat)

(defun majutsu-duplicate-clear-selections ()
  "Clear duplicate selections."
  (interactive)
  (when (consp transient--suffixes)
    (dolist (obj transient--suffixes)
      (when (and (cl-typep obj 'majutsu-duplicate-option)
                 (memq (oref obj selection-key) '(source onto after before)))
        (transient-infix-set obj nil))))
  (when transient--prefix
    (transient--redisplay))
  (majutsu-selection-render)
  (message "Cleared duplicate selections"))

(transient-define-prefix majutsu-duplicate ()
  "Internal transient for jj duplicate."
  :man-page "jj-duplicate"
  :transient-non-suffix t
  [:description "JJ Duplicate"
   :class transient-columns
   ["Sources"
    (majutsu-duplicate:-r)
    (majutsu-duplicate:source)
    ("c" "Clear selections" majutsu-duplicate-clear-selections
     :transient t)]
   ["Placement"
    (majutsu-duplicate:--onto)
    (majutsu-duplicate:--after)
    (majutsu-duplicate:--before)
    (majutsu-duplicate:onto)
    (majutsu-duplicate:after)
    (majutsu-duplicate:before)]
   ["Actions"
    ("RET" "Duplicate changes" majutsu-duplicate-execute)
    ("p" "Duplicate changes" majutsu-duplicate-execute)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (transient-setup
   'majutsu-duplicate nil nil
   :scope
   (majutsu-selection-session-begin)))

;;; _
(provide 'majutsu-duplicate)
;;; majutsu-duplicate.el ends here
