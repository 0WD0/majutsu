;;; majutsu-duplicate.el --- Duplicate command for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library implements jj duplicate commands and transients,
;; managing source and destination selections.

;;; Code:

(require 'majutsu)
(require 'majutsu-selection)

(defclass majutsu-duplicate-option (majutsu-selection-option)
  ())

(defclass majutsu-duplicate--toggle-option (majutsu-selection-toggle-option)
  ())

;;; Duplicate

(defun majutsu-duplicate-arguments ()
  "Return the current duplicate arguments.
If inside the transient, return transient args.
Otherwise, if no -r is set, add -r from point (or region values, or @)."
  (let ((args (if (eq transient-current-command 'majutsu-duplicate)
                  (transient-args 'majutsu-duplicate)
                '())))
    (unless (cl-some (lambda (arg) (string-prefix-p "-r" arg)) args)
      (let ((revsets (or (magit-region-values nil t)
                         (and (magit-section-value-if 'jj-commit)
                              (list (magit-section-value-if 'jj-commit)))
                         (list "@"))))
        (dolist (rev revsets)
          (push (concat "-r" rev) args))))
    args))

;;;###autoload
(defun majutsu-duplicate-dwim ()
  "Duplicate the changeset at point.
With prefix ARG, open the duplicate transient."
  (interactive)
  (let ((args  (majutsu-duplicate-arguments)))
    (majutsu-run-jj "duplicate" args)))

;;; Duplicate Transient
(transient-define-argument majutsu-duplicate:-r ()
  :description "Source"
  :class 'majutsu-duplicate-option
  :selection-label "[SRC]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :key "-r"
  :argument "-r"
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-duplicate:--onto ()
  :description "Onto"
  :class 'majutsu-duplicate-option
  :selection-label "[ONTO]"
  :selection-face '(:background "dark green" :foreground "white")
  :key "-o"
  :argument "--onto="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-duplicate:--after ()
  :description "After"
  :class 'majutsu-duplicate-option
  :selection-label "[AFTER]"
  :selection-face '(:background "dark blue" :foreground "white")
  :key "-A"
  :argument "--insert-after="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-duplicate:--before ()
  :description "Before"
  :class 'majutsu-duplicate-option
  :selection-label "[BEFORE]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :key "-B"
  :argument "--insert-before="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-duplicate:source ()
  :description "Source (toggle at point)"
  :class 'majutsu-duplicate--toggle-option
  :key "r"
  :argument "-r"
  :multi-value 'repeat)

(transient-define-argument majutsu-duplicate:onto ()
  :description "Onto (toggle at point)"
  :class 'majutsu-duplicate--toggle-option
  :key "o"
  :argument "--onto="
  :multi-value 'repeat)

(transient-define-argument majutsu-duplicate:after ()
  :description "After (toggle at point)"
  :class 'majutsu-duplicate--toggle-option
  :key "a"
  :argument "--insert-after="
  :multi-value 'repeat)

(transient-define-argument majutsu-duplicate:before ()
  :description "Before (toggle at point)"
  :class 'majutsu-duplicate--toggle-option
  :key "b"
  :argument "--insert-before="
  :multi-value 'repeat)

(transient-define-prefix majutsu-duplicate ()
  "Internal transient for jj duplicate."
  :man-page "jj-duplicate"
  :transient-non-suffix t
  [:description "JJ Duplicate"
   :class transient-columns
   ["Sources"
    (majutsu-duplicate:-r)
    (majutsu-duplicate:source)
    ("c" "Clear selections" majutsu-selection-clear
     :transient t)]
   ["Placement"
    (majutsu-duplicate:--onto)
    (majutsu-duplicate:--after)
    (majutsu-duplicate:--before)
    (majutsu-duplicate:onto)
    (majutsu-duplicate:after)
    (majutsu-duplicate:before)]
   ["Actions"
    ("y" "Duplicate changes" majutsu-duplicate-dwim)
    ("RET" "Duplicate changes" majutsu-duplicate-dwim)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (transient-setup
   'majutsu-duplicate nil nil
   :scope
   (majutsu-selection-session-begin)))

;;; _
(provide 'majutsu-duplicate)
;;; majutsu-duplicate.el ends here
