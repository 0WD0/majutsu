;;; majutsu-simplify-parents.el --- Simplify-parents transient -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides a transient frontend for `jj simplify-parents'.

;;; Code:

(require 'majutsu)
(require 'majutsu-selection)

(defclass majutsu-simplify-parents-option (majutsu-selection-option)
  ())

(defclass majutsu-simplify-parents--toggle-option (majutsu-selection-toggle-option)
  ())

(defun majutsu-simplify-parents--default-args ()
  "Return default arguments for `jj simplify-parents'."
  (let* ((point-rev (magit-section-value-if 'jj-commit))
         (revsets (or (magit-region-values 'jj-commit t)
                      (and point-rev (list point-rev))
                      '("@"))))
    (mapcar (lambda (rev)
              (concat "--revisions=" rev))
            revsets)))

(defun majutsu-simplify-parents-arguments ()
  "Return current simplify-parents arguments.
If no targets are provided, default to region/point/@ as --revisions."
  (let ((args (if (eq transient-current-command 'majutsu-simplify-parents-transient)
                  (transient-args 'majutsu-simplify-parents-transient)
                '())))
    (if (seq-some (lambda (arg)
                    (or (string-prefix-p "--source=" arg)
                        (string-prefix-p "--revisions=" arg)))
                  args)
        args
      (append args (majutsu-simplify-parents--default-args)))))

(defun majutsu-simplify-parents-execute (args)
  "Execute jj simplify-parents with ARGS from transient."
  (interactive (list (majutsu-simplify-parents-arguments)))
  (let ((exit (apply #'majutsu-run-jj "simplify-parents" args)))
    (when (zerop exit)
      (message "Simplify parents completed"))))

(transient-define-argument majutsu-simplify-parents:--source ()
  :description "Source"
  :class 'majutsu-simplify-parents-option
  :selection-label "[SRC]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :key "-s"
  :argument "--source="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-simplify-parents:--revisions ()
  :description "Revisions"
  :class 'majutsu-simplify-parents-option
  :selection-label "[REVS]"
  :selection-face '(:background "dark orange" :foreground "black")
  :key "-r"
  :argument "--revisions="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-simplify-parents:source ()
  :description "Source (toggle at point)"
  :class 'majutsu-simplify-parents--toggle-option
  :key "s"
  :argument "--source="
  :multi-value 'repeat)

(transient-define-argument majutsu-simplify-parents:revisions ()
  :description "Revisions (toggle at point)"
  :class 'majutsu-simplify-parents--toggle-option
  :key "r"
  :argument "--revisions="
  :multi-value 'repeat)

;;;###autoload
(defun majutsu-simplify-parents (&optional arg)
  "Open the simplify-parents transient.
With prefix ARG, pre-enable --ignore-immutable."
  (interactive "P")
  (transient-setup
   'majutsu-simplify-parents-transient nil nil
   :scope (majutsu-selection-session-begin)
   :value (append (majutsu-simplify-parents--default-args)
                  (when arg '("--ignore-immutable")))))

(transient-define-prefix majutsu-simplify-parents-transient ()
  "Transient for jj simplify-parents operations."
  :man-page "jj-simplify-parents"
  :transient-non-suffix t
  [:description "JJ Simplify Parents"
   :class transient-columns
   ["Selection"
    (majutsu-simplify-parents:--source)
    (majutsu-simplify-parents:--revisions)
    (majutsu-simplify-parents:source)
    (majutsu-simplify-parents:revisions)
    ("c" "Clear selections" majutsu-selection-clear
     :transient t)]
   ["Options"
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("P" "Simplify" majutsu-simplify-parents-execute)
    ("RET" "Simplify" majutsu-simplify-parents-execute)
    ("q" "Quit" transient-quit-one)]])

;;; _
(provide 'majutsu-simplify-parents)
;;; majutsu-simplify-parents.el ends here
