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

(defun majutsu-simplify-parents--dwim-args ()
  "Return DWIM target args for simplify-parents execution."
  (mapcar (lambda (rev) (concat "--revision=" rev))
          (or (magit-region-values 'jj-commit t)
              (when-let* ((rev (or (majutsu-thing-at-point 'jj-revision t)
                                   (majutsu-revision-at-point))))
                (list rev))
              '("@"))))

(defun majutsu-simplify-parents-execute (args)
  "Execute jj simplify-parents with ARGS from transient."
  (interactive (list (transient-args 'majutsu-simplify-parents-transient)))
  (let* ((args (if (or (transient-arg-value "--source=" args)
                       (transient-arg-value "--revision=" args))
                   args
                 (append args (majutsu-simplify-parents--dwim-args))))
         (exit (apply #'majutsu-run-jj "simplify-parents" args)))
    (when (zerop exit)
      (message "Simplify parents completed"))))

(transient-define-argument majutsu-simplify-parents:--source ()
  :description "Source"
  :class 'majutsu-simplify-parents-option
  :selection-label "[SRC]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :shortarg "-s"
  :argument "--source="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-simplify-parents:--revision ()
  :description "Revision"
  :class 'majutsu-simplify-parents-option
  :selection-label "[REV]"
  :selection-face '(:background "dark orange" :foreground "black")
  :shortarg "-r"
  :argument "--revision="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-simplify-parents:source ()
  :description "Source (toggle at point)"
  :class 'majutsu-simplify-parents--toggle-option
  :key "s"
  :argument "--source="
  :multi-value 'repeat)

(transient-define-argument majutsu-simplify-parents:revision ()
  :description "Revision (toggle at point)"
  :class 'majutsu-simplify-parents--toggle-option
  :key "r"
  :argument "--revision="
  :multi-value 'repeat)

;;;###autoload
(defun majutsu-simplify-parents ()
  "Open the simplify-parents transient."
  (interactive)
  (transient-setup
   'majutsu-simplify-parents-transient nil nil
   :scope (majutsu-selection-session-begin)))

(transient-define-prefix majutsu-simplify-parents-transient ()
  "Transient for jj simplify-parents operations."
  :man-page "jj-simplify-parents"
  :transient-non-suffix t
  [:description
   "JJ Simplify Parents"
   ["Selection"
    (majutsu-simplify-parents:--source)
    (majutsu-simplify-parents:--revision)
    (majutsu-simplify-parents:source)
    (majutsu-simplify-parents:revision)
    ("c" "Clear selections" majutsu-selection-clear
     :transient t)]
   ["Options"
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("P" "Simplify" majutsu-simplify-parents-execute)
    ("RET" "Simplify" majutsu-simplify-parents-execute)]])

;;; _
(provide 'majutsu-simplify-parents)
;;; majutsu-simplify-parents.el ends here
