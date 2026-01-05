;;; majutsu-duplicate.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

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

(defun majutsu-duplicate--run-command (args)
  "Execute jj duplicate with ARGS and refresh log."
  (when (zerop (apply #'majutsu-run-jj args))
    (message "Duplicated changeset(s)")
    t))

(defun majutsu-duplicate-execute (args)
  "Execute jj duplicate using transient selections."
  (interactive (list (transient-args 'majutsu-duplicate)))
  (let* ((sources (mapcar (lambda (s) (substring s 9))
                          (seq-filter (lambda (s) (string-prefix-p "--source=" s)) args)))
         (ontos (mapcar (lambda (s) (substring s 7))
                        (seq-filter (lambda (s) (string-prefix-p "--onto=" s)) args)))
         (afters (mapcar (lambda (s) (substring s 8))
                         (seq-filter (lambda (s) (string-prefix-p "--after=" s)) args)))
         (befores (mapcar (lambda (s) (substring s 9))
                          (seq-filter (lambda (s) (string-prefix-p "--before=" s)) args)))
         (cmd-args '("duplicate")))
    (unless sources
      (when-let* ((rev (magit-section-value-if 'jj-commit)))
        (setq sources (list rev))))
    (unless sources (setq sources '("@")))
    (dolist (rev ontos)
      (setq cmd-args (append cmd-args (list "--onto" rev))))
    (dolist (rev afters)
      (setq cmd-args (append cmd-args (list "--after" rev))))
    (dolist (rev befores)
      (setq cmd-args (append cmd-args (list "--before" rev))))
    (setq cmd-args (append cmd-args sources))
    (majutsu-duplicate--run-command cmd-args)))

;;;###autoload
(defun majutsu-duplicate-dwim (arg)
  "Duplicate the changeset at point.
With prefix ARG, open the duplicate transient."
  (interactive "P")
  (if arg
      (call-interactively #'majutsu-duplicate)
    (let* ((rev (magit-section-value-if 'jj-commit))
           (args (majutsu-duplicate--build-args
                  :sources (list (or rev "@")))))
      (majutsu-duplicate--run-command args))))

;;; Duplicate Transient
(transient-define-argument majutsu-duplicate:--source ()
  :description "Source"
  :class 'majutsu-duplicate-option
  :selection-key 'source
  :selection-label "[SRC]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :selection-type 'multi
  :key "-s"
  :argument "--source="
  :multi-value t
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
  :multi-value t
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-duplicate:--after ()
  :description "After"
  :class 'majutsu-duplicate-option
  :selection-key 'after
  :selection-label "[AFTER]"
  :selection-face '(:background "dark blue" :foreground "white")
  :selection-type 'multi
  :key "-a"
  :argument "--after="
  :multi-value t
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-duplicate:--before ()
  :description "Before"
  :class 'majutsu-duplicate-option
  :selection-key 'before
  :selection-label "[BEFORE]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-type 'multi
  :key "-b"
  :argument "--before="
  :multi-value t
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-duplicate:source ()
  :description "Source (toggle at point)"
  :class 'majutsu-duplicate--toggle-option
  :selection-key 'source
  :selection-type 'multi
  :key "y"
  :argument "--source="
  :multi-value t)

(transient-define-argument majutsu-duplicate:onto ()
  :description "Onto (toggle at point)"
  :class 'majutsu-duplicate--toggle-option
  :selection-key 'onto
  :selection-type 'multi
  :key "o"
  :argument "--onto="
  :multi-value t)

(transient-define-argument majutsu-duplicate:after ()
  :description "After (toggle at point)"
  :class 'majutsu-duplicate--toggle-option
  :selection-key 'after
  :selection-type 'multi
  :key "a"
  :argument "--after="
  :multi-value t)

(transient-define-argument majutsu-duplicate:before ()
  :description "Before (toggle at point)"
  :class 'majutsu-duplicate--toggle-option
  :selection-key 'before
  :selection-type 'multi
  :key "b"
  :argument "--before="
  :multi-value t)

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

(cl-defun majutsu-duplicate--build-args (&key sources destinations after before)
  "Legacy builder."
  (let* ((sources (or sources
                      (majutsu-selection-values 'source)
                      (list (or (magit-section-value-if 'jj-commit) "@"))))
         (destinations (or destinations (majutsu-selection-values 'onto)))
         (after (or after (majutsu-selection-values 'after)))
         (before (or before (majutsu-selection-values 'before)))
         (args '("duplicate")))
    (dolist (rev destinations)
      (setq args (append args (list "--onto" rev))))
    (dolist (rev after)
      (setq args (append args (list "--after" rev))))
    (dolist (rev before)
      (setq args (append args (list "--before" rev))))
    (setq args (append args sources))
    args))

(transient-define-prefix majutsu-duplicate ()
  "Internal transient for jj duplicate."
  :man-page "jj-duplicate"
  :transient-non-suffix t
  [:description "JJ Duplicate"
   :class transient-columns
   ["Sources"
    (majutsu-duplicate:--source)
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
