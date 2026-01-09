;;; majutsu-new.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library implements the jj new workflow and transient,
;; including parent/after/before selection and message handling.

;;; Code:

(require 'majutsu)
(require 'majutsu-selection)

(defclass majutsu-new-option (majutsu-selection-option)
  ((selection-key :initarg :selection-key :initform nil)))

(defclass majutsu-new--toggle-option (majutsu-selection-toggle-option)
  ())

;;; majutsu-new

;;;###autoload
(defun majutsu-new-dwim (arg)
  "Create a new changeset.
Without prefix ARG, use the changeset at point (or `@` when unavailable).
With prefix ARG, open the new transient for interactive selection."
  (interactive "P")
  (if arg
      (call-interactively #'majutsu-new)
    (let ((parent (magit-section-value-if 'jj-commit)))
      (majutsu-new--run-command (if parent
                                    (list "new" parent)
                                  (list "new"))))))

;;;###autoload
(defun majutsu-new-with-after ()
  "Create a new changeset with the commit at point as --after."
  (interactive)
  (when-let* ((after (magit-section-value-if 'jj-commit)))
    (majutsu-new--run-command (list "new" "--insert-after" after))))

;;;###autoload
(defun majutsu-new-with-before ()
  "Create a new changeset with the commit at point as --before."
  (interactive)
  (when-let* ((before (magit-section-value-if 'jj-commit)))
    (majutsu-new--run-command (list "new" "--insert-before" before))))

;;; Options and Infixes

(transient-define-argument majutsu-new-infix-message ()
  :description "Message"
  :class 'transient-option
  :shortarg "-m"
  :argument "--message=")

(transient-define-argument majutsu-new-infix-no-edit ()
  :description "No edit"
  :class 'transient-switch
  :shortarg "-e"
  :argument "--no-edit")

(transient-define-argument majutsu-new:-r ()
  :description "Parent"
  :class 'majutsu-new-option
  :selection-key 'parent
  :selection-label "[PARENT]"
  :selection-face '(:background "dark orange" :foreground "black")
  :selection-type 'multi
  :key "-r"
  :argument ""
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-new:--after ()
  :description "After"
  :class 'majutsu-new-option
  :selection-key 'after
  :selection-label "[AFTER]"
  :selection-face '(:background "dark blue" :foreground "white")
  :selection-type 'multi
  :key "-A"
  :argument "--insert-after="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-new:--before ()
  :description "Before"
  :class 'majutsu-new-option
  :selection-key 'before
  :selection-label "[BEFORE]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-type 'multi
  :key "-B"
  :argument "--insert-before="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-new:parent ()
  :description "Parent (toggle at point)"
  :class 'majutsu-new--toggle-option
  :selection-key 'parent
  :selection-type 'multi
  :key "p"
  :argument ""
  :multi-value 'repeat)

(transient-define-argument majutsu-new:after ()
  :description "After (toggle at point)"
  :class 'majutsu-new--toggle-option
  :selection-key 'after
  :selection-type 'multi
  :key "a"
  :argument "--insert-after="
  :multi-value 'repeat)

(transient-define-argument majutsu-new:before ()
  :description "Before (toggle at point)"
  :class 'majutsu-new--toggle-option
  :selection-key 'before
  :selection-type 'multi
  :key "b"
  :argument "--insert-before="
  :multi-value 'repeat)

(defun majutsu-new-clear-selections ()
  "Clear all jj new selections."
  (interactive)
  (when (consp transient--suffixes)
    (dolist (obj transient--suffixes)
      (when (and (cl-typep obj 'majutsu-new-option)
                 (memq (oref obj selection-key) '(parent after before)))
        (transient-infix-set obj nil))))
  (when transient--prefix
    (transient--redisplay))
  (majutsu-selection-render)
  (message "Cleared all jj new selections"))

(defun majutsu-new--run-command (args)
  "Execute jj new with ARGS and refresh the log on success."
  (let ((exit (apply #'majutsu-call-jj args)))
    (if (zerop exit)
        (progn
          (message "Created new changeset")
          (majutsu-log-refresh)
          t)
      (majutsu-refresh)
      nil)))

;;;###autoload
(defun majutsu-new-execute (args)
  "Execute jj new using the current transient selections."
  (interactive (list (transient-args 'majutsu-new)))
  (majutsu-new--run-command (cons "new" args)))

;;; New Transient

(defun majutsu-new--selection-summary ()
  "Return a list summarizing the current jj new selections."
  (let (parts)
    (when-let* ((values (majutsu-selection-values 'parent)))
      (push (format "Parents: %s"
                    (string-join values ", "))
            parts))
    (when-let* ((values (majutsu-selection-values 'after)))
      (push (format "After: %s"
                    (string-join values ", "))
            parts))
    (when-let* ((values (majutsu-selection-values 'before)))
      (push (format "Before: %s"
                    (string-join values ", "))
            parts))
    (nreverse parts)))

(defun majutsu-new--description ()
  "Compose the transient description for jj new selections."
  (let ((parts (majutsu-new--selection-summary)))
    (if parts
        (concat "JJ New | " (string-join parts " | "))
      "JJ New")))

(defun majutsu-new--action-summary ()
  "Return a short summary string for the jj new execute action."
  (let ((parts (majutsu-new--selection-summary)))
    (if parts
        (string-join parts " | ")
      "Parents: @")))

(transient-define-prefix majutsu-new ()
  "Internal transient for jj new operations."
  :man-page "jj-new"
  :transient-non-suffix t
  [:description majutsu-new--description
   :class transient-columns
   ["Selections"
    (majutsu-new:-r)
    (majutsu-new:--after)
    (majutsu-new:--before)
    (majutsu-new:parent)
    (majutsu-new:after)
    (majutsu-new:before)
    ("c" "Clear selections" majutsu-new-clear-selections
     :transient t)]
   ["Options"
    (majutsu-new-infix-message)
    (majutsu-new-infix-no-edit)
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("n" "Create new change" majutsu-new-execute
     :description (lambda ()
                    (format "Create new change (%s)"
                            (majutsu-new--action-summary))))
    ("RET" "Create new change" majutsu-new-execute
     :description (lambda ()
                    (format "Create new change (%s)"
                            (majutsu-new--action-summary))))
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (transient-setup
   'majutsu-new nil nil
   :scope
   (majutsu-selection-session-begin)))

;;; _
(provide 'majutsu-new)
;;; majutsu-new.el ends here
