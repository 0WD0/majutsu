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

(defvar majutsu-buffer-blob-root)
(defvar majutsu-buffer-blob-path)

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
    (let ((parent (majutsu-revision-at-point)))
      (majutsu-new--run-command (if parent
                                    (list "new" parent)
                                  (list "new"))))))

;;;###autoload
(defun majutsu-new-with-after ()
  "Create a new changeset with the commit at point as --after."
  (interactive)
  (if-let* ((after (majutsu-revision-at-point)))
      (majutsu-new--run-command (list "new" "--insert-after" after))
    (user-error "No revision at point")))

;;;###autoload
(defun majutsu-new-with-before ()
  "Create a new changeset with the commit at point as --before."
  (interactive)
  (if-let* ((before (majutsu-revision-at-point)))
      (majutsu-new--run-command (list "new" "--insert-before" before))
    (user-error "No revision at point")))

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
  :argument "-r"
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
  :key "r"
  :argument "-r"
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
  "Execute jj new with ARGS and refresh the log on success.
When called from a blob buffer, also visit the workspace file."
  (let ((in-blob (and (bound-and-true-p majutsu-blob-mode)
                      majutsu-buffer-blob-root
                      majutsu-buffer-blob-path))
        (blob-file (when (bound-and-true-p majutsu-blob-mode)
                     (expand-file-name majutsu-buffer-blob-path
                                       majutsu-buffer-blob-root))))
    (let ((exit (apply #'majutsu-call-jj args)))
      (if (zerop exit)
          (progn
            (message "Created new changeset")
            (majutsu-log-refresh)
            ;; Visit workspace file when in blob buffer
            (when in-blob
              (find-file blob-file))
            t)
        (majutsu-refresh)
        nil))))

(defun majutsu-new-arguments ()
  "Return the current new arguments.
If inside the transient, return transient args.
Otherwise, if no -r/--insert-after=/--insert-before= is set and point is on
a jj-commit section, add -r from that section."
  (let ((args (if (eq transient-current-command 'majutsu-new)
                  (transient-args 'majutsu-new)
                '())))
    (unless (cl-some (lambda (arg)
                       (or (string-prefix-p "-r" arg)
                           (string-prefix-p "--insert-after=" arg)
                           (string-prefix-p "--insert-before=" arg)))
                     args)
      (when-let* ((rev (magit-section-value-if 'jj-commit)))
        (push (concat "-r" rev) args)))
    args))

;;;###autoload
(defun majutsu-new-execute (args)
  "Execute jj new using the current transient selections."
  (interactive (list (majutsu-new-arguments)))
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
    ("o" "Create new change" majutsu-new-execute
     :description "Create new change")
    ("RET" "Create new change" majutsu-new-execute
     :description "Create new change")
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (transient-setup
   'majutsu-new nil nil
   :scope
   (majutsu-selection-session-begin)))

;;; _
(provide 'majutsu-new)
;;; majutsu-new.el ends here
