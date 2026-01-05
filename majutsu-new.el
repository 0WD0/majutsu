;;; majutsu-new.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

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
    (let* ((parent (magit-section-value-if 'jj-commit))
           (parents (when parent (list parent)))
           (args (majutsu-new--build-args
                  :parents parents)))
      (majutsu-new--run-command args))))

;;;###autoload
(defun majutsu-new-with-after ()
  "Create a new changeset with the commit at point as --after."
  (interactive)
  (let* ((after (magit-section-value-if 'jj-commit))
         (args (majutsu-new--build-args
                :after (when after (list after)))))
    (majutsu-new--run-command args)))

;;;###autoload
(defun majutsu-new-with-before ()
  "Create a new changeset with the commit at point as --before."
  (interactive)
  (let* ((before (magit-section-value-if 'jj-commit))
         (args (majutsu-new--build-args
                :before (when before (list before)))))
    (majutsu-new--run-command args)))

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
  :argument "--after="
  :multi-value t
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-new:--before ()
  :description "Before"
  :class 'majutsu-new-option
  :selection-key 'before
  :selection-label "[BEFORE]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-type 'multi
  :key "-B"
  :argument "--before="
  :multi-value t
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-new:parent ()
  :description "Parent (toggle at point)"
  :class 'majutsu-new--toggle-option
  :selection-key 'parent
  :selection-type 'multi
  :key "p"
  :argument "-r"
  :multi-value 'repeat)

(transient-define-argument majutsu-new:after ()
  :description "After (toggle at point)"
  :class 'majutsu-new--toggle-option
  :selection-key 'after
  :selection-type 'multi
  :key "a"
  :argument "--after="
  :multi-value t)

(transient-define-argument majutsu-new:before ()
  :description "Before (toggle at point)"
  :class 'majutsu-new--toggle-option
  :selection-key 'before
  :selection-type 'multi
  :key "b"
  :argument "--before="
  :multi-value t)

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
  (let* ((parents (seq-remove
                   #'string-empty-p
                   (mapcar (lambda (s) (substring s 2))
                           (seq-filter (lambda (s)
                                         (or (string-prefix-p "-r" s)
                                             (string-prefix-p "-o" s)))
                                       args))))
         (afters (mapcar (lambda (s) (substring s 8))
                         (seq-filter (lambda (s) (string-prefix-p "--after=" s)) args)))
         (befores (mapcar (lambda (s) (substring s 9))
                          (seq-filter (lambda (s) (string-prefix-p "--before=" s)) args)))
         (other-args (seq-filter (lambda (s)
                                   (not (or (string-prefix-p "-r" s)
                                            (string-prefix-p "-o" s)
                                            (string-prefix-p "--after=" s)
                                            (string-prefix-p "--before=" s))))
                                 args))
         (rev-args nil))
    (dolist (rev afters)
      (push "--after" rev-args)
      (push rev rev-args))
    (dolist (rev befores)
      (push "--before" rev-args)
      (push rev rev-args))
    (let ((final-args (append '("new") other-args (nreverse rev-args) parents)))
      (majutsu-new--run-command final-args))))

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

(cl-defun majutsu-new--build-revset-args (&key parents after before)
  "Build the revset argument list for jj new.
PARENTS, AFTER, BEFORE default to transient state."
  (let* ((parents (or parents (majutsu-selection-values 'parent)))
         (after (or after (majutsu-selection-values 'after)))
         (before (or before (majutsu-selection-values 'before)))
         args)
    (dolist (rev after)
      (setq args (append args (list "--after" rev))))
    (dolist (rev before)
      (setq args (append args (list "--before" rev))))
    (append args parents)))

(cl-defun majutsu-new--build-args (&rest args)
  "Legacy wrapper to build full jj new command args.
Accepts keys :parents, :after, :before."
  (cons "new" (apply #'majutsu-new--build-revset-args args)))

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
