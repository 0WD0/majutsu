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

;;;###autoload
(defun majutsu-new-clear-selections ()
  "Clear all jj new selections."
  (interactive)
  (majutsu-selection-clear)
  (when (called-interactively-p 'interactive)
    (message "Cleared all jj new selections")))

;;;###autoload
(defun majutsu-new-toggle-parent ()
  "Toggle the commit at point as a jj new parent."
  (interactive)
  (majutsu-selection-toggle 'parent))

;;;###autoload
(defun majutsu-new-toggle-after ()
  "Toggle the commit at point as a jj new --after target."
  (interactive)
  (majutsu-selection-toggle 'after))

;;;###autoload
(defun majutsu-new-toggle-before ()
  "Toggle the commit at point as a jj new --before target."
  (interactive)
  (majutsu-selection-toggle 'before))

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
(defun majutsu-new-execute ()
  "Execute jj new using the current transient selections."
  (interactive)
  (let* ((args (transient-args 'majutsu-new))
         (context-args (majutsu-new--build-revset-args))
         (args (cons "new" (append args context-args))))
    (when (majutsu-new--run-command args)
      (majutsu-selection-session-end))))

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
    ;; TODO: 这里的参数管理需要重构，和 diff 的对齐，才能引入 incompatible 设置
    ("p" "Toggle parent" majutsu-new-toggle-parent
     :description (lambda ()
                    (format "Toggle parent (%d selected)"
                            (majutsu-selection-count 'parent)))
     :transient t)
    ("a" "Toggle --after" majutsu-new-toggle-after
     :description (lambda ()
                    (format "Toggle --after (%d selected)"
                            (majutsu-selection-count 'after)))
     :transient t)
    ("b" "Toggle --before" majutsu-new-toggle-before
     :description (lambda ()
                    (format "Toggle --before (%d selected)"
                            (majutsu-selection-count 'before)))
     :transient t)
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
   (majutsu-selection-session-begin
    '((:key parent
       :label "[PARENT]"
       :face (:background "dark orange" :foreground "black")
       :type multi)
      (:key after
       :label "[AFTER]"
       :face (:background "dark blue" :foreground "white")
       :type multi)
      (:key before
       :label "[BEFORE]"
       :face (:background "dark magenta" :foreground "white")
       :type multi)))))

;;; _
(provide 'majutsu-new)
;;; majutsu-new.el ends here
