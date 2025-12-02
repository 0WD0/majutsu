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

(defun majutsu-new (arg)
  "Create a new changeset.
Without prefix ARG, use the changeset at point (or `@` when unavailable).
With prefix ARG, open the new transient for interactive selection."
  (interactive "P")
  (if arg
      (majutsu-new-transient)
    (let* ((parent (majutsu-log--revset-at-point))
           (parents (when parent (list parent)))
           (args (majutsu-new--build-args
                  :parents parents)))
      (majutsu-new--run-command args))))

(defun majutsu-new-with-after ()
  "Create a new changeset with the commit at point as --after."
  (interactive)
  (let* ((after (majutsu-log--revset-at-point))
         (args (majutsu-new--build-args
                :after (when after (list after)))))
    (majutsu-new--run-command args)))

(defun majutsu-new-with-before ()
  "Create a new changeset with the commit at point as --before."
  (interactive)
  (let* ((before (majutsu-log--revset-at-point))
         (args (majutsu-new--build-args
                :before (when before (list before)))))
    (majutsu-new--run-command args)))

;;;###autoload
(defun majutsu-new-clear-selections ()
  "Clear all jj new selections and overlays."
  (interactive)
  (majutsu--entry-clear-overlays majutsu-new-parents)
  (majutsu--entry-clear-overlays majutsu-new-after)
  (majutsu--entry-clear-overlays majutsu-new-before)
  (setq majutsu-new-parents nil
        majutsu-new-after nil
        majutsu-new-before nil
        majutsu-new-message nil
        majutsu-new-no-edit nil)
  (when (called-interactively-p 'interactive)
    (message "Cleared all jj new selections")))

;;;###autoload
(defun majutsu-new-toggle-parent ()
  "Toggle the commit at point as a jj new parent."
  (interactive)
  (majutsu--selection-toggle-revsets
   :kind "parent"
   :label "[PARENT]"
   :face '(:background "dark orange" :foreground "black")
   :collection-var 'majutsu-new-parents))

;;;###autoload
(defun majutsu-new-toggle-after ()
  "Toggle the commit at point as a jj new --after target."
  (interactive)
  (majutsu--selection-toggle-revsets
   :kind "--after"
   :label "[AFTER]"
   :face '(:background "dark blue" :foreground "white")
   :collection-var 'majutsu-new-after))

;;;###autoload
(defun majutsu-new-toggle-before ()
  "Toggle the commit at point as a jj new --before target."
  (interactive)
  (majutsu--selection-toggle-revsets
   :kind "--before"
   :label "[BEFORE]"
   :face '(:background "dark magenta" :foreground "white")
   :collection-var 'majutsu-new-before))

;;;###autoload
(defun majutsu-new-edit-message ()
  "Prompt for a jj new commit message."
  (interactive)
  (let ((input (read-string "New change message (empty to clear): " majutsu-new-message)))
    (if (string-empty-p input)
        (setq majutsu-new-message nil)
      (setq majutsu-new-message input))
    (message (if majutsu-new-message
                 "Set message for jj new"
               "Cleared message for jj new"))))

;;;###autoload
(defun majutsu-new-toggle-no-edit ()
  "Toggle passing --no-edit to jj new."
  (interactive)
  (setq majutsu-new-no-edit (not majutsu-new-no-edit))
  (message "jj new --no-edit %s"
           (if majutsu-new-no-edit "enabled" "disabled")))

(defun majutsu-new--run-command (args)
  "Execute jj new with ARGS and refresh the log on success."
  (let ((result (apply #'majutsu-run-jj args)))
    (when (majutsu--handle-command-result
           args result
           "Created new changeset"
           "Failed to create new changeset")
      (majutsu-log-refresh (majutsu-current-change-id)
                           (majutsu-current-commit-id))
      t)))

;;;###autoload
(defun majutsu-new-execute ()
  "Execute jj new using the current transient selections."
  (interactive)
  (let ((args (majutsu-new--build-args)))
    (when (majutsu-new--run-command args)
      (majutsu-new-clear-selections))))

;;;###autoload
(defun majutsu-new-cleanup-on-exit ()
  "Clean up jj new selections when the transient exits."
  (majutsu-new-clear-selections)
  (remove-hook 'transient-exit-hook 'majutsu-new-cleanup-on-exit t))

;;;###autoload
(defun majutsu-new-transient ()
  "Open the jj new transient."
  (interactive)
  (add-hook 'transient-exit-hook 'majutsu-new-cleanup-on-exit nil t)
  (majutsu-new-transient--internal))

;;; New Transient

(defvar-local majutsu-new-parents nil
  "List of selected parent entries for jj new.")

(defvar-local majutsu-new-after nil
  "List of selected --after entries for jj new.")

(defvar-local majutsu-new-before nil
  "List of selected --before entries for jj new.")

(defvar-local majutsu-new-message nil
  "Cached commit message for jj new transient.")

(defvar-local majutsu-new-no-edit nil
  "Non-nil when jj new should pass --no-edit.")

(defun majutsu-new--message-preview ()
  "Return a truncated preview of `majutsu-new-message'."
  (when majutsu-new-message
    (truncate-string-to-width majutsu-new-message 30 nil nil "...")))

(defun majutsu-new--selection-summary ()
  "Return a list summarizing the current jj new selections."
  (let (parts)
    (when majutsu-new-parents
      (push (format "Parents: %s"
                    (string-join (mapcar #'majutsu--entry-display majutsu-new-parents)
                                 ", "))
            parts))
    (when majutsu-new-after
      (push (format "After: %s"
                    (string-join (mapcar #'majutsu--entry-display majutsu-new-after)
                                 ", "))
            parts))
    (when majutsu-new-before
      (push (format "Before: %s"
                    (string-join (mapcar #'majutsu--entry-display majutsu-new-before)
                                 ", "))
            parts))
    (when majutsu-new-message
      (push (format "Message: %s" (majutsu-new--message-preview)) parts))
    (when majutsu-new-no-edit
      (push "--no-edit" parts))
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

(cl-defun majutsu-new--build-args (&key parents after before message (no-edit majutsu-new-no-edit))
  "Build the argument list for jj new.
PARENTS, AFTER, BEFORE, MESSAGE, and NO-EDIT default to transient state."
  (let* ((parents (majutsu--selection-normalize-revsets (or parents majutsu-new-parents)))
         (after (majutsu--selection-normalize-revsets (or after majutsu-new-after)))
         (before (majutsu--selection-normalize-revsets (or before majutsu-new-before)))
         (message (or message majutsu-new-message))
         (args '("new")))
    (dolist (rev after)
      (setq args (append args (list "--after" rev))))
    (dolist (rev before)
      (setq args (append args (list "--before" rev))))
    (when (and message (not (string-empty-p message)))
      (setq args (append args (list "--message" message))))
    (when no-edit
      (setq args (append args '("--no-edit"))))
    (setq args (append args parents))
    args))

(transient-define-prefix majutsu-new-transient--internal ()
  "Internal transient for jj new operations."
  :man-page "jj-new"
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description majutsu-new--description
   :class transient-columns
   ["Selections"
    ("p" "Toggle parent" majutsu-new-toggle-parent
     :description (lambda ()
                    (format "Toggle parent (%d selected)"
                            (length majutsu-new-parents)))
     :transient t)
    ("a" "Toggle --after" majutsu-new-toggle-after
     :description (lambda ()
                    (format "Toggle --after (%d selected)"
                            (length majutsu-new-after)))
     :transient t)
    ("b" "Toggle --before" majutsu-new-toggle-before
     :description (lambda ()
                    (format "Toggle --before (%d selected)"
                            (length majutsu-new-before)))
     :transient t)
    ("c" "Clear selections" majutsu-new-clear-selections
     :transient t)]
   ["Options"
    ("m" "Set message" majutsu-new-edit-message
     :description (lambda ()
                    (if majutsu-new-message
                        (format "Set message (%s)"
                                (majutsu-new--message-preview))
                      "Set message"))
     :transient t)
    ("e" "Toggle --no-edit" majutsu-new-toggle-no-edit
     :description (lambda ()
                    (if majutsu-new-no-edit
                        "--no-edit (enabled)"
                      "--no-edit (disabled)"))
     :transient t)]
   ["Actions"
    ("n" "Create new change" majutsu-new-execute
     :description (lambda ()
                    (format "Create new change (%s)"
                            (majutsu-new--action-summary))))
    ("RET" "Create new change" majutsu-new-execute
     :description (lambda ()
                    (format "Create new change (%s)"
                            (majutsu-new--action-summary))))
    ("q" "Quit" transient-quit-one)]])

;;; _
(provide 'majutsu-new)
;;; majutsu-new.el ends here
