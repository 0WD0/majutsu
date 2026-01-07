;;; majutsu-restore.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library implements jj restore and abandon operations,
;; including a transient for restore with --from, --to, --changes-in support.

;;; Code:

(require 'majutsu)
(require 'majutsu-selection)

(defclass majutsu-restore-option (majutsu-selection-option)
  ((selection-key :initarg :selection-key :initform nil)))

(defclass majutsu-restore--toggle-option (majutsu-selection-toggle-option)
  ())

;;; Abandon

;;;###autoload
(defun majutsu-abandon ()
  "Abandon the changeset at point."
  (interactive)
  (if-let* ((revset (magit-section-value-if 'jj-commit)))
      (if (not (majutsu-confirm 'abandon
                                (format "Abandon changeset %s? " revset)))
          (message "Abandon canceled")
        (majutsu-run-jj "abandon" "-r" revset))
    (message "No changeset at point to abandon")))

;;; Restore

(defvar-local majutsu-restore--filesets nil
  "Filesets for the current restore operation.")

(defun majutsu-restore--default-from ()
  "Get default --from value from context."
  (when (derived-mode-p 'majutsu-diff-mode)
    (when-let* ((range majutsu-buffer-diff-range)
                (from (seq-find (lambda (arg) (string-prefix-p "--from=" arg)) range)))
      (substring from 7))))

(defun majutsu-restore--default-to ()
  "Get default --to value from context."
  (when (derived-mode-p 'majutsu-diff-mode)
    (when-let* ((range majutsu-buffer-diff-range)
                (to (seq-find (lambda (arg) (string-prefix-p "--to=" arg)) range)))
      (substring to 5))))

(defun majutsu-restore--file-at-point ()
  "Get file at point in diff buffer."
  (when (derived-mode-p 'majutsu-diff-mode)
    (when-let* ((section (magit-current-section)))
      (cond
       ((magit-section-match 'jj-hunk section)
        (magit-section-parent-value section))
       ((magit-section-match 'jj-file section)
        (oref section value))))))

;;;###autoload
(defun majutsu-restore-dwim ()
  "Restore working copy from parent (discard all changes).
In diff buffer on a file section, restore only that file."
  (interactive)
  (let ((file (majutsu-restore--file-at-point)))
    (if file
        (when (yes-or-no-p (format "Discard changes to %s? " file))
          (majutsu-run-jj "restore" file))
      (when (yes-or-no-p "Discard all working copy changes? ")
        (majutsu-run-jj "restore")))))

(defun majutsu-restore-execute (args)
  "Execute jj restore with ARGS from the transient."
  (interactive (list (transient-args 'majutsu-restore)))
  (let* ((filesets majutsu-restore--filesets)
         (cmd-args (append args filesets))
         (exit (apply #'majutsu-run-jj "restore" cmd-args)))
    (when (zerop exit)
      (message "Restored successfully"))))

(defun majutsu-restore-changes-in-execute (args)
  "Execute jj restore --changes-in with ARGS."
  (interactive (list (transient-args 'majutsu-restore)))
  (let* ((changes-in (seq-find (lambda (arg) (string-prefix-p "--changes-in=" arg)) args))
         (other-args (seq-remove (lambda (arg)
                                   (or (string-prefix-p "--from=" arg)
                                       (string-prefix-p "--to=" arg)
                                       (string-prefix-p "--changes-in=" arg)))
                                 args))
         (filesets majutsu-restore--filesets))
    (unless changes-in
      (if-let* ((rev (magit-section-value-if 'jj-commit)))
          (setq changes-in (format "--changes-in=%s" rev))
        (user-error "No revision selected for --changes-in")))
    (let* ((cmd-args (append (list changes-in) other-args filesets))
           (exit (apply #'majutsu-run-jj "restore" cmd-args)))
      (when (zerop exit)
        (message "Restored (undid changes in revision)")))))

;;; Infix Commands

(transient-define-argument majutsu-restore:--from ()
  :description "From"
  :class 'majutsu-restore-option
  :selection-key 'from
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :selection-type 'single
  :key "-f"
  :argument "--from="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-restore:--to ()
  :description "To"
  :class 'majutsu-restore-option
  :selection-key 'to
  :selection-label "[TO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :selection-type 'single
  :key "-t"
  :argument "--to="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-restore:--changes-in ()
  :description "Changes in"
  :class 'majutsu-restore-option
  :selection-key 'changes-in
  :selection-label "[CHANGES-IN]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-type 'single
  :key "-c"
  :argument "--changes-in="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-restore:from ()
  :description "From (toggle at point)"
  :class 'majutsu-restore--toggle-option
  :selection-key 'from
  :selection-type 'single
  :key "f"
  :argument "--from=")

(transient-define-argument majutsu-restore:to ()
  :description "To (toggle at point)"
  :class 'majutsu-restore--toggle-option
  :selection-key 'to
  :selection-type 'single
  :key "t"
  :argument "--to=")

(transient-define-argument majutsu-restore:changes-in ()
  :description "Changes-in (toggle at point)"
  :class 'majutsu-restore--toggle-option
  :selection-key 'changes-in
  :selection-type 'single
  :key "c"
  :argument "--changes-in=")

(defun majutsu-restore-clear-selections ()
  "Clear all restore selections."
  (interactive)
  (when (consp transient--suffixes)
    (dolist (obj transient--suffixes)
      (when (and (cl-typep obj 'majutsu-restore-option)
                 (memq (oref obj selection-key) '(from to changes-in)))
        (transient-infix-set obj nil))))
  (setq majutsu-restore--filesets nil)
  (when transient--prefix
    (transient--redisplay))
  (majutsu-selection-render)
  (message "Cleared all restore selections"))

(defun majutsu-restore--read-filesets (prompt &optional initial)
  "Read filesets with PROMPT and INITIAL value."
  (let ((input (read-string prompt (or initial ""))))
    (if (string-empty-p input)
        nil
      (split-string input))))

(defun majutsu-restore-set-filesets ()
  "Set filesets for restore."
  (interactive)
  (let* ((current (string-join (or majutsu-restore--filesets '()) " "))
         (new (majutsu-restore--read-filesets "Filesets (space-separated): " current)))
    (setq majutsu-restore--filesets new)
    (when transient--prefix
      (transient--redisplay))
    (message "Filesets: %s" (or (string-join new " ") "(all)"))))

(defun majutsu-restore--filesets-description ()
  "Return description for filesets display."
  (if majutsu-restore--filesets
      (format "Paths: %s" (string-join majutsu-restore--filesets " "))
    "Paths: (all)"))

(defun majutsu-restore--description ()
  "Return transient description with context info."
  (let ((parts (list "JJ Restore")))
    (when majutsu-restore--filesets
      (push (format "paths: %s" (string-join majutsu-restore--filesets " ")) parts))
    (string-join (nreverse parts) " | ")))

;;; Prefix

(transient-define-prefix majutsu-restore ()
  "Transient for jj restore operations."
  :man-page "jj-restore"
  :transient-non-suffix t
  [:description majutsu-restore--description
   ["Selection"
    (majutsu-restore:--from)
    (majutsu-restore:--to)
    (majutsu-restore:--changes-in)
    (majutsu-restore:from)
    (majutsu-restore:to)
    (majutsu-restore:changes-in)
    ("x" "Clear selections" majutsu-restore-clear-selections :transient t)]
   ["Paths"
    ("p" majutsu-restore--filesets-description majutsu-restore-set-filesets :transient t)]
   ["Options"
    ("-i" "Interactive" "--interactive")
    ("-d" "Restore descendants" "--restore-descendants")
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("r" "Restore" majutsu-restore-execute)
    ("u" "Undo changes-in" majutsu-restore-changes-in-execute)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  ;; Initialize from context
  (let ((from (majutsu-restore--default-from))
        (to (majutsu-restore--default-to))
        (file (majutsu-restore--file-at-point)))
    ;; Set filesets from context
    (setq majutsu-restore--filesets
          (cond
           (file (list file))
           ((and (derived-mode-p 'majutsu-diff-mode) majutsu-buffer-diff-filesets)
            majutsu-buffer-diff-filesets)
           (t nil)))
    (transient-setup
     'majutsu-restore nil nil
     :scope (majutsu-selection-session-begin)
     :value (delq nil
                  (list (and from (concat "--from=" from))
                        (and to (concat "--to=" to)))))))

;;; _
(provide 'majutsu-restore)
;;; majutsu-restore.el ends here
