;;; majutsu-base.el --- Early utilities for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides early utilities and section subclasses that
;; other Majutsu modules rely on while avoiding heavier dependencies.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'eieio)
(require 'magit-section)
(require 'magit-mode)  ; for `majutsu-display-function'
(require 'majutsu-jj)

;;; Options

(defgroup majutsu nil
  "Interface to jj version control system."
  :group 'tools)

(defcustom majutsu-debug nil
  "Enable debug logging for jj operations."
  :type 'boolean
  :group 'majutsu)

(defcustom majutsu-show-command-output t
  "Show jj command output in messages."
  :type 'boolean
  :group 'majutsu)

(defcustom majutsu-confirm-critical-actions t
  "If non-nil, prompt for confirmation before critical operations."
  :type 'boolean
  :group 'majutsu)

(defconst majutsu--confirm-actions
  '((const undo)
    (const redo)
    (const abandon)
    (const rebase)
    (const workspace-forget))
  "Actions that may require confirmation.")

(defcustom majutsu-no-confirm nil
  "A list of symbols for actions Majutsu should not confirm, or t.

This is modeled after Magit's `magit-no-confirm`.  If this is t,
then no confirmation is required.  Otherwise, each symbol stands
for a class of actions that would normally ask for confirmation."
  :type `(choice (const :tag "Never require confirmation" t)
                 (set   :tag "Require confirmation except for"
                        ,@majutsu--confirm-actions))
  :group 'majutsu)

(defcustom majutsu-slow-confirm nil
  "A list of actions that should use `yes-or-no-p' instead of `y-or-n-p'."
  :type `(choice (const :tag "Use yes-or-no for all" t)
                 (set   :tag "Use yes-or-no for"
                        ,@majutsu--confirm-actions))
  :group 'majutsu)

;;; Section Classes

(defclass majutsu-commit-section (magit-section)
  ((overlay :initform nil
            :documentation "Selection overlay used by transient UIs.")
   (keymap :initform 'majutsu-commit-section-map)))

(defclass majutsu-diff-section (magit-section)
  ((keymap :initform 'majutsu-diff-section-map))
  :abstract t)

(defclass majutsu-file-section (majutsu-diff-section)
  ((keymap :initform 'majutsu-file-section-map)
   (header :initarg :header
           :initform nil
           :documentation "Raw file header text (diff --git + extended headers).")))

(defclass majutsu-hunk-section (majutsu-diff-section)
  ((keymap :initform 'majutsu-hunk-section-map)
   (fontified :initform nil)
   (combined :initarg :combined :initform nil)
   (from-range :initarg :from-range :initform nil)
   (from-ranges :initarg :from-ranges :initform nil)
   (to-range :initarg :to-range :initform nil)
   (about :initarg :about :initform nil)
   (painted :initform nil)
   (refined :initform nil)
   (heading-highlight-face :initform 'magit-diff-hunk-heading-highlight)
   (heading-selection-face :initform 'magit-diff-hunk-heading-selection)))

(setf (alist-get 'jj-commit magit--section-type-alist) 'majutsu-commit-section)
(setf (alist-get 'jj-file   magit--section-type-alist) 'majutsu-file-section)
(setf (alist-get 'jj-hunk   magit--section-type-alist) 'majutsu-hunk-section)

;; Workspace sections (`jj workspace list`)

(defclass majutsu-workspace-section (magit-section) ())

(setf (alist-get 'jj-workspace magit--section-type-alist) 'majutsu-workspace-section)

;;; Utilities

(defun majutsu--ensure-flag (args flag &optional position)
  "Return ARGS ensuring FLAG is present once.
POSITION may be `front' to insert FLAG at the beginning; otherwise FLAG
is appended."
  (if (member flag args)
      args
    (if (eq position 'front)
        (cons flag args)
      (append args (list flag)))))

(defun majutsu--debug (format-string &rest args)
  "Log debug message if `majutsu-debug' is enabled."
  (when majutsu-debug
    (message "[majutsu-mode] %s" (apply #'format format-string args))))

(defun majutsu--message-with-log (format-string &rest args)
  "Display message and log if debug is enabled."
  (let ((msg (apply #'format format-string args)))
    (majutsu--debug "User message: %s" msg)
    (message "%s" msg)))

(defun majutsu-y-or-n-p (prompt &optional action)
  "Ask PROMPT with y/n or yes/no depending on ACTION settings."
  (if (or (eq majutsu-slow-confirm t)
          (and action (member action majutsu-slow-confirm)))
      (yes-or-no-p prompt)
    (y-or-n-p prompt)))

(defun majutsu-confirm (action prompt)
  "Return non-nil if ACTION is confirmed.

ACTION is a symbol such as `rebase' or `abandon'.  PROMPT should
end with a question mark and space."
  (cond
   ((not majutsu-confirm-critical-actions) t)
   ((eq majutsu-no-confirm t) t)
   ((and action (memq action majutsu-no-confirm)) t)
   (t (majutsu-y-or-n-p prompt action))))

;;; Completing Read

(defun majutsu--make-completion-table (candidates &optional category)
  "Wrap CANDIDATES in a completion table.
When CATEGORY is non-nil, set it in metadata to control UI icons/styling."
  (let ((metadata `(metadata (display-sort-function . identity)
                             ,@(and category `((category . ,category))))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          metadata
        (complete-with-action action candidates string pred)))))

(defun majutsu-completing-read (prompt collection &optional predicate require-match
                                       initial-input hist def category)
  "Read a choice with completion, preserving CATEGORY metadata.
Like `completing-read' but uses `format-prompt' and supports CATEGORY
for completion UI styling (icons, grouping)."
  (let ((table (if category
                   (majutsu--make-completion-table collection category)
                 collection)))
    (completing-read (format-prompt prompt def)
                     table predicate require-match
                     initial-input hist def)))

(defun majutsu-completing-read-multiple (prompt collection &optional predicate require-match
                                                initial-input hist def category)
  "Read multiple choices with completion, preserving CATEGORY metadata.
Like `completing-read-multiple' but uses `format-prompt' and supports CATEGORY."
  (let ((table (if category
                   (majutsu--make-completion-table collection category)
                 collection)))
    (completing-read-multiple (format-prompt prompt def)
                              table predicate require-match
                              initial-input hist def)))

(defun majutsu-read-string (prompt &optional initial-input history default-value)
  "Read a string from the minibuffer, prompting with PROMPT.
Uses `format-prompt' for consistent formatting.  Empty input returns
DEFAULT-VALUE if non-nil, otherwise signals an error."
  (let ((val (read-string (format-prompt prompt default-value)
                          initial-input history default-value)))
    (if (string-empty-p val)
        (if default-value
            default-value
          (user-error "Need non-empty input"))
      val)))

(defun majutsu-display-buffer (buffer &optional kind display-function)
  "Display BUFFER using a function chosen for KIND or DISPLAY-FUNCTION.
If DISPLAY-FUNCTION is non-nil, call it directly.  Otherwise look up
KIND (a symbol such as `log', `diff' or `message') via
`majutsu-display-functions' and fall back to
`majutsu-default-display-function' when no match is found."
  (let* ((display-fn (or display-function
                         (majutsu-display-function kind))))
    (funcall display-fn buffer)
    (or (get-buffer-window buffer t)
        (selected-window))))

(defun majutsu--normalize-id-value (value)
  "Normalize VALUE (string/symbol/number) into a plain string without
text properties."
  (cond
   ((stringp value) (substring-no-properties value))
   ((and value (not (stringp value))) (format "%s" value))
   (t nil)))

(defun majutsu--buffer-root (&optional buffer)
  "Return the cached root for BUFFER (default `current-buffer').

This is used to match buffers to repositories when refreshing."
  (with-current-buffer (or buffer (current-buffer))
    (and (boundp 'majutsu--default-directory) majutsu--default-directory)))

(defun majutsu--find-mode-buffer (mode &optional root)
  "Return a live buffer in MODE for ROOT (or any repo when ROOT is nil)."
  (let ((root (or root (majutsu--buffer-root))))
    (seq-find (lambda (buf)
                (with-current-buffer buf
                  (and (derived-mode-p mode)
                       (or (null root)
                           (equal (majutsu--buffer-root buf) root)))))
              (buffer-list))))

(defun majutsu--resolve-mode-buffer (mode &optional root)
  "Prefer the current buffer if it is in MODE; otherwise find one for ROOT."
  (if (derived-mode-p mode)
      (current-buffer)
    (majutsu--find-mode-buffer mode root)))

(defun majutsu--assert-mode (mode)
  "Signal a user error unless the current buffer derives from MODE."
  (unless (derived-mode-p mode)
    (user-error "Command is only valid in %s buffers" mode)))

;;; _
(provide 'majutsu-base)
;;; majutsu-base.el ends here
