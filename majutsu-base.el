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
  "If non-nil, prompt for confirmation before undo/redo/abandon operations."
  :type 'boolean
  :group 'majutsu)

(defcustom majutsu-default-display-function #'pop-to-buffer
  "Fallback function used to display Majutsu buffers.
The function must accept one argument: the buffer to display."
  :type '(choice
          (function-item switch-to-buffer)
          (function-item pop-to-buffer)
          (function-item display-buffer)
          (function :tag "Custom function"))
  :group 'majutsu)

(defcustom majutsu-display-functions
  '((log . pop-to-buffer)
    (op-log . pop-to-buffer)
    (diff . pop-to-buffer)
    (message . pop-to-buffer))
  "Alist mapping Majutsu buffer kinds to display functions.
Each function must accept one argument: the buffer to display.
Add new entries here to extend display behavior for additional buffers."
  :type '(alist :key-type (symbol :tag "Buffer kind")
          :value-type (choice
                       (function-item switch-to-buffer)
                       (function-item pop-to-buffer)
                       (function-item display-buffer)
                       (function :tag "Custom function")))
  :group 'majutsu)

(defun majutsu--display-function (kind)
  "Return display function for KIND or the default fallback."
  (or (alist-get kind majutsu-display-functions nil nil #'eq)
      majutsu-default-display-function
      #'pop-to-buffer))

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

(defvar-local majutsu--repo-root nil
  "Cached repository root for the current buffer.")

(defun majutsu--root ()
  "Find root of the current repository."
  (let ((root (or (and (boundp 'majutsu--repo-root) majutsu--repo-root)
                  (locate-dominating-file default-directory ".jj"))))
    (unless root
      (user-error "Cannot find root -- not in a JJ repo"))
    root))

(defun majutsu--debug (format-string &rest args)
  "Log debug message if `majutsu-debug' is enabled."
  (when majutsu-debug
    (message "[majutsu-mode] %s" (apply #'format format-string args))))

(defun majutsu--message-with-log (format-string &rest args)
  "Display message and log if debug is enabled."
  (let ((msg (apply #'format format-string args)))
    (majutsu--debug "User message: %s" msg)
    (message "%s" msg)))

(defun majutsu-display-buffer (buffer &optional kind display-function)
  "Display BUFFER using a function chosen for KIND or DISPLAY-FUNCTION.
If DISPLAY-FUNCTION is non-nil, call it directly.  Otherwise look up
KIND (a symbol such as `log', `diff' or `message') via
`majutsu-display-functions' and fall back to
`majutsu-default-display-function' when no match is found."
  (let* ((display-fn (or display-function
                         (majutsu--display-function kind))))
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
  "Return the cached repo root for BUFFER (default `current-buffer').
Falls back to `majutsu--root' when available.  This is used to match
buffers to repositories when refreshing."
  (with-current-buffer (or buffer (current-buffer))
    (or (and (boundp 'majutsu--repo-root) majutsu--repo-root)
        (ignore-errors (majutsu--root)))))

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
