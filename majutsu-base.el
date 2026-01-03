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
  "If non-nil, prompt for confirmation before undo/redo/abandon operations."
  :type 'boolean
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

(defun majutsu--root ()
  "Find root of the current repository."
  (let ((root (majutsu-toplevel)))
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
    (or (and (boundp 'majutsu--default-directory) majutsu--default-directory)
        (majutsu-toplevel))))

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
