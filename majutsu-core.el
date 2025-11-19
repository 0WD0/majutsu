;;; majutsu-core.el --- Core definitions for majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Core definitions, custom variables, and utility functions for Majutsu.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)

(defgroup majutsu nil
  "Interface to jj version control system."
  :group 'tools)

;;; Customization

(defcustom majutsu-executable "jj"
  "Path to jj executable."
  :type 'string
  :group 'majutsu)

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

(defcustom majutsu-with-editor-envvar "JJ_EDITOR"
  "Environment variable used to tell jj which editor to invoke."
  :type 'string
  :group 'majutsu)

(defcustom majutsu-log-display-function #'pop-to-buffer
  "Function called to display the majutsu log buffer.
The function must accept one argument: the buffer to display."
  :type '(choice
          (function-item switch-to-buffer)
          (function-item pop-to-buffer)
          (function-item display-buffer)
          (function :tag "Custom function"))
  :group 'majutsu)

(defcustom majutsu-message-display-function #'pop-to-buffer
  "Function called to display the majutsu with-editor message buffer
The function must accept one argument: the buffer to display."
  :type '(choice
          (function-item switch-to-buffer)
          (function-item pop-to-buffer)
          (function-item display-buffer)
          (function :tag "Custom function"))
  :group 'majutsu)

;;; Mode

(defvar-keymap majutsu-mode-map
  :doc "Parent keymap for modes derived from `majutsu-mode'."
  :parent magit-section-mode-map
  "RET" 'majutsu-enter-dwim
  "g"   'majutsu-log-refresh
  "q"   'quit-window
  "l"   'majutsu-log-transient
  "?"   'majutsu-dispatch
  "c"   'majutsu-commit
  "d"   'majutsu-describe
  "N"   'majutsu-new
  "S"   'majutsu-squash-transient
  "D"   'majutsu-diff-transient
  "R"   'majutsu-rebase-transient
  "B"   'majutsu-bookmark-transient
  "Y"   'majutsu-duplicate-transient
  "G"   'majutsu-git-transient
  "u"   'majutsu-undo
  "U"   'majutsu-redo
  "X"   'majutsu-abandon)

(define-derived-mode majutsu-mode magit-section-mode "Majutsu"
  "Parent major mode from which Majutsu major modes inherit."
  :interactive nil
  :group 'majutsu)

;;; Utils

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
  "Log debug message if majutsu-debug is enabled."
  (when majutsu-debug
    (message "[majutsu-mode] %s" (apply #'format format-string args))))

(defun majutsu--message-with-log (format-string &rest args)
  "Display message and log if debug enabled."
  (let ((msg (apply #'format format-string args)))
    (majutsu--debug "User message: %s" msg)
    (message "%s" msg)))

(defun majutsu--display-buffer-for-editor (buffer &optional window)
  "Display BUFFER using `majutsu-log-display-function'.
When WINDOW is a live window, run the display function in that window.
Return the window showing BUFFER."
  (let ((display-fn (or majutsu-log-display-function #'pop-to-buffer)))
    (if (window-live-p window)
        (with-selected-window window
          (funcall display-fn buffer))
      (funcall display-fn buffer))
    (or (get-buffer-window buffer t)
        (selected-window))))

(defun majutsu--normalize-id-value (value)
  "Normalize VALUE (string/symbol/number) into a plain string without
text properties."
  (cond
   ((stringp value) (substring-no-properties value))
   ((and value (not (stringp value))) (format "%s" value))
   (t nil)))

(provide 'majutsu-core)
;;; majutsu-core.el ends here
