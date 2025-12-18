;;; majutsu-mode.el --- Base major mode for Majutsu buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library defines Majutsu's parent major mode, its keymap, and
;; shared buffer helpers.
 
;;; Code:

(require 'majutsu-base)

;;; Keymap

(defvar-keymap majutsu-mode-map
  :doc "Parent keymap for modes derived from `majutsu-mode'."
  :parent magit-section-mode-map
  "RET" 'majutsu-visit-thing
  "g"   'majutsu-refresh
  "q"   'quit-window
  "$"   'majutsu-process-buffer
  "l"   'majutsu-log-transient
  "?"   'majutsu-dispatch
  "c"   'majutsu-describe
  "C"   'majutsu-commit
  "N"   'majutsu-new
  "s"   'majutsu-squash-transient
  "d"   'majutsu-diff-transient
  "D"   'majutsu-diff
  "r"   'majutsu-rebase-transient
  "b"   'majutsu-bookmark-transient
  "y"   'majutsu-duplicate-transient
  "Y"   'majutsu-duplicate
  "G"   'majutsu-git-transient
  "a"   'majutsu-abandon
  "k"   'majutsu-abandon
  "C-/" 'majutsu-undo
  "C-?" 'majutsu-redo)

;;; Visit

(defun majutsu-visit-thing ()
  "Visit the thing at point.

This is a placeholder command.  Where applicable, section-specific
keymaps remap this command to another command that visits the thing at
  point."
  (declare (completion ignore))
  (interactive)
  (if-let* ((url (thing-at-point 'url t)))
      (browse-url url)
    (user-error "There is no thing at point that could be visited")))

;;; Helpers

(defun majutsu--refresh-buffer-function ()
  "Return the refresh function for the current Majutsu buffer, if any.
The function name is derived from `major-mode' by replacing the
\"-mode\" suffix with \"-refresh\", mirroring Magit's approach."
  (when (derived-mode-p 'majutsu-mode)
    (let* ((base (string-remove-suffix "-mode" (symbol-name major-mode)))
           (fn (intern (format "%s-refresh" base))))
      (and (fboundp fn)
           (not (memq fn '(majutsu-refresh majutsu-refresh-buffer)))
           fn))))

(defun majutsu-refresh-buffer (&optional _ignore-auto _noconfirm)
  "Refresh the current Majutsu buffer, ensuring the buffer type matches.
This is suitable for use as `revert-buffer-function'."
  (interactive)
  (majutsu--assert-mode 'majutsu-mode)
  (if-let* ((fn (majutsu--refresh-buffer-function)))
      (funcall fn)
    (user-error "No refresh function defined for %s" major-mode)))

(defun majutsu-refresh ()
  "Refresh the current Majutsu buffer (Magit-style `g`)."
  (interactive)
  (majutsu-refresh-buffer))

(defun majutsu-hack-dir-local-variables ()
  "Like `hack-dir-local-variables-non-file-buffer' but ignore some variables.
This prevents visual glitches (like red trailing whitespace) in Majutsu buffers
when the user has strict .dir-locals.el settings."
  (let ((ignored-local-variables
         (cons 'show-trailing-whitespace ignored-local-variables)))
    (hack-dir-local-variables-non-file-buffer)))

;;; Mode definition

(define-derived-mode majutsu-mode magit-section-mode "Majutsu"
  "Parent major mode from which Majutsu major modes inherit."
  :interactive nil
  :group 'majutsu
  (majutsu-hack-dir-local-variables))

;;; _
(provide 'majutsu-mode)

(with-eval-after-load 'evil
  (require 'majutsu-evil nil t))

;;; majutsu-mode.el ends here
