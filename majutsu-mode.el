;;; majutsu-mode.el --- Base major mode for Majutsu buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Provides `majutsu-mode', the parent major mode from which all Majutsu
;; buffer modes derive, along with its keymap and shared helpers.

;;; Code:

(require 'majutsu-base)
(require 'magit-section)

;;; Keymap

(defvar-keymap majutsu-mode-map
  :doc "Parent keymap for modes derived from `majutsu-mode'."
  :parent magit-section-mode-map
  "RET" 'majutsu-enter-dwim
  "g"   'majutsu-log-refresh
  "q"   'quit-window
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

;;; Helpers

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

(provide 'majutsu-mode)
;;; majutsu-mode.el ends here
