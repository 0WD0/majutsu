;;; majutsu-jjdescription.el --- Edit JJ descriptions -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;;; Code:

(require 'majutsu-jj)
(require 'majutsu-mode)
(require 'majutsu-process)

(require 'with-editor)
(require 'server)

(defvar recentf-exclude)
(defvar better-jumper-ignored-file-patterns)

(defconst majutsu-jjdescription-regexp
  (rx (seq (or string-start
               (seq (* (not (any ?\n)))
                    (any ?/ ?\\)))
           "editor-" (+ (in "0-9A-Za-z"))
           ".jjdescription" string-end))
  "Regexp matching temporary jj description files created for editing.")

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude majutsu-jjdescription-regexp))

(with-eval-after-load 'better-jumper
  (add-to-list 'better-jumper-ignored-file-patterns majutsu-jjdescription-regexp))

(add-to-list 'with-editor-file-name-history-exclude majutsu-jjdescription-regexp)

(add-to-list 'with-editor-server-window-alist
             (cons majutsu-jjdescription-regexp (majutsu-display-function 'message)))

(add-hook 'with-editor-filter-visit-hook #'majutsu--with-editor--apply-visit)
(add-hook 'server-visit-hook #'majutsu--with-editor--apply-visit)
(add-hook 'server-switch-hook #'majutsu--with-editor--apply-visit)

(defvar majutsu--with-editor-visit-queue nil
  "Queue of pending initializer functions for Majutsu with-editor buffers.")

(defun majutsu--with-editor--queue-visit (fn)
  "Enqueue FN so it runs when the next Majutsu editor buffer opens."
  (push fn majutsu--with-editor-visit-queue)
  fn)

(defun majutsu--with-editor--cancel-visit (fn)
  "Remove FN from the pending with-editor queue."
  (setq majutsu--with-editor-visit-queue
        (delq fn majutsu--with-editor-visit-queue)))

(defun majutsu--with-editor--apply-visit ()
  "Run the next pending Majutsu with-editor initializer if applicable."
  (let ((next '())
        handled)
    (dolist (fn majutsu--with-editor-visit-queue)
      (if (and (not handled)
               (with-demoted-errors "Majutsu with-editor init failed: %S"
                 (funcall fn)))
          (setq handled t)
        (push fn next)))
    (setq majutsu--with-editor-visit-queue (nreverse next))
    handled))

(defun majutsu--with-editor--target-buffer-p ()
  "Return non-nil when current buffer is a jj temporary editor file."
  (and buffer-file-name
       (string-match-p majutsu-jjdescription-regexp buffer-file-name)))

(defvar-local majutsu--with-editor-return-window nil
  "Window to restore after finishing a Majutsu with-editor session.")

(defvar-local majutsu--with-editor-return-buffer nil
  "Buffer to restore after finishing a Majutsu with-editor session.")

(defun majutsu--with-editor--restore-context ()
  "Restore window focus and buffer after a Majutsu with-editor session."
  (let ((window majutsu--with-editor-return-window)
        (buffer majutsu--with-editor-return-buffer))
    (cond
     ((and (window-live-p window)
           (buffer-live-p buffer))
      (with-selected-window window
        (switch-to-buffer buffer)))
     ((buffer-live-p buffer)
      (majutsu-display-buffer buffer 'message))))
  (remove-hook 'with-editor-post-finish-hook #'majutsu--with-editor--restore-context t)
  (remove-hook 'with-editor-post-cancel-hook #'majutsu--with-editor--restore-context t))

(defun majutsu--with-editor--visit-hook (window buffer)
  "Return initializer enabling `with-editor-mode' and tracking WINDOW/BUFFER."
  (let ((done nil))
    (lambda ()
      (when (and (not done)
                 (majutsu--with-editor--target-buffer-p))
        (setq done t)
        (when (fboundp 'with-editor-mode)
          (with-editor-mode 1))
        (goto-char (point-min))
        (majutsu--with-editor--setup-return window buffer)
        t))))

(defun majutsu--with-editor--setup-return (window buffer)
  "Remember WINDOW and BUFFER for restoring after the editor closes."
  (setq-local majutsu--with-editor-return-window window)
  (setq-local majutsu--with-editor-return-buffer buffer)
  (add-hook 'with-editor-post-finish-hook #'majutsu--with-editor--restore-context nil t)
  (add-hook 'with-editor-post-cancel-hook #'majutsu--with-editor--restore-context nil t))

;;; _
(provide 'majutsu-jjdescription)
;;; majutsu-jjdescription.el ends here
