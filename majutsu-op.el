;;; majutsu-op.el --- JJ Operation view for majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides helpers and transients for jj op workflows.

;;; Code:

(require 'majutsu)
(require 'eieio)

;;; majutsu-undo

(defun majutsu-undo ()
  "Undo the last change."
  (interactive)
  (if (and majutsu-confirm-critical-actions
           (not (yes-or-no-p "Undo the most recent change? ")))
      (message "Undo canceled")
    (let ((revset (majutsu-log--revset-at-point)))
      (majutsu-run-jj "undo")
      (majutsu-log-refresh)
      (when revset
        (majutsu-goto-commit revset)))))

;;; majutsu-redo

(defun majutsu-redo ()
  "Redo the last undone change."
  (interactive)
  (if (and majutsu-confirm-critical-actions
           (not (yes-or-no-p "Redo the previously undone change? ")))
      (message "Redo canceled")
    (let ((revset (majutsu-log--revset-at-point)))
      (majutsu-run-jj "redo")
      (majutsu-log-refresh)
      (when revset
        (majutsu-goto-commit revset)))))

;;; op log

(defclass majutsu-op-log-entry-section (magit-section)
  ((op-id :initarg :op-id)
   (user :initarg :user)
   (time :initarg :time)
   (description :initarg :description)))

(defconst majutsu--op-log-template
  (tpl-compile
   [:separate "\x1e"
              [:call 'id.short]
              [:user]
              [:method [:call 'time.end] :format "%Y-%m-%d %H:%M"]
              [:description]
              "\n"]
   'Operation))

(defvar-local majutsu-op-log--cached-entries nil
  "Cached operation log entries.")

(defun majutsu-parse-op-log-entries (&optional buf log-output)
  "Parse jj op log output."
  (if (and majutsu-op-log--cached-entries (not log-output))
      majutsu-op-log--cached-entries
    (with-current-buffer (or buf (current-buffer))
      (let* ((args (list "op" "log" "--no-graph" "-T" majutsu--op-log-template))
             (output (or log-output (apply #'majutsu-run-jj args))))
        (when (and output (not (string-empty-p output)))
          (let ((lines (split-string output "\n" t))
                (entries '()))
            (dolist (line lines)
              (seq-let (id user time desc) (split-string line "\x1e")
                (push (list :op-id id :user user :time time :desc desc) entries)))
            (nreverse entries)))))))

(defun majutsu-op-log-insert-entries ()
  "Insert operation log entries."
  (magit-insert-section (majutsu-log-graph-section)
    (magit-insert-heading "Operation Log")
    (dolist (entry (majutsu-parse-op-log-entries))
      (magit-insert-section
          (majutsu-op-log-entry-section entry t
                                        :op-id (plist-get entry :op-id))
        (magit-insert-heading
          (format "%-12s %-15s %-16s %s"
                  (propertize (plist-get entry :op-id) 'face 'font-lock-constant-face)
                  (propertize (plist-get entry :user) 'face 'font-lock-variable-name-face)
                  (propertize (plist-get entry :time) 'face 'font-lock-comment-face)
                  (plist-get entry :desc)))
        (insert "\n")))))

(defun majutsu-op-log-render ()
  "Render the op log buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (oplog)
      (majutsu-op-log-insert-entries))))

(defun majutsu-op-log-refresh ()
  "Refresh the op log buffer."
  (interactive)
  (majutsu--assert-mode 'majutsu-op-log-mode)
  (let ((root (majutsu--root))
        (buf (current-buffer)))
    (setq-local majutsu--repo-root root)
    (setq default-directory root)
    (setq majutsu-op-log--cached-entries nil)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Loading..."))
    (majutsu-run-jj-async
     (list "op" "log" "--no-graph" "-T" majutsu--op-log-template)
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (derived-mode-p 'majutsu-op-log-mode)
             (setq majutsu-op-log--cached-entries (majutsu-parse-op-log-entries nil output))
             (majutsu-op-log-render)))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (derived-mode-p 'majutsu-op-log-mode)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert "Error: " err)))))))))

(defvar-keymap majutsu-op-log-mode-map
  :doc "Keymap for `majutsu-op-log-mode'."
  :parent majutsu-mode-map)

(define-derived-mode majutsu-op-log-mode majutsu-mode "Majutsu Op Log"
  "Major mode for viewing jj operation log."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

(defun majutsu-op-log ()
  "Open the majutsu operation log."
  (interactive)
  (let* ((root (majutsu--root))
         (buffer (get-buffer-create (format "*majutsu-op: %s*" (file-name-nondirectory (directory-file-name root))))))
    (with-current-buffer buffer
      (majutsu-op-log-mode)
      (setq-local majutsu--repo-root root)
      (setq default-directory root)
      (majutsu-op-log-refresh))
    (majutsu-display-buffer buffer 'op-log)))

;;; _
(provide 'majutsu-op)
;;; majutsu-op.el ends here
