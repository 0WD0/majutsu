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

;;; majutsu-undo

;;;###autoload
(defun majutsu-undo ()
  "Undo the last change."
  (interactive)
  (if (not (majutsu-confirm 'undo "Undo the most recent change? "))
      (message "Undo canceled")
    (let ((revset (magit-section-value-if 'jj-commit)))
      (when (zerop (majutsu-run-jj "undo"))
        (when revset
          (majutsu-goto-commit revset))))))

;;; majutsu-redo

;;;###autoload
(defun majutsu-redo ()
  "Redo the last undone change."
  (interactive)
  (if (not (majutsu-confirm 'redo "Redo the previously undone change? "))
      (message "Redo canceled")
    (let ((revset (magit-section-value-if 'jj-commit)))
      (when (zerop (majutsu-run-jj "redo"))
        (when revset
          (majutsu-goto-commit revset))))))

;;; op log

(defclass majutsu-op-log-entry-section (magit-section)
  ((op-id :initarg :op-id)
   (user :initarg :user)
   (time :initarg :time)
   (description :initarg :description)))

(defconst majutsu--op-log-template
  (majutsu-tpl
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
             (output (or log-output (apply #'majutsu-jj-string args))))
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
  (magit-insert-section (oplog)
    (majutsu-op-log-insert-entries)))

(defun majutsu-op-log-refresh-buffer ()
  "Refresh the op log buffer."
  (interactive)
  (majutsu--assert-mode 'majutsu-op-log-mode)
  (setq majutsu-op-log--cached-entries nil)
  (majutsu-op-log-render))

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
  (let* ((root (majutsu--toplevel-safe))
         (repo (file-name-nondirectory (directory-file-name root))))
    (majutsu-setup-buffer #'majutsu-op-log-mode nil
      :buffer (format "*majutsu-op: %s*" repo)
      :directory root)))

;;; _
(provide 'majutsu-op)
;;; majutsu-op.el ends here
