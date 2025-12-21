;;; majutsu-config.el --- Config management for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;;; Code:

(require 'majutsu-process)

(defun majutsu-get (key)
  "Get config value for KEY from jj."
  (let ((output (majutsu-run-jj "config" "get" key)))
    (unless (string-empty-p output)
      (string-trim output))))

(defun majutsu-set (key value &optional scope)
  "Set config KEY to VALUE in SCOPE (user/repo/workspace).
SCOPE defaults to user."
  (let ((args (list "config" "set"
                    (pcase scope
                      ('repo "--repo")
                      ('workspace "--workspace")
                      (_ "--user"))
                    key value)))
    (apply #'majutsu-run-jj args)))

(defun majutsu-list (&optional prefix scope)
  "List config variables matching PREFIX in SCOPE.
Returns alist of (name . value) pairs."
  (let* ((args (append '("config" "list")
                       (when scope
                         (list (pcase scope
                                 ('repo "--repo")
                                 ('workspace "--workspace")
                                 ('user "--user"))))
                       (when prefix (list prefix))))
         (output (apply #'majutsu-run-jj args)))
    (when output
      (mapcar (lambda (line)
                (when (string-match "^\\([^=]+\\)=\"?\\(.*?\\)\"?$" line)
                  (cons (match-string 1 line)
                        (match-string 2 line))))
              (split-string output "\n" t)))))

(provide 'majutsu-config)
;;; majutsu-config.el ends here
