;;; majutsu-config.el --- Config management for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Functions for reading and writing jj configuration values.

;;; Code:

(require 'majutsu-process)

(defun majutsu-get (key)
  "Get config value for KEY from jj."
  (let ((lines (majutsu-jj-lines "config" "get" key)))
    (when lines
      (string-trim (car lines)))))

(defun majutsu-set (key value &optional scope)
  "Set config KEY to VALUE in SCOPE (user/repo/workspace).
SCOPE defaults to user."
  (let ((args (list "config" "set"
                    (pcase scope
                      ('repo "--repo")
                      ('workspace "--workspace")
                      (_ "--user"))
                    key value)))
    (majutsu-run-jj args)))

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
         (lines (majutsu-jj-lines args)))
    (when lines
      (mapcar (lambda (line)
                (when (string-match "^\\([^=]+\\)=\"?\\(.*?\\)\"?$" line)
                  (cons (match-string 1 line)
                        (match-string 2 line))))
              lines))))

(provide 'majutsu-config)
;;; majutsu-config.el ends here
