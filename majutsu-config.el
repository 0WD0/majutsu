;;; majutsu-config.el --- Config management for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Functions for reading and writing jj configuration values.

;;; Code:

(require 'majutsu-jj)

(autoload 'majutsu-run-jj "majutsu-process")

(defun majutsu-config-id-file (scope &optional root)
  "Return jj's SCOPE identity file for workspace ROOT.
SCOPE is `repo' or `workspace'.  Follow jj's repository pointer in
additional workspaces instead of treating .jj/repo as a directory."
  (when-let* ((root (or root (majutsu--buffer-root) (majutsu-toplevel)))
              (jj-dir (expand-file-name ".jj/" root)))
    (pcase-exhaustive scope
      ('workspace (expand-file-name "workspace-config-id" jj-dir))
      ('repo
       (let ((repo (expand-file-name "repo" jj-dir)))
         (when (file-regular-p repo)
           (setq repo
                 (with-temp-buffer
                   (insert-file-contents repo)
                   ;; The pointer is a path, not a line: whitespace is significant.
                   (majutsu-jj-expand-filename-from-jj (buffer-string) jj-dir))))
         (expand-file-name "config-id" (file-name-as-directory repo)))))))

(defun majutsu-config-id (scope &optional create root)
  "Return jj's SCOPE config identity for workspace ROOT.
Only CREATE permits asking jj to create the identity.  Reads never create
configuration.  Return nil when the identity does not exist."
  (when-let* ((root (or root (majutsu--buffer-root) (majutsu-toplevel)))
              (file (majutsu-config-id-file scope root)))
    (when (and create (not (file-readable-p file)))
      (let ((default-directory (file-name-as-directory root)))
        (majutsu-jj-string "config" "path"
                           (pcase-exhaustive scope
                             ('repo "--repo")
                             ('workspace "--workspace")))))
    (if (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (let ((id (string-trim (buffer-string))))
            (unless (string-empty-p id) id)))
      (when create
        (user-error "Cannot create jj %s config identity" scope)))))

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
