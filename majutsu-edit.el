;;; majutsu-edit.el --- Edit changesets with Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library implements `jj edit' helpers.

;;; Code:

(require 'majutsu-jj)
(require 'majutsu-process)

;;;###autoload
(defun majutsu-edit-revision (revset &optional arg)
  "Edit REVSET.
With prefix ARG, pass --ignore-immutable.  Return non-nil on success."
  (interactive (list (majutsu-read-single-revset "Edit revision")
                     current-prefix-arg))
  (when (zerop (apply #'majutsu-run-jj
                      (append (list "edit" revset)
                              (when arg (list "--ignore-immutable")))))
    (message "Now editing commit %s" revset)
    t))

;;;###autoload
(defun majutsu-edit-changeset (&optional arg)
  "Edit commit at point.

With prefix ARG, pass --ignore-immutable.
When called from a blob buffer, also visit the workspace file."
  (interactive "P")
  (let ((in-blob (and (bound-and-true-p majutsu-blob-mode)
                      majutsu-buffer-blob-root
                      majutsu-buffer-blob-path)))
    (if-let* ((revset (or (majutsu-thing-at-point 'jj-revision t)
                          (majutsu-revision-at-point))))
        (when (majutsu-edit-revision revset arg)
          (when in-blob
            (let ((file (expand-file-name majutsu-buffer-blob-path
                                          majutsu-buffer-blob-root)))
              (find-file file))))
      (user-error "No revision at point"))))

;;; _
(provide 'majutsu-edit)
;;; majutsu-edit.el ends here
