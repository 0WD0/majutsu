;;; majutsu-edit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library implements helpers for jj edit, using log context to
;; choose targets and with-editor when needed.

;;; Code:

(require 'majutsu)

;;; Edit

;; TODO: 我现在完全没有实现这些东西
(defun majutsu-enter-dwim ()
  "Context-sensitive Enter key behavior."
  (interactive)
  (magit-section-case
    (jj-commit
     (majutsu-edit-changeset))
    (jj-hunk
     (majutsu-goto-diff-line))
    (jj-file
     (majutsu-visit-file))
    (jj-workspace
     (majutsu-workspace-visit))))

;;;###autoload
(defun majutsu-edit-changeset (&optional arg)
  "Edit commit at point.

With prefix ARG, pass --ignore-immutable.
When called from a blob buffer, also visit the workspace file."
  (interactive "P")
  (let ((in-blob (and (bound-and-true-p majutsu-blob-mode)
                      majutsu-buffer-blob-root
                      majutsu-buffer-blob-path)))
    (if-let* ((revset (majutsu-revision-at-point))
              (args (append (list "edit" revset)
                            (when arg (list "--ignore-immutable")))))
        (when (zerop (apply #'majutsu-run-jj args))
          (message "Now editing commit %s" revset)
          ;; Visit workspace file when in blob buffer
          (when in-blob
            (let ((file (expand-file-name majutsu-buffer-blob-path
                                          majutsu-buffer-blob-root)))
              (find-file file))))
      (user-error "No revision at point"))))

;;; _
(provide 'majutsu-edit)
;;; majutsu-edit.el ends here
