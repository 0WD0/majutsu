;;; majutsu-edit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

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

;;;###autoload
(defun majutsu-edit-changeset ()
  "Edit commit at point."
  (interactive)
  (when-let* ((revset (magit-section-value-if 'jj-commit)))
    (when (zerop (majutsu-call-jj "edit" revset))
      (message "Now editing commit %s" revset))))

(defun majutsu-edit-changeset-at-point ()
  "Edit the commit at point using jj edit."
  (interactive)
  (when-let* ((revset (magit-section-value-if 'jj-commit)))
    (when (zerop (majutsu-call-jj "edit" revset))
      (message "Now editing revset %s" revset)
      (back-to-indentation))))

;;; _
(provide 'majutsu-edit)
;;; majutsu-edit.el ends here
