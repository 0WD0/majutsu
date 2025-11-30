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

(defun majutsu-enter-dwim ()
  "Context-sensitive Enter key behavior."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ;; On a changeset/commit - edit it with jj edit
     ((and section
           (eq (oref section type) 'majutsu-revision-section)
           (slot-boundp section 'commit-id))
      (majutsu-edit-changeset-at-point))

     ;; On a diff hunk line - jump to that line in the file
     ((and section
           (eq (oref section type) 'majutsu-hunk-section)
           (slot-boundp section 'file))
      (majutsu-goto-diff-line))

     ;; On a file section - visit the file
     ((and section
           (eq (oref section type) 'majutsu-file-section)
           (slot-boundp section 'file))
      (majutsu-visit-file)))))

(defun majutsu-edit-changeset ()
  "Edit commit at point."
  (interactive)
  (when-let* ((revset (majutsu-log--revset-at-point)))
    (let ((result (majutsu-run-jj "edit" revset)))
      (if (majutsu--handle-command-result (list "edit" revset) result
                                          (format "Now editing commit %s" revset)
                                          "Failed to edit commit")
          (majutsu-log-refresh)))))

(defun majutsu-edit-changeset-at-point ()
  "Edit the commit at point using jj edit."
  (interactive)
  (when-let* ((revset (majutsu-log--revset-at-point)))
    (let ((result (majutsu-run-jj "edit" revset)))
      (if (majutsu--handle-command-result (list "edit" revset) result
                                          (format "Now editing revset %s" revset)
                                          "Failed to edit commit")
          (progn
            (majutsu-log-refresh)
            (back-to-indentation))))))

;;; _
(provide 'majutsu-edit)
;;; majutsu-edit.el ends here
