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
(defun majutsu-edit-changeset (&optional arg)
  "Edit commit at point.

With prefix ARG, pass --ignore-immutable."
  (interactive "P")
  (when-let* ((revset (magit-section-value-if 'jj-commit))
              (args (append (list "edit" revset)
                            (when arg (list "--ignore-immutable")))))
    (when (zerop (apply #'majutsu-call-jj args))
      (message "Now editing commit %s" revset))))

;;; _
(provide 'majutsu-edit)
;;; majutsu-edit.el ends here
