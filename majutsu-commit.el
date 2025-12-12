;;; majutsu-commit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library implements jj commit and describe frontends using with-editor
;; and log context defaults.

;;; Code:

;;; majutsu-commit

;;;###autoload
(defun majutsu-commit ()
  "Create a commit using Emacs as the editor."
  (interactive)
  (majutsu--with-editor-run '("commit")
                            "Successfully committed changes"
                            "Failed to commit"))

;;; majutsu-describe

;;;###autoload
(defun majutsu-describe (&optional arg)
  "Update the description for the commit at point.
With prefix ARG, add --ignore-immutable."
  (interactive "P")
  (let* ((revset (or (majutsu-log--revset-at-point) "@"))
         (args (append (list "describe" "-r" revset)
                       (when arg '("--ignore-immutable")))))
    (majutsu--with-editor-run args
                              (format "Description updated for %s" revset)
                              "Failed to update description")))

;;; _
(provide 'majutsu-commit)
;;; majutsu-commit.el ends here
