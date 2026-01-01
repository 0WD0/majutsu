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
  (majutsu-run-jj-with-editor '("commit")))

;;; majutsu-describe

;;;###autoload
(defun majutsu-describe (&optional arg)
  "Update the description for the commit at point.
With prefix ARG, add --ignore-immutable."
  (interactive "P")
  (let* ((revset (or (magit-section-value-if 'jj-commit) "@"))
         (args (append (list "describe" "-r" revset)
                       (when arg '("--ignore-immutable")))))
    (majutsu-run-jj-with-editor args)))

;;; _
(provide 'majutsu-commit)
;;; majutsu-commit.el ends here
