;;; majutsu-restore.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library implements abandon and restore-style operations,
;; reusing log selections and confirmation prompts.

;;; Code:

(require 'majutsu)

;;; Abandon

;;;###autoload
(defun majutsu-abandon ()
  "Abandon the changeset at point."
  (interactive)
  (if-let* ((revset (majutsu-log--revset-at-point)))
      (if (and majutsu-confirm-critical-actions
               (not (yes-or-no-p (format "Abandon changeset %s? " revset))))
          (message "Abandon canceled")
        (progn
          (majutsu-run-jj "abandon" "-r" revset)
          (majutsu-log-refresh)))
    (message "No changeset at point to abandon")))

;;; _
(provide 'majutsu-restore)
;;; majutsu-restore.el ends here
