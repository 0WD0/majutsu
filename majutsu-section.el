;;; majutsu-section.el --- Helpers for magit-section in Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library contains generic helpers for working with `magit-section'.

;;; Code:

(require 'subr-x)
(require 'magit-section)

(defun majutsu-section-find (value &optional condition root)
  "Return the closest section matching VALUE.

When ROOT is non-nil, traverse from that section, otherwise from
`magit-root-section'."
  (let* ((root (or root (magit-root-section)))
         (anchor (or (and-let* ((cur (magit-current-section)))
                       (oref cur start))
                     (point)))
         (exact (and root value
                     (magit-get-section
                      (append `((,type . ,value)) (magit-section-ident root)))))
         best
         best-dist)
    (or exact
        (progn
          (magit-map-sections
           (##let ((value (magit-section-value-if condition it))
                   (when (and value (stringp value)
                              (or (string-prefix-p id value)
                                  (string-prefix-p value id)))
                     (let* ((pos (oref it start))
                            (dist (abs (- pos anchor))))
                       (when (or (null best-dist) (< dist best-dist))
                         (setq best it)
                         (setq best-dist dist))))))
           root))
        best)))

;;; _
(provide 'majutsu-section)
;;; majutsu-section.el ends here
