;;; majutsu-metaedit.el --- Metaedit transient for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides a transient frontend for `jj metaedit'.

;;; Code:

(require 'majutsu)

(defun majutsu-metaedit--default-revision ()
  "Return a single default revision for `jj metaedit'."
  (let ((revsets (magit-region-values 'jj-commit t)))
    (cond
     ((and revsets (> (length revsets) 1))
      (user-error "Metaedit only supports a single revision"))
     ((car revsets))
     ((magit-section-value-if 'jj-commit))
     (t "@"))))

(defun majutsu-metaedit--default-args ()
  "Return default arguments for `jj metaedit'."
  (list (concat "-r=" (majutsu-metaedit--default-revision))))

(defun majutsu-metaedit--read-revset (prompt initial-input _history)
  "Read a revset for metaedit transient options."
  (unless current-prefix-arg
    (majutsu-read-revset prompt
                         (or initial-input
                             (majutsu-metaedit--default-revision)))))

(defun majutsu-metaedit-arguments ()
  "Return the current metaedit arguments.
If inside the transient, return transient args, ensuring exactly one
revision argument is present. Outside the transient, return defaults."
  (let* ((inside-transient (eq transient-current-command 'majutsu-metaedit-transient))
         (args (if inside-transient
                   (transient-args 'majutsu-metaedit-transient)
                 '()))
         (rev-args (seq-filter (lambda (arg)
                                 (string-prefix-p "-r=" arg))
                               args)))
    (cond
     ((null rev-args)
      (append args (majutsu-metaedit--default-args)))
     ((cdr rev-args)
      (user-error "Metaedit only supports a single revision"))
     (t args))))

;;;###autoload
(defun majutsu-metaedit-execute (args)
  "Execute jj metaedit with ARGS from the transient."
  (interactive (list (majutsu-metaedit-arguments)))
  (let ((exit (apply #'majutsu-run-jj "metaedit" args)))
    (when (zerop exit)
      (message "Metaedit completed"))))

(transient-define-argument majutsu-metaedit:-r ()
  :description "Revision"
  :class 'transient-option
  :argument "-r="
  :reader #'majutsu-metaedit--read-revset)

(transient-define-argument majutsu-metaedit:--message ()
  :description "Message"
  :class 'transient-option
  :shortarg "-m"
  :argument "--message=")

(transient-define-argument majutsu-metaedit:--author ()
  :description "Author"
  :class 'transient-option
  :shortarg "-a"
  :argument "--author=")

(transient-define-argument majutsu-metaedit:--author-timestamp ()
  :description "Author timestamp"
  :class 'transient-option
  :shortarg "-t"
  :argument "--author-timestamp=")

(transient-define-argument majutsu-metaedit:--update-change-id ()
  :description "Update change-id"
  :class 'transient-switch
  :shortarg "-c"
  :argument "--update-change-id")

(transient-define-argument majutsu-metaedit:--update-author ()
  :description "Update author"
  :class 'transient-switch
  :shortarg "-u"
  :argument "--update-author")

(transient-define-argument majutsu-metaedit:--update-author-timestamp ()
  :description "Update author timestamp"
  :class 'transient-switch
  :shortarg "-U"
  :argument "--update-author-timestamp")

(transient-define-argument majutsu-metaedit:--force-rewrite ()
  :description "Force rewrite"
  :class 'transient-switch
  :shortarg "-f"
  :argument "--force-rewrite")

;;;###autoload
(defun majutsu-metaedit (&optional arg)
  "Open the metaedit transient.
With prefix ARG, pre-enable --ignore-immutable."
  (interactive "P")
  (transient-setup
   'majutsu-metaedit-transient nil nil
   :value (append (majutsu-metaedit-arguments)
                  (when arg '("--ignore-immutable")))))

(transient-define-prefix majutsu-metaedit-transient ()
  "Transient for jj metaedit operations."
  :man-page "jj-metaedit"
  :incompatible '(("--update-author" "--author=")
                  ("--update-author-timestamp" "--author-timestamp="))
  :transient-non-suffix t
  [:description "JJ Metaedit"
   :class transient-columns
   ["Selection"
    (majutsu-metaedit:-r)]
   ["Metadata"
    (majutsu-metaedit:--message)
    (majutsu-metaedit:--author)
    (majutsu-metaedit:--author-timestamp)]
   ["Options"
    (majutsu-metaedit:--update-change-id)
    (majutsu-metaedit:--update-author)
    (majutsu-metaedit:--update-author-timestamp)
    (majutsu-metaedit:--force-rewrite)
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("m" "Metaedit" majutsu-metaedit-execute)
    ("RET" "Metaedit" majutsu-metaedit-execute)
    ("q" "Quit" transient-quit-one)]])

;;; _
(provide 'majutsu-metaedit)
;;; majutsu-metaedit.el ends here
