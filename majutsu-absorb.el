;;; majutsu-absorb.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides jj absorb transients, managing source and
;; destination selections and optional fileset filtering.

;;; Code:

(require 'majutsu)
(require 'majutsu-file)
(require 'majutsu-selection)
(require 'majutsu-interactive)

(defclass majutsu-absorb-option (majutsu-selection-option)
  ())

(defclass majutsu-absorb--toggle-option (majutsu-selection-toggle-option)
  ())

(defun majutsu-absorb--default-args ()
  "Return default args from diff buffer context."
  (with-current-buffer (majutsu-interactive--selection-buffer)
    (when (derived-mode-p 'majutsu-diff-mode)
      (when-let* ((source
                   (seq-some (lambda (arg)
                               (cond
                                ((string-prefix-p "--revisions=" arg)
                                 (concat "--from=" (substring arg 12)))
                                ((string-prefix-p "--from=" arg)
                                 arg)))
                             majutsu-buffer-diff-range)))
        (list source)))))

(defun majutsu-absorb-arguments ()
  "Return the current absorb arguments.
If inside the transient, return transient args.
Otherwise, if no --from/--into is set and point is on a
jj-commit section, add --from from that section."
  (let ((args (if (eq transient-current-command 'majutsu-absorb)
                  (transient-args 'majutsu-absorb)
                '())))
    (unless (cl-some (lambda (arg)
                       (or (string-prefix-p "--from=" arg)
                           (string-prefix-p "--into=" arg)))
                     args)
      (when-let* ((rev (magit-section-value-if 'jj-commit)))
        (push (concat "--from=" rev) args)))
    args))

;;;###autoload
(defun majutsu-absorb-execute (args)
  "Execute jj absorb with ARGS from the transient."
  (interactive (list (majutsu-absorb-arguments)))
  (let ((exit (apply #'majutsu-run-jj "absorb" args)))
    (when (zerop exit)
      (message "Absorb completed"))))

;;; Infix Commands

(transient-define-argument majutsu-absorb:--from ()
  :description "From"
  :class 'majutsu-absorb-option
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :key "-f"
  :argument "--from="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-absorb:--into ()
  :description "Into"
  :class 'majutsu-absorb-option
  :selection-label "[INTO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :key "-t"
  :argument "--into="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-absorb:from ()
  :description "From (toggle at point)"
  :class 'majutsu-absorb--toggle-option
  :key "f"
  :argument "--from=")

(transient-define-argument majutsu-absorb:into ()
  :description "Into (toggle at point)"
  :class 'majutsu-absorb--toggle-option
  :key "t"
  :argument "--into=")

(transient-define-argument majutsu-absorb:-- ()
  :description "Limit to files"
  :class 'transient-files
  :key "--"
  :argument "--"
  :prompt "Limit to file,s: "
  :reader #'majutsu-read-files
  :multi-value t)

;;; Prefix

;;;###autoload
(transient-define-prefix majutsu-absorb ()
  "Transient for jj absorb operations."
  :man-page "jj-absorb"
  :transient-non-suffix t
  [
   :description "JJ Absorb"
   ["Selection"
    (majutsu-absorb:--from)
    (majutsu-absorb:--into)
    (majutsu-absorb:from)
    (majutsu-absorb:into)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Paths"
    (majutsu-absorb:--)]
   ["Options"
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("a" "Absorb" majutsu-absorb-execute)
    ("RET" "Absorb" majutsu-absorb-execute)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (let* ((file (majutsu-file-at-point))
         (files (cond
                 (file (list file))
                 ((and (derived-mode-p 'majutsu-diff-mode)
                       majutsu-buffer-diff-filesets)
                  majutsu-buffer-diff-filesets)))
         (default-args (majutsu-absorb--default-args))
         (value (if files
                    (append default-args (list (cons "--" files)))
                  default-args)))
    (transient-setup
     'majutsu-absorb nil nil
     :scope (majutsu-selection-session-begin)
     :value value)))

;;; _
(provide 'majutsu-absorb)
;;; majutsu-absorb.el ends here
