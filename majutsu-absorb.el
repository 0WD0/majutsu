;;; majutsu-absorb.el --- Absorb transient for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides jj absorb transients, managing source and
;; destination selections and optional fileset filtering.

;;; Code:

(require 'majutsu)

(defclass majutsu-absorb-option (majutsu-selection-option)
  ())

(defclass majutsu-absorb--toggle-option (majutsu-selection-toggle-option)
  ((if-not :initform #'majutsu-interactive-selection-available-p)))

(defun majutsu-absorb--default-args ()
  "Return default args from diff buffer context."
  (with-current-buffer (majutsu-interactive--selection-buffer)
    (when (derived-mode-p 'majutsu-diff-mode)
      (when-let* ((source (or (when-let* ((rev (transient-arg-value
                                                "--revisions=" majutsu-buffer-diff-range)))
                                (concat "--from=" rev))
                              (when-let* ((from (transient-arg-value
                                                 "--from=" majutsu-buffer-diff-range)))
                                (concat "--from=" from)))))
        (list source)))))

(defun majutsu-absorb-arguments ()
  "Return the current absorb arguments.
If inside the transient, return transient args.
Otherwise, if no --from/--into is set and point is on a
jj-commit section, add --from from that section."
  (let ((args (if (eq transient-current-command 'majutsu-absorb)
                  (transient-args 'majutsu-absorb)
                '())))
    (unless (or (transient-arg-value "--from=" args)
                (transient-arg-value "--into=" args))
      (when-let* ((rev (magit-section-value-if 'jj-commit)))
        (push (concat "--from=" rev) args)))
    args))

;;;###autoload
(defun majutsu-absorb-execute (args)
  "Execute jj absorb with ARGS from the transient."
  (interactive (list (majutsu-absorb-arguments)))
  (pcase-let* ((`(,args ,filesets) (majutsu-filesets-split-transient-value args))
               (exit (apply #'majutsu-run-jj
                            "absorb"
                            (majutsu-jj-append-filesets args filesets))))
    (when (zerop exit)
      (message "Absorb completed"))))

;;; Infix Commands

(transient-define-argument majutsu-absorb:--from ()
  :description "From"
  :class 'majutsu-absorb-option
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :shortarg "-f"
  :argument "--from="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-absorb:--into ()
  :description "Into"
  :class 'majutsu-absorb-option
  :selection-label "[INTO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :shortarg "-t"
  :argument "--into="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-absorb:from ()
  :description "From (toggle at point)"
  :class 'majutsu-absorb--toggle-option
  :key "f"
  :argument "--from=")

(transient-define-argument majutsu-absorb:into ()
  :description "Into (toggle at point)"
  :class 'majutsu-absorb--toggle-option
  :key "t"
  :argument "--into="
  :multi-value 'repeat)

(transient-define-argument majutsu-absorb:-- ()
  :description "Limit to files"
  :class 'transient-files
  :key "--"
  :argument "--"
  :prompt "Limit to file,s: "
  :reader #'majutsu-read-files
  :multi-value t)

;;; Prefix

;;;###autoload(autoload 'majutsu-absorb "majutsu-absorb" nil t)
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
    ("RET" "Absorb" majutsu-absorb-execute)]]
  (interactive)
  (let* ((file (majutsu-file-at-point))
         (files (cond
                 (file (list file))
                 ((and (derived-mode-p 'majutsu-diff-mode)
                       majutsu-buffer-diff-filesets)
                  majutsu-buffer-diff-filesets)))
         (default-args (majutsu-absorb--default-args))
         (value (majutsu-filesets-build-transient-value default-args files)))
    (transient-setup
     'majutsu-absorb nil nil
     :scope (majutsu-selection-session-begin)
     :value value)))

;;; _
(provide 'majutsu-absorb)
;;; majutsu-absorb.el ends here
