;;; majutsu-absorb.el --- Absorb transient for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides jj absorb transients, managing source and
;; destination selections, partial patch selection, and fileset filtering.

;;; Code:

(require 'majutsu)
(require 'majutsu-diff-editor)

(defun majutsu-absorb--default-args ()
  "Return default args from diff buffer context."
  (when (derived-mode-p 'majutsu-diff-mode)
    (when-let* ((source (or (car (majutsu-jj-option-values
                                 majutsu-buffer-diff-range "--revisions" "-r"))
                            (car (majutsu-jj-option-values
                                  majutsu-buffer-diff-range "--from" "-f")))))
      (list (concat "--from=" source)))))

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

(defun majutsu-absorb--check-patch-source (args patch-source)
  "Signal when ARGS would absorb from a commit other than PATCH-SOURCE."
  (unless patch-source
    (user-error "Patch selection for absorb requires a diff for exactly one commit"))
  (let ((sources (majutsu-jj-option-values args "--from" "-f")))
    (unless (<= (length sources) 1)
      (user-error "Patch selection for absorb requires exactly one source"))
    (let ((source (majutsu-jj-resolve-single-commit
                   (if sources (car sources) "@"))))
      (unless (and source (equal source patch-source))
        (user-error "Patch selection for absorb requires the rendered diff source")))))

;;;###autoload(autoload 'majutsu-absorb-execute "majutsu-absorb" nil t)
(transient-define-suffix majutsu-absorb-execute (args)
  "Execute jj absorb with ARGS from the transient."
  :description "Absorb"
  :class 'majutsu-transient-default-action-suffix
  (interactive (list (majutsu-absorb-arguments)))
  (pcase-let ((`(,args ,filesets) (majutsu-filesets-split-transient-value args)))
    (if (majutsu-diff-editor-interactive-arguments-p args)
        (majutsu-diff-editor-start
         "absorb" args filesets :origin-buffer (current-buffer))
      (if-let* ((plan (majutsu-interactive-build-replay-plan-if-selected
                      nil nil 'majutsu-absorb)))
          (let ((source (majutsu-diff-single-source-commit)))
            (majutsu-absorb--check-patch-source args source)
            (majutsu-interactive-run-replay-plan
             "absorb"
             (majutsu-jj-set-option-value args "--from" source "-f")
             filesets plan))
        (when (zerop (apply #'majutsu-run-jj
                           "absorb"
                           (majutsu-jj-append-filesets args filesets)))
          (message "Absorb completed"))))))

;;; Infix Commands

(transient-define-argument majutsu-absorb:--from ()
  :description "From"
  :class 'majutsu-revision-selection-option
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :selection-toggle-key "f"
  :selection-toggle-if-not #'majutsu-diff-single-source-commit
  :shortarg "-f"
  :argument "--from="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-absorb:--into ()
  :description "Into"
  :class 'majutsu-revision-selection-option
  :selection-label "[INTO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :selection-toggle-key "t"
  :selection-toggle-if-not #'majutsu-diff-single-source-commit
  :shortarg "-t"
  :argument "--into="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

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
  :description "JJ Absorb"
  :class 'majutsu-jj-transient-prefix
  :jj-command "absorb"
  :transient-non-suffix t
  [["Selection"
    (majutsu-absorb:--from)
    (majutsu-absorb:--into)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Patch Selection" :if majutsu-diff-single-source-commit
    (majutsu-interactive:select-hunk)
    (majutsu-interactive:select-file)
    (majutsu-interactive:select-region)
    ("C" "Clear patch selections" majutsu-interactive-clear :transient t)]
   ["Options"
    (majutsu-absorb:--)
    ("-i" "Interactive" ("-i" "--interactive"))
    ("=t" "Tool" "--tool=")
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("a" "Absorb" majutsu-absorb-execute)]]
  (interactive)
  (let* (;; A patch selection may span several files.  Do not silently limit
         ;; it to the file where the transient was opened.
         (file (and (not (majutsu-diff-single-source-commit))
                    (majutsu-file-at-point)))
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
