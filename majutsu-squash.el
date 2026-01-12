;;; majutsu-squash.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides jj squash transients, managing from and into
;; selections and assembling flags.

;;; Code:

(require 'majutsu)
(require 'majutsu-selection)
(require 'majutsu-interactive)

(defclass majutsu-squash-option (majutsu-selection-option)
  ())

(defclass majutsu-squash--toggle-option (majutsu-selection-toggle-option)
  ())

;;; majutsu-squash

(defun majutsu-squash-arguments ()
  "Return the current squash arguments.
If inside the transient, return transient args.
Otherwise, if no --revision/--from/--into is set and point is on
a jj-commit section, add --revision from that section."
  (let ((args (if (eq transient-current-command 'majutsu-squash)
                  (transient-args 'majutsu-squash)
                '())))
    (unless (cl-some (lambda (arg)
                       (or (string-prefix-p "--revision=" arg)
                           (string-prefix-p "--from=" arg)
                           (string-prefix-p "--into=" arg)))
                     args)
      (when-let* ((rev (magit-section-value-if 'jj-commit)))
        (push (concat "--revision=" rev) args)))
    args))

(defun majutsu-squash--default-args ()
  "Return default args from diff buffer context."
  (with-current-buffer (majutsu-interactive--selection-buffer)
    (when (derived-mode-p 'majutsu-diff-mode)
      (mapcar (##if (string-prefix-p "--revisions=" %) (concat "--revision=" (substring % 12)))
              majutsu-buffer-diff-range))))

(defun majutsu-squash-execute (args)
  "Execute squash with selections recorded in the transient."
  (interactive (list (majutsu-squash-arguments)))
  (let* ((selection-buf (majutsu-interactive--selection-buffer))
         ;; Generate patch for SELECTED content (invert=nil)
         ;; This is what gets squashed into parent
         (patch (majutsu-interactive-build-patch-if-selected selection-buf nil nil)))
    (if patch
        (progn
          ;; reverse=t means reset $right to $left, then apply patch forward
          ;; Result: $right = selected content = what gets squashed
          (majutsu-interactive-run-with-patch "squash" args patch t)
          (with-current-buffer selection-buf
            (majutsu-interactive-clear)))
      (majutsu-run-jj-with-editor (cons "squash" args)))))

;;;; Infix Commands

(transient-define-argument majutsu-squash:--revision ()
  :description "Revision"
  :class 'majutsu-squash-option
  :selection-key 'revision
  :selection-label "[REV]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :selection-type 'single
  :key "-r"
  :argument "--revision="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-squash:--from ()
  :description "From"
  :class 'majutsu-squash-option
  :selection-key 'from
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :selection-type 'multi
  :key "-f"
  :argument "--from="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-squash:--into ()
  :description "Into"
  :class 'majutsu-squash-option
  :selection-key 'into
  :selection-label "[INTO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :selection-type 'single
  :key "-t"
  :argument "--into="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-squash:--onto ()
  :description "Onto"
  :class 'majutsu-squash-option
  :selection-key 'onto
  :selection-label "[ONTO]"
  :selection-face '(:background "dark green" :foreground "white")
  :selection-type 'multi
  :key "-o"
  :argument "--onto="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-squash:--insert-after ()
  :description "Insert after"
  :class 'majutsu-squash-option
  :selection-key 'after
  :selection-label "[AFTER]"
  :selection-face '(:background "dark blue" :foreground "white")
  :selection-type 'multi
  :key "-A"
  :argument "--insert-after="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-squash:--insert-before ()
  :description "Insert before"
  :class 'majutsu-squash-option
  :selection-key 'before
  :selection-label "[BEFORE]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-type 'multi
  :key "-B"
  :argument "--insert-before="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-squash:revision ()
  :description "Revision (toggle at point)"
  :class 'majutsu-squash--toggle-option
  :selection-key 'revision
  :selection-type 'single
  :key "r"
  :argument "--revision=")

(transient-define-argument majutsu-squash:from ()
  :description "From (toggle at point)"
  :class 'majutsu-squash--toggle-option
  :selection-key 'from
  :selection-type 'multi
  :key "f"
  :argument "--from="
  :multi-value 'repeat)

(transient-define-argument majutsu-squash:into ()
  :description "Into (toggle at point)"
  :class 'majutsu-squash--toggle-option
  :selection-key 'into
  :selection-type 'single
  :key "t"
  :argument "--into=")

(transient-define-argument majutsu-squash:onto ()
  :description "Onto (toggle at point)"
  :class 'majutsu-squash--toggle-option
  :selection-key 'onto
  :selection-type 'multi
  :key "o"
  :argument "--onto="
  :multi-value 'repeat)

(transient-define-argument majutsu-squash:insert-after ()
  :description "Insert after (toggle at point)"
  :class 'majutsu-squash--toggle-option
  :selection-key 'after
  :selection-type 'multi
  :key "a"
  :argument "--insert-after="
  :multi-value 'repeat)

(transient-define-argument majutsu-squash:insert-before ()
  :description "Insert before (toggle at point)"
  :class 'majutsu-squash--toggle-option
  :selection-key 'before
  :selection-type 'multi
  :key "b"
  :argument "--insert-before="
  :multi-value 'repeat)

(defun majutsu-squash-clear-selections ()
  "Clear all squash selections."
  (interactive)
  (when (consp transient--suffixes)
    (dolist (obj transient--suffixes)
      (when (and (cl-typep obj 'majutsu-squash-option)
                 (memq (oref obj selection-key) '(revision from into onto after before)))
        (transient-infix-set obj nil))))
  (when transient--prefix
    (transient--redisplay))
  (majutsu-selection-render)
  (message "Cleared all squash selections"))

;;;; Prefix

(transient-define-prefix majutsu-squash ()
  "Internal transient for jj squash operations."
  :man-page "jj-squash"
  :transient-non-suffix t
  :incompatible '(("--revision=" "--onto=")
                  ("--revision=" "--insert-after=")
                  ("--revision=" "--insert-before="))
  [
   :description "JJ Squash"
   ["Selection"
    (majutsu-squash:--revision)
    (majutsu-squash:--from)
    (majutsu-squash:--into)
    (majutsu-squash:--onto)
    (majutsu-squash:--insert-after)
    (majutsu-squash:--insert-before)
    (majutsu-squash:from)
    (majutsu-squash:into)
    (majutsu-squash:onto)
    (majutsu-squash:insert-after)
    (majutsu-squash:insert-before)
    ("c" "Clear selections" majutsu-squash-clear-selections :transient t)]
   ["Patch Selection" :if majutsu-interactive-selection-available-p
    (majutsu-interactive:select-hunk)
    (majutsu-interactive:select-file)
    (majutsu-interactive:select-region)
    ("C" "Clear patch selections" majutsu-interactive-clear :transient t)]
   ["Options"
    ("-k" "Keep emptied commit" "-k")
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("s" "Execute squash" majutsu-squash-execute)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (transient-setup
   'majutsu-squash nil nil
   :scope
   (majutsu-selection-session-begin)
   :value (or (majutsu-squash--default-args) '())))

;;; _
(provide 'majutsu-squash)
;;; majutsu-squash.el ends here
