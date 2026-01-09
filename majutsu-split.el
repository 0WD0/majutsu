;;; majutsu-split.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides jj split transients, managing revision selection
;; and interactive hunk/region patch selection for diff buffers.

;;; Code:

(require 'majutsu)
(require 'majutsu-selection)
(require 'majutsu-interactive)

(defclass majutsu-split-option (majutsu-selection-option)
  ((selection-key :initarg :selection-key :initform nil)))

(defclass majutsu-split--toggle-option (majutsu-selection-toggle-option)
  ())

(defvar-local majutsu-split--filesets nil
  "Filesets for the current split operation.")

(defun majutsu-split--default-args ()
  "Return default args from diff buffer context."
  (with-current-buffer (majutsu-interactive--selection-buffer)
    (when (derived-mode-p 'majutsu-diff-mode)
      (mapcar (##if (string-prefix-p "--revisions=" %)
                    (concat "--revision=" (substring % 12))
                    %)
              majutsu-buffer-diff-range))))

(defun majutsu-split-execute (args)
  "Execute split with selections recorded in the transient."
  (interactive (list (transient-args 'majutsu-split)))
  (let* ((selection-buf (majutsu-interactive--selection-buffer))
         ;; Generate patch for SELECTED content (invert=nil)
         ;; This is what goes into the first commit
         (patch (majutsu-interactive-build-patch-if-selected selection-buf nil nil))
         (filesets majutsu-split--filesets)
         (args (if patch
                   (seq-remove (lambda (arg)
                                 (or (string= arg "--interactive")
                                     (string-prefix-p "--tool=" arg)))
                               args)
                 (append args filesets))))
    (if patch
        (progn
          ;; reverse=t means reset $right to $left, then apply patch forward
          ;; Result: $right = selected content = first commit
          (majutsu-interactive-run-with-patch "split" args patch t)
          (with-current-buffer selection-buf
            (majutsu-interactive-clear)))
      (majutsu-run-jj-with-editor (cons "split" args)))))

;;;; Infix Commands

(transient-define-argument majutsu-split:--revision ()
  :description "Revision"
  :class 'majutsu-split-option
  :selection-key 'revision
  :selection-label "[REV]"
  :selection-face '(:background "dark orange" :foreground "black")
  :selection-type 'single
  :key "-r"
  :argument "--revision="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-split:--onto ()
  :description "Onto"
  :class 'majutsu-split-option
  :selection-key 'onto
  :selection-label "[ONTO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :selection-type 'multi
  :key "-o"
  :argument "--onto="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-split:--insert-after ()
  :description "Insert after"
  :class 'majutsu-split-option
  :selection-key 'after
  :selection-label "[AFTER]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-type 'multi
  :key "-A"
  :argument "--insert-after="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-split:--insert-before ()
  :description "Insert before"
  :class 'majutsu-split-option
  :selection-key 'before
  :selection-label "[BEFORE]"
  :selection-face '(:background "dark sea green" :foreground "black")
  :selection-type 'multi
  :key "-B"
  :argument "--insert-before="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-split:--message ()
  :description "Message"
  :key "-m"
  :argument "--message="
  :reader #'read-string)

(transient-define-argument majutsu-split:revision ()
  :description "Revision (toggle at point)"
  :class 'majutsu-split--toggle-option
  :selection-key 'revision
  :selection-type 'single
  :key "r"
  :argument "--revision=")

(transient-define-argument majutsu-split:onto ()
  :description "Onto (toggle at point)"
  :class 'majutsu-split--toggle-option
  :selection-key 'onto
  :selection-type 'multi
  :key "o"
  :argument "--onto="
  :multi-value 'repeat)

(transient-define-argument majutsu-split:insert-after ()
  :description "Insert after (toggle at point)"
  :class 'majutsu-split--toggle-option
  :selection-key 'after
  :selection-type 'multi
  :key "a"
  :argument "--insert-after="
  :multi-value 'repeat)

(transient-define-argument majutsu-split:insert-before ()
  :description "Insert before (toggle at point)"
  :class 'majutsu-split--toggle-option
  :selection-key 'before
  :selection-type 'multi
  :key "b"
  :argument "--insert-before="
  :multi-value 'repeat)

(defun majutsu-split-clear-selections ()
  "Clear all split selections."
  (interactive)
  (when (consp transient--suffixes)
    (dolist (obj transient--suffixes)
      (when (and (cl-typep obj 'majutsu-split-option)
                 (memq (oref obj selection-key) '(revision onto after before)))
        (transient-infix-set obj nil))))
  (setq majutsu-split--filesets nil)
  (when transient--prefix
    (transient--redisplay))
  (majutsu-selection-render)
  (message "Cleared all split selections"))

(defun majutsu-split--read-filesets (prompt &optional initial)
  "Read filesets with PROMPT and INITIAL value."
  (let ((input (read-string prompt (or initial ""))))
    (if (string-empty-p input)
        nil
      (split-string input))))

(defun majutsu-split-set-filesets ()
  "Set filesets for split."
  (interactive)
  (let* ((current (string-join (or majutsu-split--filesets '()) " "))
         (new (majutsu-split--read-filesets "Filesets (space-separated): " current)))
    (setq majutsu-split--filesets new)
    (when transient--prefix
      (transient--redisplay))
    (message "Filesets: %s" (or (string-join new " ") "(all)"))))

(defun majutsu-split--filesets-description ()
  "Return description for filesets display."
  (if majutsu-split--filesets
      (format "Paths: %s" (string-join majutsu-split--filesets " "))
    "Paths: (all)"))

;;;; Prefix

(transient-define-prefix majutsu-split ()
  "Transient for jj split operations."
  :man-page "jj-split"
  :transient-non-suffix t
  [
   :description "JJ Split"
   ["Selection"
    (majutsu-split:--revision)
    (majutsu-split:--onto)
    (majutsu-split:--insert-after)
    (majutsu-split:--insert-before)
    (majutsu-split:revision)
    (majutsu-split:onto)
    (majutsu-split:insert-after)
    (majutsu-split:insert-before)
    ("c" "Clear selections" majutsu-split-clear-selections :transient t)]
   ["Patch Selection" :if majutsu-interactive-selection-available-p
    (majutsu-interactive:select-hunk)
    (majutsu-interactive:select-file)
    (majutsu-interactive:select-region)
    ("C" "Clear patch selections" majutsu-interactive-clear :transient t)]
   ["Paths" :if-not majutsu-interactive-selection-available-p
    ("p" majutsu-split--filesets-description majutsu-split-set-filesets :transient t)]
   ["Options"
    ("-i" "Interactive" "--interactive")
    ("-p" "Parallel" "--parallel")
    ("-e" "Editor" "--editor")
    ("-t" "Tool" "--tool=")
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("s" "Execute split" majutsu-split-execute)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (transient-setup
   'majutsu-split nil nil
   :scope
   (majutsu-selection-session-begin)
   :value (or (majutsu-split--default-args) '())))

;;; _
(provide 'majutsu-split)
;;; majutsu-split.el ends here
