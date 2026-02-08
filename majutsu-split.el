;;; majutsu-split.el --- Split transient for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides jj split transients, managing revision selection
;; and interactive hunk/region patch selection for diff buffers.

;;; Code:

(require 'majutsu)
(require 'majutsu-file)
(require 'majutsu-selection)
(require 'majutsu-interactive)

(defclass majutsu-split-option (majutsu-selection-option)
  ())

(defclass majutsu-split--toggle-option (majutsu-selection-toggle-option)
  ())

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
         (args (if patch
                   (seq-remove (lambda (arg)
                                 (or (string= arg "--interactive")
                                     (string-prefix-p "--tool=" arg)))
                               args)
                 args)))
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
  :selection-label "[REV]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :key "-r"
  :argument "--revision="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-split:--onto ()
  :description "Onto"
  :class 'majutsu-split-option
  :selection-label "[ONTO]"
  :selection-face '(:background "dark green" :foreground "white")
  :key "-o"
  :argument "--onto="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-split:--insert-after ()
  :description "Insert after"
  :class 'majutsu-split-option
  :selection-label "[AFTER]"
  :selection-face '(:background "dark blue" :foreground "white")
  :key "-A"
  :argument "--insert-after="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-split:--insert-before ()
  :description "Insert before"
  :class 'majutsu-split-option
  :selection-label "[BEFORE]"
  :selection-face '(:background "dark magenta" :foreground "white")
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
  :key "r"
  :argument "--revision=")

(transient-define-argument majutsu-split:onto ()
  :description "Onto (toggle at point)"
  :class 'majutsu-split--toggle-option
  :key "o"
  :argument "--onto="
  :multi-value 'repeat)

(transient-define-argument majutsu-split:insert-after ()
  :description "Insert after (toggle at point)"
  :class 'majutsu-split--toggle-option
  :key "a"
  :argument "--insert-after="
  :multi-value 'repeat)

(transient-define-argument majutsu-split:insert-before ()
  :description "Insert before (toggle at point)"
  :class 'majutsu-split--toggle-option
  :key "b"
  :argument "--insert-before="
  :multi-value 'repeat)

(transient-define-argument majutsu-split:-- ()
  :description "Limit to files"
  :class 'transient-files
  :key "--"
  :argument "--"
  :prompt "Limit to file,s: "
  :reader #'majutsu-read-files
  :multi-value t)

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
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Patch Selection" :if majutsu-interactive-selection-available-p
    (majutsu-interactive:select-hunk)
    (majutsu-interactive:select-file)
    (majutsu-interactive:select-region)
    ("C" "Clear patch selections" majutsu-interactive-clear :transient t)]
   ["Paths" :if-not majutsu-interactive-selection-available-p
    (majutsu-split:--)]
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
