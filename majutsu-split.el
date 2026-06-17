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

(defclass majutsu-split-option (majutsu-selection-option)
  ())

(defun majutsu-split--default-args ()
  "Return default args from diff buffer context."
  (when (derived-mode-p 'majutsu-diff-mode)
    (mapcar (lambda (arg)
              (if-let* ((rev (transient-arg-value "--revisions=" (list arg))))
                  (concat "--revision=" rev)
                arg))
            majutsu-buffer-diff-range)))

(transient-define-suffix majutsu-split-execute (args)
  "Execute split with selections recorded in the transient."
  :description "Execute split"
  :class 'majutsu-transient-default-action-suffix
  (interactive (list (transient-args 'majutsu-split)))
  (pcase-let* ((`(,args ,filesets) (majutsu-filesets-split-transient-value args))
               ;; Generate patch for SELECTED content (invert=nil)
               ;; This is what goes into the first commit
               (patch (majutsu-interactive-build-patch-if-selected nil nil nil))
               (args (if patch
                         (seq-remove (lambda (arg)
                                       (or (string= arg "--interactive")
                                           (transient-arg-value "--tool=" (list arg))))
                                     args)
                       args)))
    (if patch
        (progn
          ;; reverse=t means reset $right to $left, then apply patch forward
          ;; Result: $right = selected content = first commit
          (majutsu-interactive-run-with-patch "split" args filesets patch t)
          (majutsu-interactive-clear))
      (majutsu-run-jj-with-editor
       (cons "split" (majutsu-jj-append-filesets args filesets))))))

;;;; Infix Commands

(transient-define-argument majutsu-split:--revision ()
  :description "Revision"
  :class 'majutsu-split-option
  :selection-label "[REV]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :selection-toggle-key "r"
  :selection-toggle-if-not #'majutsu-interactive-selection-available-p
  :shortarg "-r"
  :argument "--revision="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-split:--onto ()
  :description "Onto"
  :class 'majutsu-split-option
  :selection-label "[ONTO]"
  :selection-face '(:background "dark green" :foreground "white")
  :selection-toggle-key "o"
  :selection-toggle-if-not #'majutsu-interactive-selection-available-p
  :shortarg "-o"
  :argument "--onto="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-split:--insert-after ()
  :description "Insert after"
  :class 'majutsu-split-option
  :selection-label "[AFTER]"
  :selection-face '(:background "dark blue" :foreground "white")
  :selection-toggle-key "a"
  :selection-toggle-if-not #'majutsu-interactive-selection-available-p
  :shortarg "-A"
  :argument "--insert-after="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-split:--insert-before ()
  :description "Insert before"
  :class 'majutsu-split-option
  :selection-label "[BEFORE]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-toggle-key "b"
  :selection-toggle-if-not #'majutsu-interactive-selection-available-p
  :shortarg "-B"
  :argument "--insert-before="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-split:--message ()
  :description "Message"
  :shortarg "-m"
  :argument "--message="
  :reader #'read-string)

(transient-define-argument majutsu-split:-- ()
  :description "Limit to files"
  :class 'transient-files
  :key "--"
  :argument "--"
  :prompt "Limit to file,s: "
  :reader #'majutsu-read-files
  :multi-value t)

;;;; Prefix

;;;###autoload(autoload 'majutsu-split "majutsu-split" nil t)
(transient-define-prefix majutsu-split ()
  "Transient for jj split operations."
  :man-page "jj-split"
  :description "JJ Split"
  :class 'majutsu-jj-transient-prefix
  :jj-command "split"
  :transient-non-suffix t
  [["Selection"
    (majutsu-split:--revision)
    (majutsu-split:--onto)
    (majutsu-split:--insert-after)
    (majutsu-split:--insert-before)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Patch Selection" :if majutsu-interactive-selection-available-p
    (majutsu-interactive:select-hunk)
    (majutsu-interactive:select-file)
    (majutsu-interactive:select-region)
    ("C" "Clear patch selections" majutsu-interactive-clear :transient t)]
   ["Paths" :if-not majutsu-interactive-selection-available-p
    (majutsu-split:--)]
   ["Options"
    ("-i" "Interactive" ("-i" "--interactive"))
    ("-p" "Parallel" ("-p" "--parallel"))
    ("-e" "Editor" "--editor")
    ("-t" "Tool" "--tool=")
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("s" "Execute split" majutsu-split-execute)]]
  (interactive)
  (transient-setup
   'majutsu-split nil nil
   :scope
   (majutsu-selection-session-begin)
   :value (or (majutsu-split--default-args) '())))

;;; _
(provide 'majutsu-split)
;;; majutsu-split.el ends here
