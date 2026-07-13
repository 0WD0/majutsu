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

(defun majutsu-split--diff-source-revision (&optional buffer)
  "Return the single Split source represented by diff BUFFER.
Return the resolved change ID only when the displayed diff represents exactly
one change."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'majutsu-diff-mode)
      (plist-get (majutsu-diff--revision-metadata) :change-id))))

(defun majutsu-split--default-args ()
  "Return a safe Split default from the current diff context."
  (when-let* ((revision (majutsu-split--diff-source-revision)))
    (list (concat "--revision=" revision))))

(defun majutsu-split-interactive-selection-available-p ()
  "Return non-nil when the current diff can safely drive a patch Split."
  (and (majutsu-interactive-selection-available-p)
       (majutsu-split--diff-source-revision)))

(defun majutsu-split--check-patch-source (args patch-source)
  "Signal if ARGS select a revision incompatible with PATCH-SOURCE."
  (unless patch-source
    (user-error "Patch selection for split requires a single-revision diff"))
  (when-let* ((revision (transient-arg-value "--revision=" args)))
    (unless (equal revision patch-source)
      (user-error "Patch selection for split requires the diff source"))))

(defun majutsu-split--remove-interactive-tool-args (args)
  "Return ARGS without native interactive-editor or tool arguments."
  (let (result)
    (while args
      (let ((arg (pop args)))
        (cond
         ((member arg '("-i" "--interactive")) nil)
         ((member arg '("-t" "--tool")) (pop args))
         ((or (string-prefix-p "--tool=" arg)
              (string-prefix-p "-t=" arg)) nil)
         (t (push arg result)))))
    (nreverse result)))

(transient-define-suffix majutsu-split-execute (args)
  "Execute split with selections recorded in the transient."
  :description "Execute split"
  :class 'majutsu-transient-default-action-suffix
  (interactive (list (transient-args 'majutsu-split)))
  (pcase-let* ((`(,args ,filesets) (majutsu-filesets-split-transient-value args))
               ;; Text hunks and hunkless files coexist in one operation.
               (operation (majutsu-interactive-build-operation-if-selected
                           nil nil nil nil))
               (patch (plist-get operation :patch))
               (file-ops (plist-get operation :file-ops))
               (patch-source
                (and operation (majutsu-split--diff-source-revision)))
               (args (if operation
                         (majutsu-split--remove-interactive-tool-args args)
                       args)))
    (if operation
        (progn
          (majutsu-split--check-patch-source args patch-source)
          ;; Reset to the left tree, then replay precisely the selections.
          (majutsu-interactive-run-with-patch
           "split" args filesets patch t file-ops)
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
  :selection-toggle-if-not #'majutsu-split-interactive-selection-available-p
  :shortarg "-r"
  :argument "--revision="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-split:--onto ()
  :description "Onto"
  :class 'majutsu-split-option
  :selection-label "[ONTO]"
  :selection-face '(:background "dark green" :foreground "white")
  :selection-toggle-key "o"
  :selection-toggle-if-not #'majutsu-split-interactive-selection-available-p
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
  :selection-toggle-if-not #'majutsu-split-interactive-selection-available-p
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
  :selection-toggle-if-not #'majutsu-split-interactive-selection-available-p
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
   ["Patch Selection" :if majutsu-split-interactive-selection-available-p
    (majutsu-interactive:select-hunk)
    (majutsu-interactive:select-file)
    (majutsu-interactive:select-region)
    ("C" "Clear patch selections" majutsu-interactive-clear :transient t)]
   ["Paths" :if-not majutsu-split-interactive-selection-available-p
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
