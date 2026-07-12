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
(require 'majutsu-diff-editor)

(defclass majutsu-split-option (majutsu-selection-option)
  ())

(defvar-local majutsu-split--patch-source-cache-key :uncomputed
  "Diff range and rendered-text tick for the cached split source.")

(defvar-local majutsu-split--patch-source-cache-value nil
  "Cached canonical commit ID for the rendered split source.")

(defun majutsu-split--diff-source-revset ()
  "Return the safe single-source revset described by the current diff.
The default diff range denotes @.  Explicit ranges must use exactly one
-r/--revisions option; arbitrary from/to ranges are never split sources."
  (let* ((range majutsu-buffer-diff-range)
         (from (majutsu-jj-option-values range "--from" "-f"))
         (to (majutsu-jj-option-values range "--to" "-t"))
         (revisions (majutsu-jj-option-values range "--revisions" "-r")))
    (cond
     ((or from to) nil)
     ((null range) "@")
     ((= (length revisions) 1) (car revisions)))))

(defun majutsu-split--patch-source-commit (&optional buffer)
  "Return BUFFER's canonical commit ID when it is safe for patch split."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'majutsu-diff-mode)
      (let* ((range (copy-tree majutsu-buffer-diff-range))
             (cache-key (list range (buffer-chars-modified-tick))))
        (unless (equal cache-key majutsu-split--patch-source-cache-key)
          (setq-local majutsu-split--patch-source-cache-key cache-key)
          (setq-local majutsu-split--patch-source-cache-value
                      (when-let* ((revset (majutsu-split--diff-source-revset)))
                        (majutsu-jj-resolve-single-commit revset))))
        majutsu-split--patch-source-cache-value))))

(defun majutsu-split-interactive-selection-available-p ()
  "Return non-nil when split patch selection is safe in the current diff."
  (and (majutsu-interactive-selection-available-p)
       (majutsu-split--patch-source-commit)))

(defun majutsu-split--check-patch-source (args patch-source)
  "Signal when ARGS would split a commit other than PATCH-SOURCE."
  (unless patch-source
    (user-error "Patch selection for split requires a diff for exactly one commit"))
  (let ((revisions (majutsu-jj-option-values args "--revision" "-r")))
    (unless (<= (length revisions) 1)
      (user-error "Patch selection for split requires exactly one revision"))
    (let ((source (majutsu-jj-resolve-single-commit
                   (if revisions (car revisions) "@"))))
      (unless (and source (equal source patch-source))
        (user-error "Patch selection for split requires the rendered diff source")))))

(defun majutsu-split--default-args ()
  "Return a resolved single-revision default from the diff buffer context."
  (when (derived-mode-p 'majutsu-diff-mode)
    (when-let* ((range majutsu-buffer-diff-range)
                (commit (majutsu-split--patch-source-commit)))
      (list (concat "--revision=" commit)))))

(transient-define-suffix majutsu-split-execute (args)
  "Execute split with selections recorded in the transient."
  :description "Execute split"
  :class 'majutsu-transient-default-action-suffix
  (interactive (list (transient-args 'majutsu-split)))
  (pcase-let ((`(,args ,filesets) (majutsu-filesets-split-transient-value args)))
    ;; An explicit jj editor request takes precedence over an Emacs-owned
    ;; patch selection.  Do not inspect its owner here: jj does not consume
    ;; the selection, so it remains available for the originating command.
    (if (majutsu-diff-editor-interactive-arguments-p args)
        (majutsu-diff-editor-start
         "split" args filesets :origin-buffer (current-buffer))
      (let* (;; Text hunks and hunkless files coexist in one operation.
             (plan (majutsu-interactive-build-replay-plan-if-selected
                    nil nil 'majutsu-split))
             (patch-source (and plan
                                (majutsu-split--patch-source-commit))))
        (when plan
          (majutsu-split--check-patch-source args patch-source)
          ;; Execute against the immutable commit which was just validated;
          ;; a dynamic @/bookmark must not move between the guard and jj.
          (setq args (majutsu-jj-set-option-value
                      args "--revision" patch-source "-r")))
        (cond
         (plan
          ;; Reset to the left tree, then replay precisely the selections.
          (majutsu-interactive-run-replay-plan
           "split"
           (majutsu-diff-editor-strip-interactive-arguments args)
           filesets plan))
         ;; `jj split' selects interactively by default when no fileset is given.
         ((null filesets)
         (majutsu-diff-editor-start
           "split" args filesets :origin-buffer (current-buffer)))
         (t
          (majutsu-run-jj-with-editor
           (cons "split" (majutsu-jj-append-filesets args filesets)))))))))

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
    ("=t" "Tool" "--tool=")
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
