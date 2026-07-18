;;; majutsu-squash.el --- Squash transient for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides jj squash transients, managing source and destination
;; selections and assembling flags.  Majutsu uses --from as the canonical source
;; selection and adds an explicit --into default when no destination is selected.

;;; Code:

(require 'majutsu)

(declare-function majutsu-read-optional-revset "majutsu-jj" (prompt &optional default initial-input history completion-args))
(defvar majutsu-buffer-diff-range)

(defclass majutsu-squash-option (majutsu-selection-option)
  ())

;;; Arguments

(defun majutsu-squash--source-values (args)
  "Return --from values in ARGS."
  (seq-keep (lambda (arg) (transient-arg-value "--from=" (list arg)))
            args))

(defun majutsu-squash--remove-interactive-tool-args (args)
  "Remove native interactive/tool arguments from ARGS."
  (let (out)
    (while args
      (let ((arg (pop args)))
        (cond
         ((member arg '("-i" "--interactive")))
         ((transient-arg-value "--tool=" (list arg)))
         ((equal arg "--tool")
          (when args (pop args)))
         (t
          (push arg out)))))
    (nreverse out)))

(defun majutsu-squash--source-revset (sources)
  "Return a revset union expression for SOURCES."
  (mapconcat (lambda (source) (format "(%s)" source)) sources " | "))

(defun majutsu-squash--point-revision ()
  "Return the jj commit revision at point."
  (magit-section-value-if 'jj-commit))

(defun majutsu-squash--none-source-p (sources)
  "Return non-nil when SOURCES is the literal empty source none()."
  (and (= (length sources) 1)
       (equal (string-trim (car sources)) "none()")))

(defun majutsu-squash--destination-revset (source-revset explicit-source)
  "Return inferred destination revset for SOURCE-REVSET.
If EXPLICIT-SOURCE is non-nil and point is on a commit outside SOURCE-REVSET,
prefer point as the destination.  Otherwise use SOURCE-REVSET's external parent.
The resulting revset is intentionally left for jj to resolve."
  (let ((parent (format "parents(roots(%s))" source-revset)))
    (if-let* ((point (and explicit-source (majutsu-squash--point-revision))))
        (format "coalesce((%s) ~ (%s), %s)" point source-revset parent)
      parent)))

;;; Defaults

(defun majutsu-squash--diff-default-args ()
  "Return default squash args from a diff buffer context."
  (let* ((range majutsu-buffer-diff-range)
         (from (transient-arg-value "--from=" range))
         (to (transient-arg-value "--to=" range))
         (revisions (seq-keep (lambda (arg)
                                (transient-arg-value "--revisions=" (list arg)))
                              range)))
    (cond
     ;; Arbitrary --from/--to diff buffers do not describe a squash source.
     ((or from to) nil)
     (revisions
      (mapcar (lambda (rev) (concat "--from=" rev)) revisions)))))

(defun majutsu-squash--log-default-args ()
  "Return default squash args from log point or region."
  (or (when-let* ((revsets (magit-region-values 'jj-commit t)))
        (mapcar (lambda (rev) (concat "--from=" rev)) revsets))
      (when-let* ((rev (magit-section-value-if 'jj-commit)))
        (list (concat "--from=" rev)))))

(defun majutsu-squash--default-args ()
  "Return source defaults from the current diff/log context."
  (if (derived-mode-p 'majutsu-diff-mode)
      (majutsu-squash--diff-default-args)
    (majutsu-squash--log-default-args)))

(defun majutsu-squash-arguments ()
  "Return the current squash arguments.
Inside the transient, return transient args unchanged.  Outside the transient,
return the same context defaults that execution would use."
  (if (eq transient-current-command 'majutsu-squash)
      (transient-args 'majutsu-squash)
    (or (majutsu-squash--default-args) '())))

;;; Patch selection safety

(defun majutsu-squash--patch-source-revset (&optional buffer)
  "Return a source revset when BUFFER is a safe squash patch-selection buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'majutsu-diff-mode)
      (let* ((range majutsu-buffer-diff-range)
             (from (transient-arg-value "--from=" range))
             (to (transient-arg-value "--to=" range))
             (revisions (seq-keep (lambda (arg)
                                    (transient-arg-value "--revisions=" (list arg)))
                                  range)))
        (cond
         ((or from to) nil)
         ((null range) "@")
         ((= (length revisions) 1) (car revisions)))))))

(defun majutsu-squash-interactive-selection-available-p ()
  "Return non-nil when squash patch selection is available."
  (and (majutsu-interactive-selection-available-p)
       (majutsu-squash--patch-source-revset)))

(defun majutsu-squash--check-patch-source (args patch-source)
  "Signal if ARGS select a source incompatible with PATCH-SOURCE."
  (let ((sources (majutsu-squash--source-values args)))
    (unless patch-source
      (user-error "Patch selection for squash requires a revision diff"))
    (unless (or (null sources)
                (and (= (length sources) 1)
                     (equal (car sources) patch-source)))
      (user-error "Patch selection for squash requires the diff source"))))

;;; Execution

(transient-define-suffix majutsu-squash-execute (args)
  "Execute squash with selections recorded in the transient."
  :description "Execute squash"
  :class 'majutsu-transient-default-action-suffix
  (interactive (list (majutsu-squash-arguments)))
  (pcase-let* ((`(,args ,filesets) (majutsu-filesets-split-transient-value args))
               ;; Text hunks and whole-file changes both move into the destination.
               (plan (majutsu-interactive-build-replay-plan-if-selected))
               (patch-source (and plan (majutsu-squash--patch-source-revset))))
    (when plan
      (majutsu-squash--check-patch-source args patch-source)
      (setq args (majutsu-squash--remove-interactive-tool-args args)))
    (let* ((explicit-sources (majutsu-squash--source-values args))
           (explicit-destination
            (seq-some (lambda (arg) (transient-arg-value arg args))
                      '("--into=" "--to=" "--onto=" "--destination="
                        "--insert-after=" "--after="
                        "--insert-before=" "--before=")))
           (source-default (if explicit-destination
                               '("--from=@")
                             (or (majutsu-squash--default-args) '("--from=@")))))
      (unless explicit-sources
        (setq args (append args source-default)))
      (let ((sources (majutsu-squash--source-values args)))
        (unless (or explicit-destination
                    (majutsu-squash--none-source-p sources))
          (setq args (append
                      args
                      (list (concat
                             "--into="
                             (majutsu-squash--destination-revset
                              (majutsu-squash--source-revset sources)
                              explicit-sources))))))))
    (if plan
        (progn
          ;; The selected plan rebuilds right from left, then applies forward.
          (majutsu-interactive-run-replay-plan "squash" args filesets plan)
          (majutsu-interactive-clear))
      (majutsu-run-jj-with-editor
       (cons "squash" (majutsu-jj-append-filesets args filesets))))))

;;;; Infix Commands

(transient-define-argument majutsu-squash:--from ()
  :description "Source revisions"
  :class 'majutsu-squash-option
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :selection-toggle-key "f"
  :selection-toggle-if-not #'majutsu-squash-interactive-selection-available-p
  :shortarg "-f"
  :argument "--from="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-squash:--into ()
  :description "Into"
  :class 'majutsu-squash-option
  :selection-label "[INTO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :selection-toggle-key "t"
  :selection-toggle-if-not #'majutsu-squash-interactive-selection-available-p
  :shortarg "-t"
  :argument "--into="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-squash:--onto ()
  :description "Onto"
  :class 'majutsu-squash-option
  :selection-label "[ONTO]"
  :selection-face '(:background "dark green" :foreground "white")
  :selection-toggle-key "o"
  :selection-toggle-if-not #'majutsu-squash-interactive-selection-available-p
  :shortarg "-o"
  :argument "--onto="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-squash:--insert-after ()
  :description "Insert after"
  :class 'majutsu-squash-option
  :selection-label "[AFTER]"
  :selection-face '(:background "dark blue" :foreground "white")
  :selection-toggle-key "a"
  :selection-toggle-if-not #'majutsu-squash-interactive-selection-available-p
  :shortarg "-A"
  :argument "--insert-after="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-squash:--insert-before ()
  :description "Insert before"
  :class 'majutsu-squash-option
  :selection-label "[BEFORE]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-toggle-key "b"
  :selection-toggle-if-not #'majutsu-squash-interactive-selection-available-p
  :shortarg "-B"
  :argument "--insert-before="
  :multi-value 'repeat
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-squash:-- ()
  :description "Limit to files"
  :class 'transient-files
  :key "--"
  :argument "--"
  :prompt "Limit to file,s: "
  :reader #'majutsu-read-files
  :multi-value t)

;;;; Prefix

;;;###autoload(autoload 'majutsu-squash "majutsu-squash" nil t)
(transient-define-prefix majutsu-squash ()
  "Internal transient for jj squash operations."
  :man-page "jj-squash"
  :description "JJ Squash"
  :class 'majutsu-jj-transient-prefix
  :jj-command "squash"
  :transient-non-suffix t
  :incompatible '(("--into=" "--onto=")
                  ("--into=" "--insert-after=")
                  ("--into=" "--insert-before=")
                  ("--onto=" "--insert-after=")
                  ("--onto=" "--insert-before="))
  [["Selection"
    (majutsu-squash:--from)
    (majutsu-squash:--into)
    (majutsu-squash:--onto)
    (majutsu-squash:--insert-after)
    (majutsu-squash:--insert-before)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Patch Selection" :if majutsu-squash-interactive-selection-available-p
    (majutsu-interactive:select-hunk)
    (majutsu-interactive:select-file)
    (majutsu-interactive:select-region)
    ("C" "Clear patch selections" majutsu-interactive-clear :transient t)]
   ["Paths" :if-not majutsu-squash-interactive-selection-available-p
    (majutsu-squash:--)]
   ["Options"
    ("-k" "Keep emptied commit" ("-k" "--keep-emptied"))
    ("-u" "Use destination message" ("-u" "--use-destination-message"))
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("s" "Execute squash" majutsu-squash-execute)]]
  (interactive)
  (transient-setup
   'majutsu-squash nil nil
   :scope
   (majutsu-selection-session-begin)))

;;; _
(provide 'majutsu-squash)
;;; majutsu-squash.el ends here
