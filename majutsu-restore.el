;;; majutsu-restore.el --- Restore transient for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library implements jj restore and abandon operations,
;; including a transient for restore with --from, --to, --changes-in support.

;;; Code:

(require 'majutsu)
(require 'majutsu-diff-editor)

(defvar majutsu-buffer-diff-range)

(defclass majutsu-restore-option (majutsu-selection-option)
  ())

;;; Abandon

;;;###autoload
(defun majutsu-abandon ()
  "Abandon the changeset at point or in region."
  (interactive)
  (let ((revsets (or (magit-region-values 'jj-commit t)
                     (when-let* ((rev (magit-section-value-if 'jj-commit)))
                       (list rev)))))
    (if (not revsets)
        (message "No changeset at point to abandon")
      (let ((prompt (if (= (length revsets) 1)
                        (format "Abandon changeset %s? " (car revsets))
                      (format "Abandon %d changesets? " (length revsets)))))
        (if (not (majutsu-confirm 'abandon prompt))
            (message "Abandon canceled")
          (majutsu-run-jj "abandon" revsets))))))

;;; Restore

(defvar-local majutsu-restore--unsafe-diff-context nil
  "Diagnostic for a diff that cannot seed `jj restore' safely.")

(defvar-local majutsu-restore--diff-context-cache-key :uncomputed
  "Diff range and rendered generation for the cached Restore context.")

(defvar-local majutsu-restore--diff-context-cache-value nil
  "Cached semantic Restore context for the current rendered diff.")

(defun majutsu-restore--only-value (values)
  "Return the sole nonempty member of VALUES, or nil."
  (and (= (length values) 1)
       (not (string-empty-p (car values)))
       (car values)))

(defun majutsu-restore--endpoint-context (from to)
  "Return canonical restore endpoint context for FROM and TO."
  (when-let* ((source (majutsu-jj-resolve-single-commit (or from "@")))
              (destination (majutsu-jj-resolve-single-commit (or to "@"))))
    (list :endpoints source destination)))

(defun majutsu-restore--changes-context (revision)
  "Return canonical `--changes-in' context for REVISION."
  (when-let* ((commit (majutsu-jj-resolve-single-commit revision)))
    (list :changes-in commit)))

(defun majutsu-restore--compute-diff-context ()
  "Compute safe Restore defaults and patch context for the current diff.

The returned plist contains `:args' and `:patch-context'.  A diff for one
revision maps to `--changes-in'.  An aggregate revision diff is deliberately
not guessed into a different Restore operation and instead carries `:error'."
  (when (derived-mode-p 'majutsu-diff-mode)
    (let* ((range majutsu-buffer-diff-range)
           (from-values (majutsu-jj-option-values range "--from" "-f"))
           (to-values (majutsu-jj-option-values range "--to" "-t"))
           (revisions (majutsu-jj-option-values range "--revisions" "-r")))
      (cond
       ((or from-values to-values)
        (let* ((from (majutsu-restore--only-value from-values))
               (to (majutsu-restore--only-value to-values))
               (context (and (or from (null from-values))
                             (or to (null to-values))
                             (majutsu-restore--endpoint-context from to))))
          (if context
              (list :args (append (and from (list (concat "--from=" from)))
                                  (and to (list (concat "--to=" to))))
                    :patch-context context)
            (list :error "Restore diff endpoints must each resolve to one commit"))))
       ((null revisions)
        (let ((context (majutsu-restore--changes-context "@")))
          (list :args nil :patch-context context)))
       ((and (= (length revisions) 1)
             (not (string-empty-p (car revisions))))
        (if-let* ((context (majutsu-restore--changes-context (car revisions))))
            (list :args
                  (list (concat "--changes-in=" (cadr context)))
                  :patch-context context)
          (list :error
                "Restore patch selection requires a diff for exactly one commit")))
       (t
        (list :error
              "Restore patch selection does not support aggregate revision diffs"))))))

(defun majutsu-restore--diff-context ()
  "Return the cached semantic Restore context for the current diff."
  (when (derived-mode-p 'majutsu-diff-mode)
    (let ((key (list (copy-tree majutsu-buffer-diff-range)
                     (buffer-chars-modified-tick))))
      (unless (equal key majutsu-restore--diff-context-cache-key)
        (setq-local majutsu-restore--diff-context-cache-key key)
        (setq-local majutsu-restore--diff-context-cache-value
                    (majutsu-restore--compute-diff-context)))
      majutsu-restore--diff-context-cache-value)))

(defun majutsu-restore--args-context (args)
  "Return the canonical Restore tree context selected by ARGS."
  (let* ((changes (majutsu-jj-option-values args "--changes-in" "-c"))
         (from (majutsu-jj-option-values args "--from" "-f"))
         (to (append (majutsu-jj-option-values args "--to" "-t")
                     (majutsu-jj-option-values args "--into"))))
    (cond
     ((not (or changes from to))
      (majutsu-restore--changes-context "@"))
     ((and changes (not (or from to)))
      (when-let* ((revision (majutsu-restore--only-value changes)))
        (majutsu-restore--changes-context revision)))
     ((and (not changes)
           (<= (length from) 1)
           (<= (length to) 1))
      (let ((from-value (majutsu-restore--only-value from))
            (to-value (majutsu-restore--only-value to)))
        (and (or from-value (null from))
             (or to-value (null to))
             (majutsu-restore--endpoint-context
              from-value to-value)))))))

(defun majutsu-restore--check-patch-context (args context)
  "Signal unless ARGS select the same Restore tree pair as CONTEXT."
  (unless context
    (user-error "Patch selection is not safe for this diff context"))
  (unless (equal (majutsu-restore--args-context args) context)
    (user-error "Patch selection requires the source and destination shown by the diff")))

(defun majutsu-restore--pin-patch-context (args context)
  "Return ARGS pinned to the canonical Restore CONTEXT."
  (pcase context
    (`(:changes-in ,commit)
     (setq args (majutsu-jj-set-option-value args "--from" nil "-f"))
     (setq args (majutsu-jj-set-option-value args "--to" nil "-t"))
     (setq args (majutsu-jj-set-option-value args "--into" nil))
     (majutsu-jj-set-option-value args "--changes-in" commit "-c"))
    (`(:endpoints ,source ,destination)
     (setq args (majutsu-jj-set-option-value args "--changes-in" nil "-c"))
     (setq args (majutsu-jj-set-option-value args "--into" nil))
     (setq args (majutsu-jj-set-option-value args "--from" source "-f"))
     (majutsu-jj-set-option-value args "--to" destination "-t"))
    (_ args)))

(defun majutsu-restore--explicit-context-p (args)
  "Return non-nil when ARGS explicitly choose a Restore tree context."
  (or (majutsu-jj-option-values args "--changes-in" "-c")
      (majutsu-jj-option-values args "--from" "-f")
      (majutsu-jj-option-values args "--to" "-t")
      (majutsu-jj-option-values args "--into")))

(defun majutsu-restore-interactive-selection-available-p ()
  "Return non-nil when this diff has a safe Restore patch context."
  (and (majutsu-interactive-selection-available-p)
       (plist-get (majutsu-restore--diff-context) :patch-context)))

(defun majutsu-restore--default-args ()
  "Return default args from diff buffer context."
  (setq-local majutsu-restore--unsafe-diff-context nil)
  (when-let* ((context (majutsu-restore--diff-context)))
    (setq-local majutsu-restore--unsafe-diff-context
                (plist-get context :error))
    (plist-get context :args)))

;;;###autoload
(defun majutsu-restore-dwim ()
  "Restore working copy from parent (discard all changes).
In diff buffer on a file section, restore only that file."
  (interactive)
  (let ((file (majutsu-file-at-point)))
    (if file
        (when (yes-or-no-p (format "Discard changes to %s? " file))
          (majutsu-run-jj "restore" (majutsu-jj-fileset-quote file)))
      (when (yes-or-no-p "Discard all working copy changes? ")
        (majutsu-run-jj "restore")))))

;;;###autoload(autoload 'majutsu-restore-execute "majutsu-restore" nil t)
(transient-define-suffix majutsu-restore-execute (args)
  "Execute jj restore with ARGS from the transient."
  :description "Execute restore"
  :class 'majutsu-transient-default-action-suffix
  (interactive (list (transient-args 'majutsu-restore)))
  (pcase-let* ((`(,args ,filesets) (majutsu-filesets-split-transient-value args))
               (jj-editor-p (majutsu-diff-editor-interactive-arguments-p args))
               (diff-context (majutsu-restore--diff-context))
               ;; A jj editor does not consume the Emacs selection, so avoid
               ;; validating a possibly different selection owner here.
               (plan (and (not jj-editor-p)
                          (majutsu-interactive-build-replay-plan-if-selected
                           nil 'complement 'majutsu-restore))))
    (when (and (plist-get diff-context :error)
               (not (majutsu-restore--explicit-context-p args)))
      (user-error "%s" (plist-get diff-context :error)))
    (when plan
      (majutsu-restore--check-patch-context
       args (plist-get diff-context :patch-context))
      (setq args (majutsu-restore--pin-patch-context
                  args (plist-get diff-context :patch-context))))
    (cond
     ;; Explicit jj editor flags win over an existing Majutsu patch selection.
     ;; Do not clear it: the user may return and apply it later.
     (jj-editor-p
      (majutsu-diff-editor-start
       "restore" args filesets :origin-buffer (current-buffer)))
     (plan
      ;; A transient opened at a file section carries that file as a convenient
      ;; default for ordinary Restore.  A patch selection instead belongs to
      ;; the entire rendered diff.  Reuse the diff's own matcher so jj checks
      ;; out exactly the files represented by the inverse patch.
      (majutsu-interactive-run-replay-plan
       "restore"
       (majutsu-diff-editor-strip-interactive-arguments args)
       (and (derived-mode-p 'majutsu-diff-mode)
            (copy-sequence majutsu-buffer-diff-filesets))
       plan))
     (t
      (let ((exit (apply #'majutsu-run-jj
                         "restore"
                         (majutsu-jj-append-filesets args filesets))))
        (when (zerop exit)
          (message "Restored successfully")))))))

;;; Infix Commands

(transient-define-argument majutsu-restore:--from ()
  :description "From"
  :class 'majutsu-restore-option
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :selection-toggle-key "f"
  :selection-toggle-if-not #'majutsu-restore-interactive-selection-available-p
  :shortarg "-f"
  :argument "--from="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-restore:--to ()
  :description "To"
  :class 'majutsu-restore-option
  :selection-label "[TO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :selection-toggle-key "t"
  :selection-toggle-if-not #'majutsu-restore-interactive-selection-available-p
  :shortarg "-t"
  :argument "--to="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-restore:--changes-in ()
  :description "Changes in"
  :class 'majutsu-restore-option
  :selection-label "[CHANGES-IN]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-toggle-key "c"
  :selection-toggle-if-not #'majutsu-restore-interactive-selection-available-p
  :shortarg "-c"
  :argument "--changes-in="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-restore:-- ()
  :description "Limit to files"
  :class 'transient-files
  :key "--"
  :argument "--"
  :prompt "Limit to file,s: "
  :reader #'majutsu-read-files
  :multi-value t)

;;; Prefix

;;;###autoload(autoload 'majutsu-restore "majutsu-restore" nil t)
(transient-define-prefix majutsu-restore ()
  "Transient for jj restore operations."
  :man-page "jj-restore"
  :description "JJ Restore"
  :class 'majutsu-jj-transient-prefix
  :jj-command "restore"
  :incompatible '(("--from=" "--changes-in=")
                  ("--to=" "--changes-in="))
  :transient-non-suffix t
  [["Selection"
    (majutsu-restore:--from)
    (majutsu-restore:--to)
    (majutsu-restore:--changes-in)
    ("x" "Clear selections" majutsu-selection-clear :transient t)]
   ["Patch Selection" :if majutsu-restore-interactive-selection-available-p
    (majutsu-interactive:select-hunk)
    (majutsu-interactive:select-file)
    (majutsu-interactive:select-region)
    ("C" "Clear patch selections" majutsu-interactive-clear :transient t)]
   ["Paths" :if-not majutsu-restore-interactive-selection-available-p
    (majutsu-restore:--)]
   ["Options"
    ("-i" "Interactive" ("-i" "--interactive"))
    ("=t" "Tool" "--tool=")
    ("-d" "Restore descendants" "--restore-descendants")
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("r" "Execute restore" majutsu-restore-execute)]]
  (interactive)
  (let* ((file (majutsu-file-at-point))
         (files (cond
                 (file (list file))
                 ((and (derived-mode-p 'majutsu-diff-mode) majutsu-buffer-diff-filesets)
                  majutsu-buffer-diff-filesets)))
         (default-args (majutsu-restore--default-args))
         (value (majutsu-filesets-build-transient-value default-args files)))
    (transient-setup
     'majutsu-restore nil nil
     :scope (majutsu-selection-session-begin)
     :value value)
    (when majutsu-restore--unsafe-diff-context
      (message "%s" majutsu-restore--unsafe-diff-context))))

;;; _
(provide 'majutsu-restore)
;;; majutsu-restore.el ends here
