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

(declare-function majutsu-diff--revision-metadata "majutsu-diff" ())
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

(defun majutsu-restore--default-args ()
  "Return default args from diff buffer context.
Diff =--revisions= / =-r= become Restore =--changes-in=; other range args
are passed through unchanged."
  (when (derived-mode-p 'majutsu-diff-mode)
    (if (and (equal (car majutsu-buffer-diff-range) "-r")
             (cadr majutsu-buffer-diff-range))
        (list (concat "--changes-in=" (cadr majutsu-buffer-diff-range)))
      (mapcar (lambda (arg)
                (if-let* ((rev (transient-arg-value "--revisions=" (list arg))))
                    (concat "--changes-in=" rev)
                  arg))
              majutsu-buffer-diff-range))))

(defun majutsu-restore--selector (args)
  "Return Restore selector canonicalized from ARGS.
The selector is either (:changes-in REV) or (:from FROM :to TO).
Missing --from/--to default to @, matching jj restore/diff."
  (let ((changes-in (transient-arg-value "--changes-in=" args))
        (from (transient-arg-value "--from=" args))
        (to (transient-arg-value "--to=" args)))
    (cond
     (changes-in (list :changes-in changes-in))
     ((or from to) (list :from (or from "@") :to (or to "@")))
     (t (list :changes-in "@")))))

(defun majutsu-restore--patch-selector (&optional buffer)
  "Return Restore selector for BUFFER, or nil when patch restore is unsafe.
Only single-revision diffs (with structured metadata) and explicit
--from/--to ranges can drive patch selection."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'majutsu-diff-mode)
      (let* ((range majutsu-buffer-diff-range)
             (from (transient-arg-value "--from=" range))
             (to (transient-arg-value "--to=" range))
             (revisions
              (append
               (seq-keep (lambda (arg)
                           (transient-arg-value "--revisions=" (list arg)))
                         range)
               (and (equal (car range) "-r") (cadr range)
                    (list (cadr range))))))
        (cond
         ((or from to)
          (majutsu-restore--selector (majutsu-restore--default-args)))
         ((null range)
          (list :changes-in "@"))
         ((and (= (length revisions) 1)
               (majutsu-diff--revision-metadata))
          (list :changes-in (car revisions))))))))

(defun majutsu-restore--check-patch-selector (args selector)
  "Signal unless ARGS match the displayed diff SELECTOR."
  (unless selector
    (user-error
     "Patch selection for restore requires a single-revision or explicit-range diff"))
  (unless (equal selector (majutsu-restore--selector args))
    (user-error
     "Patch selection for restore requires the displayed diff selector")))

(defun majutsu-restore-interactive-selection-available-p ()
  "Return non-nil when the current diff can safely drive patch Restore."
  (and (majutsu-interactive-selection-available-p)
       (majutsu-restore--patch-selector)))

(defun majutsu-restore--remove-interactive-tool-args (args)
  "Return ARGS without native interactive-editor or tool arguments."
  (let (out)
    (while args
      (let ((arg (pop args)))
        (cond
         ((member arg '("-i" "--interactive")))
         ((member arg '("-t" "--tool"))
          (when args (pop args)))
         ((and (stringp arg)
               (or (string-prefix-p "--tool=" arg)
                   (string-prefix-p "-t=" arg))))
         (t (push arg out)))))
    (nreverse out)))

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
               ;; jj presents destination on the left and restore source on
               ;; the right.  The complement plan keeps that source tree and
               ;; replays only unselected changes forward from the left.
               (plan (majutsu-interactive-build-replay-plan-if-selected
                      nil 'complement))
               (selector (and plan (majutsu-restore--patch-selector)))
               (args (if plan
                         (majutsu-restore--remove-interactive-tool-args args)
                       args)))
    (if plan
        (progn
          (majutsu-restore--check-patch-selector args selector)
          (majutsu-interactive-run-replay-plan "restore" args filesets plan)
          (majutsu-interactive-clear))
      (let ((exit (apply #'majutsu-run-jj
                         "restore"
                         (majutsu-jj-append-filesets args filesets))))
        (when (zerop exit)
          (message "Restored successfully"))))))

;;; Infix Commands

(transient-define-argument majutsu-restore:--from ()
  :description "From"
  :class 'majutsu-restore-option
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :selection-toggle-key "f"
  :selection-toggle-if-not #'majutsu-interactive-selection-available-p
  :shortarg "-f"
  :argument "--from="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-restore:--to ()
  :description "To"
  :class 'majutsu-restore-option
  :selection-label "[TO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :selection-toggle-key "t"
  :selection-toggle-if-not #'majutsu-interactive-selection-available-p
  :shortarg "-t"
  :argument "--to="
  :reader #'majutsu-transient-read-revset)

(transient-define-argument majutsu-restore:--changes-in ()
  :description "Changes in"
  :class 'majutsu-restore-option
  :selection-label "[CHANGES-IN]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-toggle-key "c"
  :selection-toggle-if-not #'majutsu-interactive-selection-available-p
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
   ["Paths" :if-not majutsu-interactive-selection-available-p
    (majutsu-restore:--)]
   ["Options"
    ("-i" "Interactive" ("-i" "--interactive"))
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
     :value value)))

;;; _
(provide 'majutsu-restore)
;;; majutsu-restore.el ends here
