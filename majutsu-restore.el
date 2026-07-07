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
  "Return default args from diff buffer context."
  (when (derived-mode-p 'majutsu-diff-mode)
    (mapcar (lambda (arg)
              (if-let* ((rev (transient-arg-value "--revisions=" (list arg))))
                  (concat "--changes-in=" rev)
                arg))
            majutsu-buffer-diff-range)))

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
               (patch (majutsu-interactive-build-patch-if-selected nil t t))
               (args (if patch
                         (seq-remove (lambda (arg)
                                       (or (string= arg "--interactive")
                                           (transient-arg-value "--tool=" (list arg))))
                                     args)
                       args)))
    (if patch
        (progn
          (majutsu-interactive-run-with-patch "restore" args filesets patch)
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
  :incompatible '(("--from=" "--changes-in=")
                  ("--to=" "--changes-in="))
  :transient-non-suffix t
  [["Selection"
    (majutsu-restore:--from)
    (majutsu-restore:--to)
    (majutsu-restore:--changes-in)
    ("x" "Clear selections" majutsu-selection-clear :transient t)]
   ["Patch Selection" :if majutsu-interactive-selection-available-p
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
