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
(require 'majutsu-file)
(require 'majutsu-selection)
(require 'majutsu-interactive)

(defclass majutsu-restore-option (majutsu-selection-option)
  ())

(defclass majutsu-restore--toggle-option (majutsu-selection-toggle-option)
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
  (with-current-buffer (majutsu-interactive--selection-buffer)
    (when (derived-mode-p 'majutsu-diff-mode)
      (mapcar (##if (string-prefix-p "--revisions=" %)
                    (concat "--changes-in=" (substring % 12))
                    %)
              majutsu-buffer-diff-range))))

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

(defun majutsu-restore-execute (args)
  "Execute jj restore with ARGS from the transient."
  (interactive (list (transient-args 'majutsu-restore)))
  (let* ((selection-buf (majutsu-interactive--selection-buffer))
         (patch (majutsu-interactive-build-patch-if-selected selection-buf t t))
         (args (if patch
                   (seq-remove (lambda (arg)
                                 (or (string= arg "--interactive")
                                     (string-prefix-p "--tool=" arg)))
                               args)
                 args)))
    (if patch
        (progn
          (majutsu-interactive-run-with-patch "restore" args patch)
          (with-current-buffer selection-buf
            (majutsu-interactive-clear)))
      (let ((exit (apply #'majutsu-run-jj "restore" args)))
        (when (zerop exit)
          (message "Restored successfully"))))))

;;; Infix Commands

(transient-define-argument majutsu-restore:--from ()
  :description "From"
  :class 'majutsu-restore-option
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :key "-f"
  :argument "--from="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-restore:--to ()
  :description "To"
  :class 'majutsu-restore-option
  :selection-label "[TO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :key "-t"
  :argument "--to="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-restore:--changes-in ()
  :description "Changes in"
  :class 'majutsu-restore-option
  :selection-label "[CHANGES-IN]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :key "-c"
  :argument "--changes-in="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-restore:from ()
  :description "From (toggle at point)"
  :class 'majutsu-restore--toggle-option
  :key "f"
  :argument "--from=")

(transient-define-argument majutsu-restore:to ()
  :description "To (toggle at point)"
  :class 'majutsu-restore--toggle-option
  :key "t"
  :argument "--to=")

(transient-define-argument majutsu-restore:changes-in ()
  :description "Changes-in (toggle at point)"
  :class 'majutsu-restore--toggle-option
  :key "c"
  :argument "--changes-in=")

(transient-define-argument majutsu-restore:-- ()
  :description "Limit to files"
  :class 'transient-files
  :key "--"
  :argument "--"
  :prompt "Limit to file,s: "
  :reader #'majutsu-read-files
  :multi-value t)

;;; Prefix

(transient-define-prefix majutsu-restore ()
  "Transient for jj restore operations."
  :man-page "jj-restore"
  :incompatible '(("--from=" "--changes-in=")
                  ("--to=" "--changes-in="))
  :transient-non-suffix t
  [
   :description "JJ Restore"
   ["Selection"
    (majutsu-restore:--from)
    (majutsu-restore:--to)
    (majutsu-restore:--changes-in)
    (majutsu-restore:from)
    (majutsu-restore:to)
    (majutsu-restore:changes-in)
    ("x" "Clear selections" majutsu-selection-clear :transient t)]
   ["Patch Selection" :if majutsu-interactive-selection-available-p
    (majutsu-interactive:select-hunk)
    (majutsu-interactive:select-file)
    (majutsu-interactive:select-region)
    ("C" "Clear patch selections" majutsu-interactive-clear :transient t)]
   ["Paths" :if-not majutsu-interactive-selection-available-p
    (majutsu-restore:--)]
   ["Options"
    ("-i" "Interactive" "--interactive")
    ("-d" "Restore descendants" "--restore-descendants")
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("r" "Restore" majutsu-restore-execute)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (let* ((file (majutsu-file-at-point))
         (files (cond
                 (file (list file))
                 ((and (derived-mode-p 'majutsu-diff-mode) majutsu-buffer-diff-filesets)
                  majutsu-buffer-diff-filesets)))
         (default-args (majutsu-restore--default-args))
         (value (if files
                    (append default-args (list (cons "--" files)))
                  default-args)))
    (transient-setup
     'majutsu-restore nil nil
     :scope (majutsu-selection-session-begin)
     :value value)))

;;; _
(provide 'majutsu-restore)
;;; majutsu-restore.el ends here
