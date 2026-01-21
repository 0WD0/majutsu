;;; majutsu-restore.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library implements jj restore and abandon operations,
;; including a transient for restore with --from, --to, --changes-in support.

;;; Code:

(require 'majutsu)
(require 'majutsu-selection)
(require 'majutsu-interactive)
(require 'majutsu-section)

(defclass majutsu-restore-option (majutsu-selection-option)
  ((selection-key :initarg :selection-key :initform nil)))

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

(defvar-local majutsu-restore--filesets nil
  "Filesets for the current restore operation.")

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
          (majutsu-run-jj "restore" file))
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
  :selection-key 'from
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :selection-type 'single
  :key "-f"
  :argument "--from="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-restore:--to ()
  :description "To"
  :class 'majutsu-restore-option
  :selection-key 'to
  :selection-label "[TO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :selection-type 'single
  :key "-t"
  :argument "--to="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-restore:--changes-in ()
  :description "Changes in"
  :class 'majutsu-restore-option
  :selection-key 'changes-in
  :selection-label "[CHANGES-IN]"
  :selection-face '(:background "dark magenta" :foreground "white")
  :selection-type 'single
  :key "-c"
  :argument "--changes-in="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-restore:from ()
  :description "From (toggle at point)"
  :class 'majutsu-restore--toggle-option
  :selection-key 'from
  :selection-type 'single
  :key "f"
  :argument "--from=")

(transient-define-argument majutsu-restore:to ()
  :description "To (toggle at point)"
  :class 'majutsu-restore--toggle-option
  :selection-key 'to
  :selection-type 'single
  :key "t"
  :argument "--to=")

(transient-define-argument majutsu-restore:changes-in ()
  :description "Changes-in (toggle at point)"
  :class 'majutsu-restore--toggle-option
  :selection-key 'changes-in
  :selection-type 'single
  :key "c"
  :argument "--changes-in=")

(defun majutsu-restore-clear-selections ()
  "Clear all restore selections."
  (interactive)
  (when (consp transient--suffixes)
    (dolist (obj transient--suffixes)
      (when (and (cl-typep obj 'majutsu-restore-option)
                 (memq (oref obj selection-key) '(from to changes-in)))
        (transient-infix-set obj nil))))
  (setq majutsu-restore--filesets nil)
  (when transient--prefix
    (transient--redisplay))
  (majutsu-selection-render)
  (message "Cleared all restore selections"))

(defun majutsu-restore--read-filesets (prompt &optional initial)
  "Read filesets with PROMPT and INITIAL value."
  (let ((input (read-string prompt (or initial ""))))
    (if (string-empty-p input)
        nil
      (split-string input))))

(defun majutsu-restore-set-filesets ()
  "Set filesets for restore."
  (interactive)
  (let* ((current (string-join (or majutsu-restore--filesets '()) " "))
         (new (majutsu-restore--read-filesets "Filesets (space-separated): " current)))
    (setq majutsu-restore--filesets new)
    (when transient--prefix
      (transient--redisplay))
    (message "Filesets: %s" (or (string-join new " ") "(all)"))))

(defun majutsu-restore--filesets-description ()
  "Return description for filesets display."
  (if majutsu-restore--filesets
      (format "Paths: %s" (string-join majutsu-restore--filesets " "))
    "Paths: (all)"))

(defun majutsu-restore--description ()
  "Return transient description with context info."
  (let ((parts (list "JJ Restore")))
    (when majutsu-restore--filesets
      (push (format "paths: %s" (string-join majutsu-restore--filesets " ")) parts))
    (string-join (nreverse parts) " | ")))

;;; Prefix

(transient-define-prefix majutsu-restore ()
  "Transient for jj restore operations."
  :man-page "jj-restore"
  :incompatible '(("--from=" "--changes-in=")
                  ("--to=" "--changes-in="))
  :transient-non-suffix t
  [
   :description majutsu-restore--description
   ["Selection"
    (majutsu-restore:--from)
    (majutsu-restore:--to)
    (majutsu-restore:--changes-in)
    (majutsu-restore:from)
    (majutsu-restore:to)
    (majutsu-restore:changes-in)
    ("x" "Clear selections" majutsu-restore-clear-selections :transient t)]
   ["Patch Selection" :if majutsu-interactive-selection-available-p
    (majutsu-interactive:select-hunk)
    (majutsu-interactive:select-file)
    (majutsu-interactive:select-region)
    ("C" "Clear patch selections" majutsu-interactive-clear :transient t)]
   ["Paths" :if-not majutsu-interactive-selection-available-p
    ("p" majutsu-restore--filesets-description majutsu-restore-set-filesets :transient t)]
   ["Options"
    ("-i" "Interactive" "--interactive")
    ("-d" "Restore descendants" "--restore-descendants")
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("r" "Restore" majutsu-restore-execute)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  ;; Initialize from context
  (let ((file (majutsu-file-at-point)))
    ;; Set filesets from context
    (setq majutsu-restore--filesets
          (cond
           (file (list file))
           ((and (derived-mode-p 'majutsu-diff-mode) majutsu-buffer-diff-filesets)
            majutsu-buffer-diff-filesets)
           (t nil)))
    (transient-setup
     'majutsu-restore nil nil
     :scope (majutsu-selection-session-begin)
     :value (or (majutsu-restore--default-args) '()))))

;;; _
(provide 'majutsu-restore)
;;; majutsu-restore.el ends here
