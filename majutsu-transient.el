;;; majutsu-transient.el --- Transient helpers for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Shared Transient extensions used by Majutsu.

;;; Code:

(require 'cl-lib)
(require 'majutsu-jj)
(require 'subr-x)
(require 'transient)

(defclass majutsu-jj-transient-prefix (transient-prefix)
  ((jj-command :initarg :jj-command :initform nil))
  "Transient prefix backed by a jj command.

The `jj-command' slot is a string or list of strings used as native jj
completion context for revset readers.")

(defun majutsu-transient-default-revset ()
  "Return the default revset for transient revset readers."
  (or (magit-section-value-if 'jj-commit) "@"))

(defun majutsu-transient--jj-command-args (command)
  "Return COMMAND as a list of jj command arguments."
  (cond ((null command) nil)
        ((listp command) command)
        (t (list command))))

(defun majutsu-transient-jj-command-args ()
  "Return jj subcommand args for the active revset transient."
  (when-let* ((prefix (transient-prefix-object))
              ((cl-typep prefix 'majutsu-jj-transient-prefix)))
    (majutsu-transient--jj-command-args (oref prefix jj-command))))

(defun majutsu-transient-jj-option-arg ()
  "Return jj option arg for the active revset infix command."
  (string-remove-suffix "=" (oref (transient-suffix-object) argument)))

(defun majutsu-transient-revset-completion-args ()
  "Return jj native completion context for the active revset reader."
  (when-let* ((command (majutsu-transient-jj-command-args))
              (option (majutsu-transient-jj-option-arg)))
    (append command (list option))))

(defun majutsu-transient-expression-revset-p ()
  "Return non-nil if the active transient argument accepts a revset expression."
  (member (majutsu-transient-jj-option-arg)
          '("-r" "--revisions" "--revision" "--source" "--branch")))

(defun majutsu-transient-read-revset (prompt initial-input history)
  "Read a revset value for transient infix options."
  (unless current-prefix-arg
    (let ((completion-args (majutsu-transient-revset-completion-args)))
      (if (majutsu-transient-expression-revset-p)
          (majutsu-read-optional-revset prompt nil initial-input history completion-args)
        (majutsu-read-optional-single-revset prompt nil initial-input history completion-args)))))

(transient-define-argument majutsu-transient-arg-ignore-immutable ()
  :description "Ignore immutable"
  :class 'transient-switch
  :shortarg "-I"
  :argument "--ignore-immutable")

(defclass majutsu-transient-key-alias-suffix ()
  ((key-aliases :initarg :key-aliases :initform nil))
  "Mixin for suffixes that bind additional keys to the same item."
  :abstract t)

(cl-defgeneric majutsu-transient-key-aliases (obj)
  "Return additional key descriptions for OBJ.")

(cl-defmethod majutsu-transient-key-aliases ((obj majutsu-transient-key-alias-suffix))
  (ensure-list (oref obj key-aliases)))

(defun majutsu-transient--key-events (key)
  "Return the event list for key description or vector KEY."
  (listify-key-sequence (if (vectorp key) key (kbd key))))

(defun majutsu-transient-key= (a b)
  "Return non-nil when key descriptions or vectors A and B are equal."
  (equal (majutsu-transient--key-events a)
         (majutsu-transient--key-events b)))

(defun majutsu-transient-key-invoked-p (key)
  "Return non-nil when KEY invoked the current command."
  (majutsu-transient-key= key (this-command-keys-vector)))

(defun majutsu-transient--suffix-key-aliases (obj)
  "Return OBJ's effective, de-duplicated key aliases."
  (let (aliases)
    (dolist (key (ensure-list (majutsu-transient-key-aliases obj))
                 (nreverse aliases))
      (unless (or (null key)
                  (majutsu-transient-key= key (oref obj key))
                  (cl-find key aliases :test #'majutsu-transient-key=))
        (push key aliases)))))

(defun majutsu-transient--add-key-aliases (map)
  "Bind key aliases in MAP for the current transient."
  (dolist (obj transient--suffixes)
    (when (and (cl-typep obj 'majutsu-transient-key-alias-suffix)
               (not (oref obj inactive))
               (not (oref obj inapt)))
      (dolist (key (majutsu-transient--suffix-key-aliases obj))
        (let* ((kbd (kbd key))
               (cmd (oref obj command))
               (other (transient--lookup-key map kbd)))
          (cond
           ((not other)
            (define-key map kbd cmd))
           ((eq other cmd))
           (t
            (error "Cannot bind %S to %s; already bound to %s"
                   key cmd other)))))))
  map)

(cl-defmethod transient-format-key :around ((obj majutsu-transient-key-alias-suffix))
  (let ((transient-highlight-mismatched-keys
         (and transient-highlight-mismatched-keys
              (not (majutsu-transient--suffix-key-aliases obj)))))
    (cl-call-next-method)))

(cl-defmethod transient-format-key ((obj majutsu-transient-key-alias-suffix))
  (if-let* ((aliases (majutsu-transient--suffix-key-aliases obj)))
      (let* ((command (oref obj command))
             (key (mapconcat
                   (lambda (key)
                     (propertize key 'face (transient--key-face command key)))
                   (cons (oref obj key) aliases)
                   "/")))
        (when-let* ((width (oref transient--pending-group pad-keys)))
          (setq key (truncate-string-to-width key width nil ?\s)))
        key)
    (cl-call-next-method)))

(advice-add 'transient--make-transient-map
            :filter-return #'majutsu-transient--add-key-aliases)

(provide 'majutsu-transient)
;;; majutsu-transient.el ends here
