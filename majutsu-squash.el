;;; majutsu-squash.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; This library provides jj squash transients, managing from and into
;; selections and assembling flags.

;;; Code:

(require 'majutsu)
(require 'majutsu-selection)

(defclass majutsu-squash-option (majutsu-selection-option)
  ((selection-key :initarg :selection-key :initform nil)))

(defclass majutsu-squash--toggle-option (majutsu-selection-toggle-option)
  ())

;;; majutsu-squash

(defun majutsu-squash-execute (args)
  "Execute squash with selections recorded in the transient."
  (interactive (list (transient-args 'majutsu-squash)))
  (let* ((keep (member "--keep" args))
         (ignore-immutable (member "--ignore-immutable" args))
         (froms (mapcar (lambda (s) (substring s 7))
                        (seq-filter (lambda (s) (string-prefix-p "--from=" s)) args)))
         (intos (mapcar (lambda (s) (substring s 7))
                        (seq-filter (lambda (s) (string-prefix-p "--into=" s)) args))))
    (cond
     ((and froms intos)
      (majutsu-squash-run froms (car intos) keep ignore-immutable))
     (froms
      (majutsu-squash-run froms nil keep ignore-immutable))
     ((magit-section-value-if 'jj-commit)
      (majutsu-squash-run (list (magit-section-value-if 'jj-commit)) nil keep ignore-immutable))
     (t
      (majutsu--message-with-log "No commit selected for squash")))))

(defun majutsu-squash-run (from-list into keep ignore-immutable)
  "Run jj squash using with-editor."
  (let* ((froms (seq-filter (lambda (rev)
                              (and rev (not (string-empty-p (string-trim rev)))))
                            (mapcar (lambda (rev) (and rev (string-trim rev)))
                                    from-list)))
         (froms (or froms '("@")))
         (args (append '("squash")
                       (apply #'append (mapcar (lambda (rev) (list "--from" rev)) froms))
                       (when into (list "--into" into))
                       (when keep '("--keep-emptied"))
                       (when ignore-immutable '("--ignore-immutable")))))
    (majutsu-run-jj-with-editor args)))

;;;; Infix Commands

(transient-define-argument majutsu-squash:--from ()
  :description "From"
  :class 'majutsu-squash-option
  :selection-key 'from
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :selection-type 'multi
  :key "-f"
  :argument "--from="
  :multi-value 'repeat
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-squash:--into ()
  :description "Into"
  :class 'majutsu-squash-option
  :selection-key 'into
  :selection-label "[INTO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :selection-type 'single
  :key "-t"
  :argument "--into="
  :reader #'majutsu-diff--transient-read-revset)

(transient-define-argument majutsu-squash:from ()
  :description "From (toggle at point)"
  :class 'majutsu-squash--toggle-option
  :selection-key 'from
  :selection-type 'multi
  :key "f"
  :argument "--from="
  :multi-value 'repeat)

(transient-define-argument majutsu-squash:into ()
  :description "Into (toggle at point)"
  :class 'majutsu-squash--toggle-option
  :selection-key 'into
  :selection-type 'single
  :key "t"
  :argument "--into=")

(defun majutsu-squash-clear-selections ()
  "Clear all squash selections."
  (interactive)
  (when (consp transient--suffixes)
    (dolist (obj transient--suffixes)
      (when (and (cl-typep obj 'majutsu-squash-option)
                 (memq (oref obj selection-key) '(from into)))
        (transient-infix-set obj nil))))
  (when transient--prefix
    (transient--redisplay))
  (majutsu-selection-render)
  (message "Cleared all squash selections"))

;;;; Prefix

(transient-define-prefix majutsu-squash ()
  "Internal transient for jj squash operations."
  :man-page "jj-squash"
  :transient-non-suffix t
  [:description "JJ Squash"
   ["Selection"
    (majutsu-squash:--from)
    (majutsu-squash:--into)
    (majutsu-squash:from)
    (majutsu-squash:into)
    ("c" "Clear selections" majutsu-squash-clear-selections :transient t)]
   ["Options"
    ("-k" "Keep emptied commit" "--keep")
    (majutsu-transient-arg-ignore-immutable)]
   ["Actions"
    ("s" "Execute squash" majutsu-squash-execute)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (transient-setup
   'majutsu-squash nil nil
   :scope
   (majutsu-selection-session-begin)))

;;; _
(provide 'majutsu-squash)
;;; majutsu-squash.el ends here
