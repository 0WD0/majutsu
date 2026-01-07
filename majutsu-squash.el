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
(require 'majutsu-interactive)

(defclass majutsu-squash-option (majutsu-selection-option)
  ((selection-key :initarg :selection-key :initform nil)))

(defclass majutsu-squash--toggle-option (majutsu-selection-toggle-option)
  ())

;;; majutsu-squash

(defun majutsu-squash--default-args ()
  "Return default args from diff buffer context."
  (with-current-buffer (majutsu-interactive--selection-buffer)
    (when (derived-mode-p 'majutsu-diff-mode)
      (mapcar (##cond
               ((string-prefix-p "--revisions=" %) (concat "--revision=" (substring % 12)))
               ((string-prefix-p "--to=" %) (concat "--into=" (substring % 5)))
               (t %))
              majutsu-buffer-diff-range))))

(defun majutsu-squash-execute (args)
  "Execute squash with selections recorded in the transient."
  (interactive (list (transient-args 'majutsu-squash)))
  (let* ((keep (member "--keep" args))
         (args (seq-remove (lambda (arg) (string= arg "--keep")) args))
         (selection-buf (majutsu-interactive--selection-buffer))
         (patch (majutsu-interactive-build-patch-if-selected selection-buf)))
    (when keep
      (push "--keep-emptied" args))
    (if patch
        (progn
          (majutsu-interactive-run-with-patch "squash" args patch)
          (with-current-buffer selection-buf
            (majutsu-interactive-clear)))
      (majutsu-run-jj-with-editor (cons "squash" args)))))

;;;; Infix Commands

(transient-define-argument majutsu-squash:--revision ()
  :description "Revision"
  :class 'majutsu-squash-option
  :selection-key 'revision
  :selection-label "[REV]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :selection-type 'single
  :key "-r"
  :argument "--revision="
  :reader #'majutsu-diff--transient-read-revset)

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
                 (memq (oref obj selection-key) '(revision from into)))
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
  [
   :description "JJ Squash"
   ["Selection"
    (majutsu-squash:--revision)
    (majutsu-squash:--from)
    (majutsu-squash:--into)
    (majutsu-squash:from)
    (majutsu-squash:into)
    ("c" "Clear selections" majutsu-squash-clear-selections :transient t)]
   ["Patch Selection" :if majutsu-interactive-selection-available-p
    (majutsu-interactive:select-hunk)
    (majutsu-interactive:select-file)
    (majutsu-interactive:select-region)
    ("C" "Clear patch selections" majutsu-interactive-clear :transient t)]
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
   (majutsu-selection-session-begin)
   :value (or (majutsu-squash--default-args) '())))

;;; _
(provide 'majutsu-squash)
;;; majutsu-squash.el ends here
