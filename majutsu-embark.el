;;; majutsu-embark.el --- Embark actions for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Optional Embark actions for Majutsu completion categories.  Loading
;; `majutsu' registers this integration after Embark becomes available; Embark
;; remains an optional dependency.

;;; Code:

(require 'majutsu-bookmark)
(require 'majutsu-diff)
(require 'majutsu-edit)
(require 'majutsu-git)
(require 'majutsu-evolog)
(require 'majutsu-workspace)

(defvar embark-general-map)
(defvar embark-keymap-alist)
(defvar embark-target-finders)
(defvar embark-default-action-overrides)

(defvar-keymap majutsu-embark-workspace-map
  :doc "Embark actions for Majutsu workspace candidates.")

;; Set these outside `defvar-keymap' so reloading the integration updates an
;; already-bound action map.
(keymap-set majutsu-embark-workspace-map "RET" #'majutsu-workspace-visit-name)
(keymap-set majutsu-embark-workspace-map "v" #'majutsu-workspace-visit-name)
(keymap-set majutsu-embark-workspace-map "d" #'majutsu-workspace-dired)
(keymap-set majutsu-embark-workspace-map "W" #'majutsu-workspace-copy-root)

(defvar-keymap majutsu-embark-bookmark-map
  :doc "Embark actions for Majutsu bookmark candidates.")

;; Keep Embark's general bindings (notably `w' for copying) available through
;; the parent map.  These keys mirror Majutsu's bookmark and revision commands.
(keymap-set majutsu-embark-bookmark-map "RET" #'majutsu-diff-revset)
(keymap-set majutsu-embark-bookmark-map "e" #'majutsu-edit-revision)
(keymap-set majutsu-embark-bookmark-map "D" #'majutsu-diff-revset)
(keymap-set majutsu-embark-bookmark-map "v" #'majutsu-evolog)
(keymap-set majutsu-embark-bookmark-map "p" #'majutsu-bookmark-advance-patterns)
(keymap-set majutsu-embark-bookmark-map "s" #'majutsu-bookmark-set)
(keymap-set majutsu-embark-bookmark-map "m" #'majutsu-bookmark-move)
(keymap-set majutsu-embark-bookmark-map "M" #'majutsu-bookmark-move-allow-backwards)
(keymap-set majutsu-embark-bookmark-map "r" #'majutsu-bookmark-rename)
(keymap-set majutsu-embark-bookmark-map "d" #'majutsu-bookmark-delete)
(keymap-set majutsu-embark-bookmark-map "f" #'majutsu-bookmark-forget)

(defvar-keymap majutsu-embark-revision-map
  :doc "Inspect a revision without exposing local bookmark mutations."
  "RET" #'majutsu-diff-revset
  "D" #'majutsu-diff-revset
  "e" #'majutsu-edit-revision
  "v" #'majutsu-evolog)

(defvar-keymap majutsu-embark-tracked-bookmark-map
  :parent majutsu-embark-revision-map
  "u" #'majutsu-bookmark-untrack-ref)

(defvar-keymap majutsu-embark-untracked-bookmark-map
  :parent majutsu-embark-revision-map
  "t" #'majutsu-bookmark-track-ref)

(defvar-keymap majutsu-embark-remote-map
  :doc "Actions on one configured Git remote."
  "f" #'majutsu-git-fetch-from
  "r" #'majutsu-git-remote-rename
  "d" #'majutsu-git-remote-remove)

(defun majutsu-embark-target-section ()
  "Return an Embark target for the current Majutsu section."
  (magit-section-case
    (jj-bookmark
     (with-slots (value remote tracked) it
       (cons (cond ((null remote) 'majutsu-bookmark)
                   ((equal remote "git") 'majutsu-git-bookmark)
                   (tracked 'majutsu-tracked-bookmark)
                   (t 'majutsu-untracked-bookmark))
             value)))
    (majutsu-revision-section (cons 'majutsu-revision (oref it value)))
    (jj-git-remote (cons 'majutsu-remote (oref it value)))))

(defun majutsu-embark--register ()
  "Register optional actions and structured section targets with Embark."
  (dolist (map (list majutsu-embark-workspace-map majutsu-embark-bookmark-map
                     majutsu-embark-revision-map majutsu-embark-remote-map))
    (set-keymap-parent map embark-general-map))
  (dolist (entry '((majutsu-workspace . majutsu-embark-workspace-map)
                   (majutsu-bookmark . majutsu-embark-bookmark-map)
                   (majutsu-revision . majutsu-embark-revision-map)
                   (majutsu-git-bookmark . majutsu-embark-revision-map)
                   (majutsu-tracked-bookmark . majutsu-embark-tracked-bookmark-map)
                   (majutsu-untracked-bookmark . majutsu-embark-untracked-bookmark-map)
                   (majutsu-remote . majutsu-embark-remote-map)))
    (setf (alist-get (car entry) embark-keymap-alist) (cdr entry)))
  (dolist (type '(majutsu-bookmark majutsu-revision majutsu-git-bookmark
                  majutsu-tracked-bookmark majutsu-untracked-bookmark))
    (setf (alist-get type embark-default-action-overrides) #'majutsu-diff-revset))
  (setf (alist-get 'majutsu-workspace embark-default-action-overrides)
        #'majutsu-workspace-visit-name)
  (add-hook 'embark-target-finders #'majutsu-embark-target-section))

(if (featurep 'embark)
    (majutsu-embark--register)
  (with-eval-after-load 'embark
    (majutsu-embark--register)))

(provide 'majutsu-embark)
;;; majutsu-embark.el ends here
