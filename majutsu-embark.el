;;; majutsu-embark.el --- Embark actions for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
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
(require 'majutsu-evolog)
(require 'majutsu-workspace)

(defvar embark-general-map)
(defvar embark-keymap-alist)

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
(keymap-set majutsu-embark-bookmark-map "RET" #'majutsu-edit-revision)
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

(defun majutsu-embark--register ()
  "Register Majutsu completion categories and action maps with Embark."
  (set-keymap-parent majutsu-embark-workspace-map embark-general-map)
  (set-keymap-parent majutsu-embark-bookmark-map embark-general-map)
  (add-to-list 'embark-keymap-alist
               '(majutsu-workspace . majutsu-embark-workspace-map))
  (add-to-list 'embark-keymap-alist
               '(majutsu-bookmark . majutsu-embark-bookmark-map)))

(if (featurep 'embark)
    (majutsu-embark--register)
  (with-eval-after-load 'embark
    (majutsu-embark--register)))

(provide 'majutsu-embark)
;;; majutsu-embark.el ends here
