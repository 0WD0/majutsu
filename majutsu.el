;;; majutsu.el --- Interface to jj version control system  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu
;; Version: 0.6.0
;; Package-Requires: ((emacs "29.1") (transient "0.5.0") (magit "3.3.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Majutsu is an interface to the Jujutsu (jj) version control system
;; inspired by Magit.  This file loads core modules and defines the
;; top-level dispatch command.

;;; Code:

(require 'majutsu-core)

;;; Aliases

;;;###autoload
(defalias 'majutsu 'majutsu-log
  "Begin using Majutsu.

This alias for `majutsu-log' exists for better discoverability.

Instead of invoking this alias for `majutsu-log' using
\"M-x majutsu RET\", you should bind a key to `majutsu-log'.")

;;; Dispatch Popup

;;;###autoload
(transient-define-prefix majutsu-dispatch ()
  "Top-level Majutsu command dispatcher."
  ["Transient and dwim commands"
   [("k" "Abandon"           majutsu-abandon)
    ("b" "Bookmarks"         majutsu-bookmark)
    ("c" "Describe"          majutsu-describe)
    ("C" "Commit"            majutsu-commit)
    ("d" "Diff"              majutsu-diff)
    ("D" "Diff (dwim)"       majutsu-diff-dwim)
    ("e" "Edit change"       majutsu-edit-changeset)
    ("E" "Ediff"             majutsu-ediff)]
   [("G" "Git"               majutsu-git-transient)
    ("l" "Log options"       majutsu-log-transient)
    ("o" "New"               majutsu-new)
    ("O" "New (dwim)"        majutsu-new-dwim)
    ("m" "Resolve conflicts" majutsu-ediff-resolve)
    ("r" "Rebase"            majutsu-rebase)
    ("R" "Restore"           majutsu-restore)]
   [("s" "Squash"            majutsu-squash)
    ("S" "Split"             majutsu-split)
    ("A" "Absorb"            majutsu-absorb)
    ("y" "Duplicate"         majutsu-duplicate)
    ("Y" "Duplicate (dwim)"  majutsu-duplicate-dwim)
    ("Z" "Workspaces"        majutsu-workspace)
    ("C-/" "Undo"            majutsu-undo)
    ("C-?" "Redo"            majutsu-redo)]]
  ["Essential commands"
   :if-derived majutsu-mode
   [("g" "Refresh"           majutsu-refresh)
    ("q" "Quit"              majutsu-mode-bury-buffer)]
   [("?" "Help"              transient-help)
    ("$" "Process buffer"    majutsu-process-buffer)]
   [("C-x m"    "Show all key bindings"    describe-mode)]])

(provide 'majutsu)

(cl-eval-when (load eval)
  (require 'majutsu-jjdescription)
  (require 'majutsu-log)
  (require 'majutsu-diff)
  (require 'majutsu-ediff)
  (require 'majutsu-bookmark)
  (require 'majutsu-duplicate)
  (require 'majutsu-edit)
  (require 'majutsu-git)
  (require 'majutsu-interactive)
  (require 'majutsu-file)
  (require 'majutsu-annotate)
  (require 'majutsu-rebase)
  (require 'majutsu-restore)
  (require 'majutsu-absorb)
  (require 'majutsu-split)
  (require 'majutsu-squash)
  (require 'majutsu-sparse)
  (require 'majutsu-commit)
  (require 'majutsu-new)
  (require 'majutsu-op)
  (require 'majutsu-workspace)
  (require 'majutsu-conflict))

(with-eval-after-load 'evil
  (require 'majutsu-evil nil t))

;;; majutsu.el ends here
