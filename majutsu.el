;;; majutsu.el --- Interface to jj version control system  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu
;; Version: 0.6.0
;; Package-Requires: ((emacs "29.1") (compat "31.0.0.1") (transient "0.13.0") (magit "4.4.0") (consult "1.0") (plz "0.9.1"))

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

(defun majutsu-dispatch--evil-keys-p ()
  "Return non-nil when `majutsu-dispatch' should offer Evil-facing keys.
Evil integration must be enabled, and the buffer from which the
dispatcher is invoked must be in Evil's Normal, Visual, or Motion
state."
  (and (bound-and-true-p evil-local-mode)
       (bound-and-true-p majutsu-evil-enable-integration)
       (memq (bound-and-true-p evil-state) '(normal visual motion))))

;;;###autoload(autoload 'majutsu-dispatch "majutsu" nil t)
(transient-define-prefix majutsu-dispatch ()
  "Top-level Majutsu command dispatcher."
  ["Transient and dwim commands"
   [("k" "Abandon"           majutsu-abandon
     :if-not majutsu-dispatch--evil-keys-p)
    ("x" "Abandon"           majutsu-abandon
     :if majutsu-dispatch--evil-keys-p)
    ("b" "Bookmarks"         majutsu-bookmark)
    ("t" "Tags"              majutsu-tag)
    ("c" "Describe"          majutsu-describe)
    ("C" "Commit"            majutsu-commit)
    ("m" "Metaedit"          majutsu-metaedit)
    ("d" "Diff"              majutsu-diff)
    ("v" "Evolution log"     majutsu-evolog)
    ("D" "Diff (dwim)"       majutsu-diff-dwim)
    ("e" "Edit revision"     majutsu-edit)
    ("E" "Ediff"             majutsu-ediff)
    ("M" "Resolve conflicts" majutsu-resolve)]
   [("G" "Git"               majutsu-git-transient)
    ("!" "Run"               majutsu-command)
    ("l" "Log options"       majutsu-log-transient
     :if-not majutsu-dispatch--evil-keys-p)
    ("L" "Log options"       majutsu-log-transient
     :if majutsu-dispatch--evil-keys-p)
    ("o" "New"               majutsu-new)
    ("O" "New (dwim)"        majutsu-new-dwim)
    ("r" "Rebase"            majutsu-rebase)
    ("P" "Simplify parents"  majutsu-simplify-parents)
    ("V" "Revert"            majutsu-revert
     :if-not majutsu-dispatch--evil-keys-p)
    ("_" "Revert"            majutsu-revert
     :if majutsu-dispatch--evil-keys-p)
    ("R" "Restore"           majutsu-restore)]
   [("s" "Squash"            majutsu-squash)
    ("S" "Split"             majutsu-split)
    ("a" "Absorb"            majutsu-absorb)
    ("A" "Arrange (TUI)"     majutsu-arrange-tui)
    ("y" "Duplicate"         majutsu-duplicate)
    ("j" "Sign"              majutsu-sign)
    ("J" "Unsign"            majutsu-unsign)
    (">" "Sparse"            majutsu-sparse)
    ("Z" "Workspaces"        majutsu-workspace
     :if-not majutsu-dispatch--evil-keys-p)
    ("*" "Workspaces"        majutsu-workspace
     :if majutsu-dispatch--evil-keys-p)
    ("X" "Operations"        majutsu-op-transient)
    ("C-/" "Undo"            majutsu-undo
     :if-not majutsu-dispatch--evil-keys-p)
    ("u" "Undo"              majutsu-undo
     :if majutsu-dispatch--evil-keys-p)
    ("C-?" "Redo"            majutsu-redo
     :if-not majutsu-dispatch--evil-keys-p)
    ("C-r" "Redo"            majutsu-redo
     :if majutsu-dispatch--evil-keys-p)]]
  ["Essential commands"
   :if-derived majutsu-mode
   [("g" "Refresh"           majutsu-refresh)
    ("q" "Quit"              majutsu-mode-bury-buffer)]
   [("?" "Help"              transient-help)
    ("$" "Process buffer"    majutsu-process-buffer
     :if-not majutsu-dispatch--evil-keys-p)
    ("`" "Process buffer"    majutsu-process-buffer
     :if majutsu-dispatch--evil-keys-p)]
   [("C-x m"    "Show all key bindings"    describe-mode)]]
  ["Copy commands"
   :if-derived (majutsu-log-mode majutsu-evolog-mode majutsu-op-log-mode)
   [("w" "Copy…"             majutsu-row-copy-transient)]])

(provide 'majutsu)

(cl-eval-when (load eval)
  (require 'majutsu-template)
  (require 'majutsu-jjdescription)
  (require 'majutsu-ref)
  (require 'majutsu-remote)
  (require 'majutsu-row)
  (require 'majutsu-log)
  (require 'majutsu-diff)
  (require 'majutsu-ediff)
  (require 'majutsu-bookmark)
  (require 'majutsu-tag)
  (require 'majutsu-duplicate)
  (require 'majutsu-edit)
  (require 'majutsu-diffedit)
  (require 'majutsu-git)
  (require 'majutsu-command)
  (require 'majutsu-interactive)
  (require 'majutsu-arrange)
  (require 'majutsu-file)
  (require 'majutsu-annotate)
  (require 'majutsu-rebase)
  (require 'majutsu-simplify-parents)
  (require 'majutsu-revert)
  (require 'majutsu-restore)
  (require 'majutsu-absorb)
  (require 'majutsu-split)
  (require 'majutsu-squash)
  (require 'majutsu-sparse)
  (require 'majutsu-commit)
  (require 'majutsu-metaedit)
  (require 'majutsu-sign)
  (require 'majutsu-new)
  (require 'majutsu-op)
  (require 'majutsu-evolog)
  (require 'majutsu-workspace)
  (require 'majutsu-conflict)
  (require 'majutsu-resolve)
  (require 'majutsu-gerrit))

(with-eval-after-load 'evil
  (require 'majutsu-evil nil t))

(with-eval-after-load 'embark
  (require 'majutsu-embark))

;;; majutsu.el ends here
