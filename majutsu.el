;;; majutsu.el --- Interface to jj version control system  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1") (transient "0.5.0") (magit "3.3.0"))

;;; Commentary:

;; Majutsu is an interface to the Jujutsu (jj) version control system
;; inspired by Magit.  This file loads core modules and defines the
;; top-level dispatch command.

;;; Code:

(require 'majutsu-core)
(require 'majutsu-process)
(require 'majutsu-log)
(require 'majutsu-diff)
(require 'majutsu-jjdescription)
(require 'majutsu-selection)

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
  [:description "Majutsu Commands"
   :class transient-columns
   ["Basic Operations"
    ("g" "Refresh log" majutsu-log-refresh)
    ("c" "Commit" majutsu-commit)
    ("e" "Edit change" majutsu-edit-changeset)
    ("u" "Undo" majutsu-undo)
    ("R" "Redo" majutsu-redo)
    ("l" "Log options" majutsu-log-transient)
    ("N" "New" majutsu-new)
    ("n" "New (transient)" majutsu-new-transient)
    ("y" "Duplicate (transient)" majutsu-duplicate-transient)
    ("Y" "Duplicate" majutsu-duplicate)
    ("a" "Abandon" majutsu-abandon)
    ("d" "Describe" majutsu-describe)
    ("s" "Squash" majutsu-squash-transient)]
   ["Advanced"
    ("r" "Rebase" majutsu-rebase-transient)
    ("b" "Bookmarks" majutsu-bookmark-transient)
    ("G" "Git" majutsu-git-transient)]
   ["Diff & Fix"
    ("D" "Diff menu" majutsu-diff-transient)
    ("E" "DiffEdit (ediff)" majutsu-diffedit-emacs)
    ("M" "DiffEdit (smerge)" majutsu-diffedit-smerge)]
   ["Exit"
    ("?" "Help" transient-help)
    ("q" "Quit" transient-quit-one)]])

(provide 'majutsu)

(cl-eval-when (load eval)
  (require 'majutsu-duplicate)
  (require 'majutsu-edit)
  (require 'majutsu-op)
  (require 'majutsu-git)
  (require 'majutsu-rebase)
  (require 'majutsu-restore)
  (require 'majutsu-commit)
  (require 'majutsu-new)
  (require 'majutsu-bookmark))

(with-eval-after-load 'evil
  (require 'majutsu-evil nil t))

;;; majutsu.el ends here
