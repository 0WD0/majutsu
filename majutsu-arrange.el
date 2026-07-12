;;; majutsu-arrange.el --- Run jj's native Arrange TUI  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library hosts jj's native Arrange TUI in a Ghostel terminal session.

;;; Code:

(require 'majutsu-jj)
(require 'majutsu-terminal-session)

;;;###autoload
(defun majutsu-arrange-tui (&rest revsets)
  "Run jj's native Arrange TUI for REVSETS.

With no REVSETS, let jj use its configured `revsets.arrange' value.
Multiple REVSETS are passed as separate positional arguments.  When called
interactively with a prefix argument, prompt for one revset expression."
  (interactive
   (and current-prefix-arg
        (list (majutsu-read-revset "Arrange revisions" nil '("arrange")))))
  (majutsu-terminal-session-start-jj
   (cons "arrange" revsets)
   :origin-buffer (current-buffer)
   :title "arrange"
   :unchanged-message "jj arrange ended without a repository operation"))

(provide 'majutsu-arrange)
;;; majutsu-arrange.el ends here
