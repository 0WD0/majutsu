;;; majutsu-project.el --- project.el integration for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; project.el integration for Majutsu.  This file provides wrapper functions
;; `majutsu-project-log' and `majutsu-project-dispatch' (around `majutsu-log'
;; and `majutsu-dispatch', respectively) that can be registered as
;; `project-switch-commands' commands for `project-switch-project'.

;;; Code:

(require 'majutsu-log)
(require 'majutsu)

(require 'project)

;;;###autoload
(defun majutsu-project-log ()
  "Run `majutsu-log' in the current project's root.

To teach `project-switch-project' about this command, you have to
add something like this to your configuration:

    (keymap-set project-prefix-map \"j\" #\\='majutsu-project-log)
    (add-to-list \\='project-switch-commands
                 \\='(majutsu-project-log \"Majutsu\") t)

Also see `majutsu-project-dispatch'."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (majutsu-log-setup-buffer)))

;;;###autoload
(defun majutsu-project-dispatch ()
  "Run `majutsu-dispatch' in the current project's root.

Note that for `majutsu-dispatch' to operate in the selected project,
the current buffer's `default-directory' must be located in the
selected repository.  To achieve that, a Dired buffer is created.

To teach `project-switch-project' about this command, you have to
add something like this to your configuration:

    (keymap-set project-prefix-map \"M\" #\\='majutsu-project-dispatch)
    (add-to-list \\='project-switch-commands
                 \\='(majutsu-project-dispatch \"Majutsu Dispatch\") t)

Also see `majutsu-project-log'."
  (interactive)
  (let ((dir (project-root (project-current t))))
    (find-file dir)
    (transient-setup 'majutsu-dispatch)))

(provide 'majutsu-project)
;;; majutsu-project.el ends here
