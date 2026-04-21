;;; majutsu-command.el --- Ad-hoc command runners for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides Magit-style ad-hoc command runners for Majutsu.
;; Plain `jj ...' commands are routed through Majutsu's process helpers so
;; they inherit `--no-pager', color handling, process buffering, and
;; with-editor integration.  Other commands are run through the shell.

;;; Code:

(require 'majutsu-core)
(require 'majutsu-completion)

(require 'subr-x)

(defgroup majutsu-command nil
  "Ad-hoc command runners for Majutsu."
  :group 'majutsu)

(defcustom majutsu-shell-command-verbose-prompt t
  "Whether to show the working directory when reading a command.
This affects `majutsu-jj-command', `majutsu-jj-command-topdir',
`majutsu-shell-command', and `majutsu-shell-command-topdir'."
  :group 'majutsu-command
  :type 'boolean)

(defvar majutsu-jj-command-history nil)
(defvar majutsu-shell-command-history nil)

(defvar majutsu-read-jj-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'majutsu-jj-command-complete)
    (define-key map (kbd "<tab>") #'majutsu-jj-command-complete)
    (define-key map [remap completion-at-point]
                #'majutsu-jj-command-complete)
    (make-composed-keymap map minibuffer-local-shell-command-map))
  "Keymap used while reading jj commands.")

(defun majutsu--shell-command-directory (&optional topdir)
  "Return directory used for ad-hoc command runners.
When TOPDIR or `current-prefix-arg' is non-nil, use the current
workspace root.  Otherwise use `default-directory'."
  (if (or topdir current-prefix-arg)
      (majutsu--toplevel-safe)
    default-directory))

(defun majutsu--command-program-name (program)
  "Return PROGRAM's normalized executable name."
  (let ((name (downcase (file-name-nondirectory program))))
    (if (string-suffix-p ".exe" name)
        (string-remove-suffix ".exe" name)
      name)))

(defun majutsu--shell-command-needs-shell-p (command)
  "Return non-nil when COMMAND should be handed to the shell.
This conservatively detects shell operators that cannot be preserved by
`split-string-shell-command'."
  (string-match-p "[|&;<>`]" command))

(defun majutsu--shell-command-jj-args (command)
  "Return jj arguments parsed from COMMAND, or nil.
Only plain `jj ...' commands are recognized.  Commands using shell
operators are left for the shell runner."
  (unless (majutsu--shell-command-needs-shell-p command)
    (condition-case nil
        (when-let* ((argv (split-string-shell-command command))
                    (program (car argv))
                    (jj (majutsu-jj--executable))
                    ((string= (majutsu--command-program-name program)
                              (majutsu--command-program-name jj))))
          (cdr argv))
      (error nil))))

(defun majutsu--jj-command-args (command)
  "Return jj arguments parsed from COMMAND.
COMMAND may start with the jj executable name, but it does not have to.
Signal a user error if COMMAND is empty or uses shell syntax."
  (setq command (string-trim (or command "")))
  (when (string-empty-p command)
    (user-error "Need non-empty input"))
  (when (majutsu--shell-command-needs-shell-p command)
    (user-error "Shell syntax is not supported here; use `majutsu-shell-command'"))
  (let* ((argv (condition-case nil
                   (split-string-shell-command command)
                 (error
                  (user-error "Failed to parse command: %s" command))))
         (program (car argv))
         (jj (majutsu-jj--executable)))
    (cond
     ((null argv)
      (user-error "Need non-empty input"))
     ((string= (majutsu--command-program-name program)
               (majutsu--command-program-name jj))
      (setq argv (cdr argv))
      (when (and argv
                 (string= (majutsu--command-program-name (car argv))
                          (majutsu--command-program-name jj)))
        (setq argv (cdr argv)))
      (or argv
          (user-error "Need jj subcommand")))
     (t argv))))

(defun majutsu--start-jj-command (command)
  "Start jj COMMAND asynchronously and return the process."
  (apply #'majutsu-start-process
         (majutsu-jj--executable)
         nil
         (majutsu-process-jj-arguments (majutsu--jj-command-args command))))

(defun majutsu--jj-completion-argv (command)
  "Return argv to pass to jj's completion engine for COMMAND.
COMMAND is the minibuffer text before point.  Preserve an empty trailing
argument when completion happens after whitespace."
  (let* ((ends-in-space (or (string-empty-p command)
                            (string-match-p "[ 	]\'" command)))
         (argv (condition-case nil
                   (split-string-shell-command command)
                 (error nil)))
         (jj (majutsu-jj--executable)))
    (when (and argv
               (string= (majutsu--command-program-name (car argv))
                        (majutsu--command-program-name jj)))
      (setq argv (cdr argv)))
    (when ends-in-space
      (setq argv (append argv '(""))))
    argv))

(defun majutsu--jj-completion-items (command)
  "Return completion items for COMMAND using jj's native completer.
Each item is (CANDIDATE . HELP)."
  (majutsu-jj-completion-items (majutsu--jj-completion-argv command)))

(defun majutsu--jj-completion-table (items)
  "Return a completion table for completion ITEMS.
ITEMS is a list of (CANDIDATE . HELP)."
  (majutsu-completion-table items))

(defun majutsu--jj-command-completion-bounds ()
  "Return minibuffer token bounds for jj command completion."
  (let ((end (point)))
    (save-excursion
      (skip-chars-backward "^ 	\n" (minibuffer-prompt-end))
      (cons (point) end))))

(defun majutsu-jj-command-complete ()
  "Complete the jj command at point using jj's native completer."
  (interactive)
  (let* ((input (buffer-substring-no-properties (minibuffer-prompt-end) (point)))
         (items (majutsu--jj-completion-items input)))
    (if items
        (pcase-let ((`(,beg . ,end) (majutsu--jj-command-completion-bounds)))
          (completion-in-region beg end (majutsu--jj-completion-table items)))
      (minibuffer-message "No jj completions"))))

(defun majutsu--start-shell-command (command)
  "Start COMMAND asynchronously and return the process.
Plain `jj ...' commands are started directly using Majutsu's jj process
helpers.  Other commands are run through `shell-file-name'."
  (if-let* ((jj-args (majutsu--shell-command-jj-args command)))
      (apply #'majutsu-start-process
             (majutsu-jj--executable)
             nil
             (majutsu-process-jj-arguments jj-args))
    (majutsu-start-process shell-file-name nil shell-command-switch command)))

(defun majutsu-read-jj-command (&optional topdir)
  "Read a jj command for Majutsu runners.
When TOPDIR or `current-prefix-arg' is non-nil, prompt relative to the
workspace root.  The leading `jj' executable name is optional."
  (let ((default-directory (majutsu--shell-command-directory topdir)))
    (read-from-minibuffer (if majutsu-shell-command-verbose-prompt
                              (format "Async jj command in %s: "
                                      (abbreviate-file-name default-directory))
                            "Async jj command: ")
                          nil
                          majutsu-read-jj-command-map
                          nil
                          'majutsu-jj-command-history)))

(defun majutsu-read-shell-command (&optional topdir initial-input)
  "Read a shell command for Majutsu runners.
When TOPDIR or `current-prefix-arg' is non-nil, prompt relative to the
workspace root.  INITIAL-INPUT is inserted into the minibuffer."
  (let ((default-directory (majutsu--shell-command-directory topdir)))
    (read-shell-command (if majutsu-shell-command-verbose-prompt
                            (format "Async shell command in %s: "
                                    (abbreviate-file-name default-directory))
                          "Async shell command: ")
                        initial-input
                        'majutsu-shell-command-history)))

(defun majutsu--shell-command (command &optional directory)
  "Execute COMMAND asynchronously in DIRECTORY and display output."
  (setq command (string-trim (or command "")))
  (when (string-empty-p command)
    (user-error "Need non-empty input"))
  (let ((default-directory (or directory default-directory))
        process)
    (with-connection-local-variables
      (majutsu-with-editor
        (setq process (majutsu--start-shell-command command))))
    (majutsu-process-buffer)
    process))

(defun majutsu--run-jj-command (command &optional directory)
  "Execute jj COMMAND asynchronously in DIRECTORY and display output."
  (let ((default-directory (or directory default-directory))
        process)
    (with-connection-local-variables
      (majutsu-with-editor
        (setq process (majutsu--start-jj-command command))))
    (majutsu-process-buffer)
    process))

;;;###autoload
(defun majutsu-jj-command (command)
  "Execute COMMAND asynchronously; display output.

Interactively, prompt for a jj subcommand in the minibuffer.  A leading
`jj' executable name is accepted but not required.

With a prefix argument COMMAND is run in the workspace root, otherwise
in `default-directory'."
  (interactive (list (majutsu-read-jj-command nil)))
  (majutsu--run-jj-command command (majutsu--shell-command-directory)))

;;;###autoload
(defun majutsu-jj-command-topdir (command)
  "Execute COMMAND asynchronously in the workspace root; display output.

Interactively, prompt for a jj subcommand in the minibuffer.  A leading
`jj' executable name is accepted but not required."
  (interactive (list (majutsu-read-jj-command t)))
  (majutsu--run-jj-command command (majutsu--shell-command-directory t)))

;;;###autoload
(defun majutsu-shell-command (command)
  "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  With a prefix
argument COMMAND is run in the workspace root, otherwise in
`default-directory'."
  (interactive (list (majutsu-read-shell-command)))
  (majutsu--shell-command command (majutsu--shell-command-directory)))

;;;###autoload
(defun majutsu-shell-command-topdir (command)
  "Execute COMMAND asynchronously in the workspace root; display output.

Interactively, prompt for COMMAND in the minibuffer."
  (interactive (list (majutsu-read-shell-command t)))
  (majutsu--shell-command command (majutsu--shell-command-directory t)))

;;;###autoload(autoload 'majutsu-command "majutsu-command" nil t)
(transient-define-prefix majutsu-command ()
  "Run jj or another command."
  [["Run jj subcommand"
    ("!" "in workspace root"   majutsu-jj-command-topdir)
    ("p" "in working directory" majutsu-jj-command)]
   ["Run shell command"
    ("s" "in workspace root"   majutsu-shell-command-topdir)
    ("S" "in working directory" majutsu-shell-command)]])

(provide 'majutsu-command)
;;; majutsu-command.el ends here
