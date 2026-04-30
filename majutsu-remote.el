;;; majutsu-remote.el --- Git remote readers for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shared readers and completion payloads for jj Git remote names.

;;; Code:

(require 'majutsu)

(require 'subr-x)

(declare-function magit-section-value-if "magit-section" (type))

(defvar majutsu-remote-name-history nil
  "Minibuffer history for exact remote-name input.")

(defvar majutsu-remote-pattern-history nil
  "Minibuffer history for remote name-pattern input.")

(defun majutsu-remote--validate-new-name (remote)
  "Signal a user error if REMOTE is not a valid new jj Git remote name."
  (cond
   ((string= remote "git")
    (user-error "Git remote named 'git' is reserved for local Git repository"))
   ((string-match-p "/" remote)
    (user-error "Git remotes with slashes are incompatible with jj: %s" remote))))

(defun majutsu-remote--parse-list-line (line)
  "Parse one `jj git remote list` LINE into a plist."
  (let ((raw (string-trim (substring-no-properties (or line "")))))
    (when (and (not (string-empty-p raw))
               (string-match "\\`\\([^[:blank:]]+\\)[[:blank:]]+\\(.+?\\)\\(?:[[:blank:]]+(push: \\(.*\\))\\)?\\'" raw))
      (list :name (match-string 1 raw)
            :fetch-url (string-trim (match-string 2 raw))
            :push-url (when-let* ((push (match-string 3 raw)))
                        (string-trim push))))))

(defun majutsu-remote-candidate-data (&optional directory)
  "Return completion payload for Git remote names in DIRECTORY."
  (let ((default-directory (or directory default-directory))
        (entries (make-hash-table :test #'equal))
        candidates)
    (condition-case _
        (dolist (line (or (majutsu-jj-lines "git" "remote" "list") '()))
          (when-let* ((entry (majutsu-remote--parse-list-line line))
                      (name (plist-get entry :name)))
            (unless (gethash name entries)
              (setq candidates (append candidates (list name))))
            (puthash name entry entries)))
      (error nil))
    (list :category 'majutsu-remote
          :candidates candidates
          :entries entries)))

(defun majutsu-remote-names (&optional directory)
  "Return a list of Git remote names for DIRECTORY."
  (plist-get (majutsu-remote-candidate-data directory) :candidates))

(defun majutsu-remote-at-point ()
  "Return the Git remote name at point, if any."
  (magit-section-value-if 'jj-git-remote))

(defun majutsu-read-remote-name (prompt &optional require-match default)
  "Read a Git remote name with PROMPT.
If REQUIRE-MATCH is non-nil, require an existing remote name.  DEFAULT
is preselected when non-nil; otherwise the remote section at point is used."
  (let* ((root (ignore-errors (majutsu--toplevel-safe)))
         (payload (majutsu-remote-candidate-data root))
         (candidates (plist-get payload :candidates))
         (default (or default (majutsu-remote-at-point))))
    (cond
     (candidates)
     (require-match
      (user-error "No Git remotes"))
     (t
      (setq payload (plist-put payload :candidates '("origin")))))
    (majutsu-completing-read-payload prompt payload
                                     nil (or require-match 'any) nil
                                     'majutsu-remote-name-history
                                     default 'majutsu-remote nil
                                     (or root default-directory))))

(defun majutsu-transient-read-remote-name (prompt initial-input _history)
  "Read a named Git remote for a transient option."
  (majutsu-read-remote-name prompt t initial-input))

(defun majutsu-read-new-remote-name (prompt &optional default)
  "Read a new Git remote name with PROMPT.
DEFAULT is preselected when non-nil.  When there are no remotes, `origin' is
used as the default.  Existing remote names are rejected."
  (let* ((root (ignore-errors (majutsu--toplevel-safe)))
         (payload (majutsu-remote-candidate-data root))
         (remotes (plist-get payload :candidates))
         (remote (majutsu-completing-read-payload
                  prompt payload nil 'any nil 'majutsu-remote-name-history
                  (or default (unless remotes "origin")) 'majutsu-remote nil
                  (or root default-directory))))
    (majutsu-remote--validate-new-name remote)
    (when (member remote remotes)
      (user-error "Remote already exists: %s" remote))
    remote))

(defun majutsu-read-remote-pattern (prompt &optional initial-input history default candidates)
  "Read one remote name pattern with PROMPT.
INITIAL-INPUT is inserted for editing.  HISTORY defaults to
`majutsu-remote-pattern-history'.  DEFAULT is preselected when non-nil.
CANDIDATES defaults to known Git remote names."
  (let ((payload (majutsu-remote-candidate-data default-directory)))
    (unless (plist-get payload :candidates)
      (setq payload (plist-put payload :candidates candidates)))
    (majutsu-completing-read-payload
     prompt payload
     nil 'any initial-input (or history 'majutsu-remote-pattern-history)
     default 'majutsu-remote nil default-directory)))

(defun majutsu-transient-read-remote-pattern (prompt initial-input _history)
  "Read a Git remote name pattern for a transient option."
  (majutsu-read-remote-pattern prompt initial-input nil initial-input))

(defun majutsu-read-remote-patterns (prompt &optional candidates default initial-input history)
  "Read remote name patterns with PROMPT.
CANDIDATES defaults to known Git remote names.  DEFAULT is preselected when
non-nil.  INITIAL-INPUT and HISTORY are forwarded to the minibuffer."
  (let ((payload (majutsu-remote-candidate-data default-directory)))
    (unless (plist-get payload :candidates)
      (setq payload (plist-put payload :candidates candidates)))
    (majutsu-completing-read-multiple-payload
     prompt payload
     nil nil initial-input (or history 'majutsu-remote-pattern-history)
     default 'majutsu-remote nil default-directory)))

(defun majutsu-transient-read-remote-patterns (prompt initial-input history)
  "Read Git remote name patterns for a repeat transient option."
  (majutsu-read-remote-patterns prompt nil nil initial-input history))

(provide 'majutsu-remote)
;;; majutsu-remote.el ends here
