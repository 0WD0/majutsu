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
    (when (member remote remotes)
      (user-error "Remote already exists: %s" remote))
    remote))

(defun majutsu-read-remote-patterns (prompt &optional candidates default)
  "Read remote name patterns with PROMPT.
CANDIDATES defaults to known Git remote names.  DEFAULT is preselected when
non-nil."
  (let ((payload (majutsu-remote-candidate-data default-directory)))
    (unless (plist-get payload :candidates)
      (setq payload (plist-put payload :candidates candidates)))
    (majutsu-completing-read-multiple-payload
     prompt payload
     nil nil nil 'majutsu-remote-pattern-history
     default 'majutsu-remote nil default-directory)))

(provide 'majutsu-remote)
;;; majutsu-remote.el ends here
