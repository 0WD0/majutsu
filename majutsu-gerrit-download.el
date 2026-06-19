;;; majutsu-gerrit-download.el --- Download Gerrit patch sets for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fetch Gerrit patch-set refs into the colocated Git store and create a jj
;; working-copy child on top of the fetched patch set.

;;; Code:

(require 'majutsu-gerrit-data)
(require 'majutsu-gerrit-rest)
(require 'majutsu-jj)
(require 'majutsu-process)

(require 'cl-lib)
(require 'magit-section)
(require 'seq)
(require 'subr-x)

(declare-function majutsu-gerrit--selected-remote "majutsu-gerrit" (&optional args directory))
(defvar majutsu-gerrit-dashboard--remote)

(defcustom majutsu-gerrit-download-ref-prefix "majutsu/gerrit"
  "Remote bookmark namespace used for downloaded Gerrit patch sets."
  :type 'string
  :group 'majutsu-gerrit)

(defun majutsu-gerrit-download--change-at-point ()
  "Return the Gerrit change at point, or nil."
  (magit-section-value-if 'majutsu-gerrit-change))

(defun majutsu-gerrit-download--remote (&optional remote directory)
  "Return the Gerrit REMOTE name for DIRECTORY."
  (or remote
      (and (boundp 'majutsu-gerrit-dashboard--remote)
           majutsu-gerrit-dashboard--remote)
      (and (fboundp 'majutsu-gerrit--selected-remote)
           (majutsu-gerrit--selected-remote nil directory))))

(defun majutsu-gerrit-download--change-resource (change)
  "Return the best REST resource id for CHANGE."
  (or (majutsu-gerrit-change-id change)
      (majutsu-gerrit-change-number change)
      (majutsu-gerrit-change-change-id change)))

(defun majutsu-gerrit-download--current-revision (change)
  "Return CHANGE's current `majutsu-gerrit-revision', if present."
  (let ((current (majutsu-gerrit-change-current-revision change))
        (revisions (majutsu-gerrit-change-revisions change)))
    (or (and current (cdr (assoc current revisions)))
        (seq-find #'majutsu-gerrit-revision-current-p (mapcar #'cdr revisions))
        (car (sort (mapcar #'cdr revisions)
                   (lambda (a b)
                     (> (or (majutsu-gerrit-revision-number a) 0)
                        (or (majutsu-gerrit-revision-number b) 0))))))))

(defun majutsu-gerrit-download--ensure-current-revision (change &optional spec)
  "Return CHANGE's current revision, fetching detail through SPEC if needed."
  (or (majutsu-gerrit-download--current-revision change)
      (let* ((resource (majutsu-gerrit-download--change-resource change))
             (detailed (and resource
                            (majutsu-gerrit-change-from-alist
                             (majutsu-gerrit-rest-change-get
                              resource '("CURRENT_REVISION") spec)))))
        (and detailed (majutsu-gerrit-download--current-revision detailed)))
      (user-error "Gerrit change has no current patch-set ref")))

(defun majutsu-gerrit-download--bookmark-name (change revision)
  "Return local remote-bookmark name for CHANGE and REVISION."
  (format "%s/%s/%s"
          (string-remove-suffix "/" majutsu-gerrit-download-ref-prefix)
          (or (majutsu-gerrit-change-number change)
              (majutsu-gerrit-change-change-id change)
              (majutsu-gerrit-change-id change))
          (or (majutsu-gerrit-revision-number revision)
              (majutsu-gerrit-revision-id revision))))

(defun majutsu-gerrit-download--git-dir ()
  "Return the underlying Git directory for the current jj repository."
  (or (majutsu-jj-string "git" "root")
      (user-error "Current jj repository is not backed by Git")))

(defun majutsu-gerrit-download--remote-ref (remote bookmark)
  "Return the Git remote-tracking ref for REMOTE BOOKMARK."
  ;; `jj git import' only imports Git refs that `parse_git_ref' recognizes:
  ;; local branches, remote branches, and tags.  Gerrit patch sets are under
  ;; refs/changes, so fetch them into refs/remotes/<remote>/... first.
  (format "refs/remotes/%s/%s" remote bookmark))

(defun majutsu-gerrit-download--revset (remote bookmark)
  "Return jj revset selecting REMOTE BOOKMARK."
  (format "%s@%s" bookmark remote))

(defun majutsu-gerrit-download--fetch-refspec (source-ref remote bookmark)
  "Return forced Git fetch refspec from SOURCE-REF to REMOTE BOOKMARK."
  (format "+%s:%s" source-ref
          (majutsu-gerrit-download--remote-ref remote bookmark)))

(defun majutsu-gerrit-download--start-git-fetch (remote source-ref bookmark callback)
  "Fetch SOURCE-REF from REMOTE into BOOKMARK, then call CALLBACK."
  (let* ((refspec (majutsu-gerrit-download--fetch-refspec source-ref remote bookmark))
         (git-dir (majutsu-gerrit-download--git-dir))
         (process (majutsu-start-process "git" nil "--git-dir" git-dir
                                         "fetch" remote refspec)))
    (process-put process 'success-msg "Fetched Gerrit patch set")
    (process-put process 'error-prefix "git error")
    (process-put process 'finish-callback
                 (lambda (_process exit)
                   (when (zerop exit)
                     (funcall callback))))
    process))

(defun majutsu-gerrit-download--start-import (callback)
  "Import newly fetched Git refs into jj, then call CALLBACK."
  (let ((process (majutsu-start-jj
                  '("--config=git.abandon-unreachable-commits=false" "git" "import")
                  "Imported Gerrit patch set")))
    (process-put process 'finish-callback
                 (lambda (_process exit)
                   (when (zerop exit)
                     (funcall callback))))
    process))

(defun majutsu-gerrit-download--start-new (remote bookmark)
  "Create a jj working-copy child on top of REMOTE BOOKMARK."
  (majutsu-start-jj (list "new" (majutsu-gerrit-download--revset remote bookmark))
                    "Created working-copy child for Gerrit patch set"))

;;;###autoload
(defun majutsu-gerrit-download-change (&optional change remote directory)
  "Download Gerrit CHANGE and create a jj working-copy child.
Interactively, use the Gerrit change section at point."
  (interactive (list (majutsu-gerrit-download--change-at-point)))
  (unless change
    (user-error "No Gerrit change at point"))
  (let* ((directory (or directory majutsu--default-directory default-directory))
         (default-directory directory)
         (remote (majutsu-gerrit-download--remote remote directory))
         (spec (majutsu-gerrit-rest-current-spec remote directory))
         (revision (majutsu-gerrit-download--ensure-current-revision change spec))
         (source-ref (majutsu-gerrit-revision-ref revision))
         (bookmark (majutsu-gerrit-download--bookmark-name change revision)))
    (unless (and remote (not (string-empty-p remote)))
      (user-error "No Gerrit remote selected"))
    (unless (and source-ref (not (string-empty-p source-ref)))
      (user-error "Gerrit change has no current patch-set ref"))
    ;; `jj git fetch' intentionally only supports branch/tag patterns.  Gerrit
    ;; patch sets live under refs/changes, which `jj git import' will not scan
    ;; directly.  Fetch the exact patch-set ref into refs/remotes/<remote>/...,
    ;; i.e. a namespace that jj imports as a remote bookmark.
    (majutsu-gerrit-download--start-git-fetch
     remote source-ref bookmark
     (lambda ()
       (majutsu-gerrit-download--start-import
        (lambda ()
          (majutsu-gerrit-download--start-new remote bookmark)))))))

(provide 'majutsu-gerrit-download)

;;; majutsu-gerrit-download.el ends here
