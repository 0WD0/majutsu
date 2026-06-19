;;; majutsu-gerrit-download.el --- Download Gerrit patch sets for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Download a Gerrit change into the local jj repository.
;;
;; A Gerrit change created with `jj gerrit upload' carries the originating
;; jj change id in its Change-Id footer (=I<jj-change-id>6a6a6964=).  When
;; that change still exists locally, downloading shows its diff directly
;; instead of fetching the rewritten Gerrit commit (whose commit id differs
;; and whose jj `change-id' header is usually dropped by the server).
;;
;; Otherwise (someone else's change, or a non-jj Change-Id) we fetch the
;; patch-set ref into a jj-visible remote bookmark and create a child on it.

;;; Code:

(require 'majutsu-gerrit-data)
(require 'majutsu-gerrit-rest)
(require 'majutsu-diff)
(require 'majutsu-jj)
(require 'majutsu-process)

(require 'magit-section)
(require 'seq)
(require 'subr-x)

(declare-function majutsu-gerrit--selected-remote "majutsu-gerrit" (&optional args directory))
(defvar majutsu-gerrit-dashboard--remote)

(defcustom majutsu-gerrit-download-ref-prefix "majutsu/gerrit"
  "Remote bookmark namespace used for downloaded Gerrit patch sets."
  :type 'string
  :group 'majutsu-gerrit)

;;;; Local alignment

(defun majutsu-gerrit-download--local-change-id (change)
  "Return the local jj change id for CHANGE if it exists locally.

Recover the jj change id from CHANGE's Gerrit Change-Id and confirm jj
knows it.  Return nil when CHANGE was not uploaded from this repo."
  (when-let* ((gerrit-id (majutsu-gerrit-change-change-id change))
              (jj-id (majutsu-gerrit-data-gerrit-id-to-jj-change-id gerrit-id))
              (resolved (ignore-errors
                          (majutsu-jj-string "log" "--no-graph" "-r" jj-id
                                             "-T" "change_id"))))
    jj-id))

;;;; Remote fetch fallback

(defun majutsu-gerrit-download--remote (&optional remote directory)
  "Return the Gerrit REMOTE name for DIRECTORY."
  (or remote
      (and (boundp 'majutsu-gerrit-dashboard--remote)
           majutsu-gerrit-dashboard--remote)
      (and (fboundp 'majutsu-gerrit--selected-remote)
           (majutsu-gerrit--selected-remote nil directory))))

(defun majutsu-gerrit-download--current-revision (change)
  "Return CHANGE's current `majutsu-gerrit-revision', if present."
  (let ((current (majutsu-gerrit-change-current-revision change))
        (revisions (majutsu-gerrit-change-revisions change)))
    (or (and current (cdr (assoc current revisions)))
        (seq-find #'majutsu-gerrit-revision-current-p (mapcar #'cdr revisions)))))

(defun majutsu-gerrit-download--ensure-current-revision (change spec)
  "Return CHANGE's current revision, fetching detail through SPEC if needed."
  (or (majutsu-gerrit-download--current-revision change)
      (when-let* ((number (majutsu-gerrit-change-number change))
                  (detailed (majutsu-gerrit-change-from-alist
                             (majutsu-gerrit-rest-change-get
                              number '("CURRENT_REVISION") spec))))
        (majutsu-gerrit-download--current-revision detailed))
      (user-error "Gerrit change has no current patch-set ref")))

(defun majutsu-gerrit-download--bookmark (change revision)
  "Return the jj remote-bookmark name for CHANGE REVISION."
  (format "%s/%s/%s"
          (string-remove-suffix "/" majutsu-gerrit-download-ref-prefix)
          (majutsu-gerrit-change-number change)
          (majutsu-gerrit-revision-number revision)))

(defun majutsu-gerrit-download--git-dir ()
  "Return the underlying Git directory for the current jj repository."
  (or (majutsu-jj-string "git" "root")
      (user-error "Current jj repository is not backed by Git")))

(defun majutsu-gerrit-download--fetch-refspec (source-ref remote bookmark)
  "Return a forced Git refspec mapping SOURCE-REF to REMOTE BOOKMARK.

Gerrit patch sets live under refs/changes, which neither `jj git fetch'
nor `jj git import' will scan.  Fetch them into refs/remotes/<remote>/...,
the namespace jj imports as a remote bookmark."
  (format "+%s:refs/remotes/%s/%s" source-ref remote bookmark))

(defun majutsu-gerrit-download--fetch-and-checkout (remote source-ref bookmark)
  "Fetch SOURCE-REF into REMOTE BOOKMARK, import it, and check it out."
  (let ((git-dir (majutsu-gerrit-download--git-dir))
        (refspec (majutsu-gerrit-download--fetch-refspec source-ref remote bookmark)))
    (unless (zerop (majutsu-run-jj
                    "git" "--git-dir" git-dir "fetch" remote refspec))
      (user-error "Failed to fetch %s from %s" source-ref remote)))
  (unless (zerop (majutsu-run-jj "git" "import"))
    (user-error "Failed to import fetched Gerrit patch set"))
  (when (zerop (majutsu-run-jj "new" (format "%s@%s" bookmark remote)))
    (majutsu-refresh)
    (message "Downloaded Gerrit change as %s@%s" bookmark remote)))

;;;; Entry point

;;;###autoload
(defun majutsu-gerrit-download-change (&optional change directory)
  "Download Gerrit CHANGE into the local jj repository.

If CHANGE was uploaded from this repo, show the local change it came
from in a diff buffer.  Otherwise fetch its current patch set and create
a child on it.  Interactively, use the Gerrit change section at point."
  (interactive (list (magit-section-value-if 'majutsu-gerrit-change)))
  (unless change
    (user-error "No Gerrit change at point"))
  (let* ((directory (or directory majutsu--default-directory default-directory))
         (default-directory directory))
    (if-let* ((local-id (majutsu-gerrit-download--local-change-id change)))
        (majutsu-diff-revset local-id)
      (let* ((remote (majutsu-gerrit-download--remote nil directory))
             (spec (majutsu-gerrit-rest-current-spec remote directory))
             (revision (majutsu-gerrit-download--ensure-current-revision change spec))
             (source-ref (majutsu-gerrit-revision-ref revision)))
        (unless (and remote (not (string-empty-p remote)))
          (user-error "No Gerrit remote selected"))
        (unless (and source-ref (not (string-empty-p source-ref)))
          (user-error "Gerrit change has no current patch-set ref"))
        (majutsu-gerrit-download--fetch-and-checkout
         remote source-ref
         (majutsu-gerrit-download--bookmark change revision))))))

(provide 'majutsu-gerrit-download)

;;; majutsu-gerrit-download.el ends here
