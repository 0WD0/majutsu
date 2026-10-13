;;; majutsu-org.el --- Org links to Majutsu buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc, hypermedia
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library defines the Org link types `majutsu' and `majutsu-rev',
;; in the spirit of `orgit' for Magit:
;;
;;   majutsu:/path/to/repo/            opens the repository's log
;;   majutsu-rev:/path/to/repo/::REV   opens the revision
;;
;; Use `org-store-link' inside a Majutsu buffer to store a link and
;; `org-insert-link' to insert it.  When the region selects several
;; revisions, one `majutsu-rev' link is stored for each.  Loading
;; `majutsu' registers this integration once Org becomes available.

;;; Code:

(require 'majutsu-mode)
(require 'org)

(declare-function majutsu-log "majutsu-log" (&optional directory))
(declare-function majutsu-diff-revset "majutsu-diff"
                  (revset &optional args range filesets))

(defvar majutsu-org--store-revision-only nil
  "When non-nil, offer only the `majutsu-rev' store function.
Bound while storing one link per revision in an active region.")

(defun majutsu-org--root ()
  "Return the current repository root as an absolute directory name."
  (when-let* ((root (or (majutsu--buffer-root)
                        (majutsu-toplevel))))
    (file-name-as-directory (expand-file-name root))))

(defun majutsu-org--store-revision (root revision)
  "Store a `majutsu-rev' link to REVISION in ROOT."
  (let ((revision (substring-no-properties revision)))
    (org-link-store-props
     :type "majutsu-rev"
     :link (format "majutsu-rev:%s::%s" root revision)
     :description (format "%s::%s" (abbreviate-file-name root) revision))))

(defun majutsu-org-store-revision ()
  "Store a `majutsu-rev' link to the revision at point."
  (when-let* ((revision (and (derived-mode-p 'majutsu-mode)
                             (majutsu-revision-at-point)))
              (root (majutsu-org--root)))
    (majutsu-org--store-revision root revision)))

(defun majutsu-org-store-repository ()
  "Store a `majutsu' link to the current repository."
  (when (and (derived-mode-p 'majutsu-mode)
             (not majutsu-org--store-revision-only))
    (when-let* ((root (majutsu-org--root)))
      (org-link-store-props
       :type "majutsu"
       :link (format "majutsu:%s" root)
       :description (format "%s (majutsu)" (abbreviate-file-name root))))))

(defun majutsu-org-store-link (&optional _arg)
  "Store Org links to the selected or current Majutsu revisions.

With several revisions selected in the region, store one
`majutsu-rev' link per revision.  Otherwise behave like
`org-store-link'; when both the repository and a revision are
available, Org asks which link to store, preferring the revision."
  (interactive "P")
  (if-let* ((sections (magit-region-sections 'jj-commit)))
      (save-excursion
        (let ((majutsu-org--store-revision-only t))
          (dolist (section sections)
            (goto-char (oref section start))
            (set-mark (point))
            (activate-mark)
            (call-interactively #'org-store-link)))
        (deactivate-mark))
    (call-interactively #'org-store-link)))

(defun majutsu-org-open-repository (path)
  "Open the Majutsu log for the repository at PATH."
  (majutsu-log (file-name-as-directory (expand-file-name path))))

(defun majutsu-org-open-revision (path)
  "Open the revision encoded as REPO::REV in PATH."
  (pcase-let* ((`(,repository ,revision) (split-string path "::"))
               (default-directory (file-name-as-directory
                                   (expand-file-name repository))))
    (majutsu-diff-revset revision)))

(defun majutsu-org-complete-repository (&optional _arg)
  "Complete a `majutsu' link by reading a repository directory."
  (concat "majutsu:"
          (file-name-as-directory
           (expand-file-name
            (read-directory-name "Repository: " (majutsu-org--root))))))

(defun majutsu-org-complete-revision (&optional _arg)
  "Complete a `majutsu-rev' link by reading a repository and revision."
  (let* ((root (or (majutsu-org--root)
                   (file-name-as-directory
                    (expand-file-name (read-directory-name "Repository: ")))))
         (default-directory root))
    (format "majutsu-rev:%s::%s" root (majutsu-read-revision "Revision"))))

(defun majutsu-org-setup ()
  "Register the Majutsu Org link types and the `org-store-link' remap."
  (org-link-set-parameters "majutsu-rev"
                           :store #'majutsu-org-store-revision
                           :follow #'majutsu-org-open-revision
                           :complete #'majutsu-org-complete-revision)
  (org-link-set-parameters "majutsu"
                           :store #'majutsu-org-store-repository
                           :follow #'majutsu-org-open-repository
                           :complete #'majutsu-org-complete-repository)
  (keymap-set majutsu-mode-map "<remap> <org-store-link>" #'majutsu-org-store-link))

(majutsu-org-setup)

(provide 'majutsu-org)
;;; majutsu-org.el ends here
