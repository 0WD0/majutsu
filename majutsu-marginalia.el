;;; majutsu-marginalia.el --- Marginalia integration for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: convenience, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Optional Marginalia integration for Majutsu completion categories.
;; This file is only loaded after `marginalia' is available and does not make
;; Marginalia a hard dependency of Majutsu.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar marginalia--metadata)
(defvar marginalia-annotators)
(defvar marginalia-separator)

(declare-function marginalia--orig-completion-metadata-get "marginalia"
                  (metadata prop))
(declare-function marginalia-annotate-file "marginalia" (cand))
(declare-function majutsu-toplevel "majutsu-jj" (&optional directory))
(declare-function majutsu-workspace-current-name "majutsu-workspace" (&optional directory))

(defvar majutsu-marginalia--payload-cache (make-hash-table :test #'equal)
  "Cache of prewarmed candidate payloads keyed by category and directory.")

(defun majutsu-marginalia--normalize-directory (directory)
  "Return DIRECTORY as a normalized directory name, if possible."
  (when directory
    (condition-case nil
        (file-name-as-directory (expand-file-name directory))
      (error directory))))

(defun majutsu-marginalia--cache-key (category &optional directory)
  "Return the payload-cache key for CATEGORY in DIRECTORY."
  (list category (majutsu-marginalia--normalize-directory directory)))

(defun majutsu-marginalia-prewarm-candidate-data (category payload &optional _context directory)
  "Cache completion PAYLOAD for Marginalia CATEGORY.

CONTEXT is accepted for API compatibility with Majutsu callers.  DIRECTORY is
used to scope cached payloads to the current repository context."
  (puthash (majutsu-marginalia--cache-key category directory)
           payload majutsu-marginalia--payload-cache)
  (puthash category payload majutsu-marginalia--payload-cache))

(defun majutsu-marginalia--cached-payload (category)
  "Return cached payload for CATEGORY in the current minibuffer context."
  (or (gethash (majutsu-marginalia--cache-key
                category
                (majutsu-marginalia--minibuffer-default-directory))
               majutsu-marginalia--payload-cache)
      (gethash category majutsu-marginalia--payload-cache)))

(defun majutsu-marginalia--cached-entry (category cand)
  "Return cached structured entry for CAND in CATEGORY, if any."
  (when-let* ((payload (majutsu-marginalia--cached-payload category))
              (entries (plist-get payload :entries)))
    (and (hash-table-p entries)
         (gethash cand entries))))

(defun majutsu-marginalia--cached-annotation (category cand)
  "Return cached annotation string for CAND in CATEGORY, if any."
  (when-let* ((payload (majutsu-marginalia--cached-payload category))
              (annotations (plist-get payload :annotations))
              (annotation (and (hash-table-p annotations)
                               (gethash cand annotations))))
    (concat marginalia-separator annotation)))

(defun majutsu-marginalia--orig-annotation (cand)
  "Return the original completion metadata annotation for CAND, if any."
  (when-let* ((fun (marginalia--orig-completion-metadata-get
                    marginalia--metadata 'annotation-function)))
    (funcall fun cand)))

(defun majutsu-marginalia--field (text &optional face)
  "Return TEXT formatted as one Marginalia annotation field."
  (when (and text (not (string-empty-p text)))
    (if face
        (propertize (format "%s" text) 'face face)
      (format "%s" text))))

(defun majutsu-marginalia--column (text width &optional face)
  "Return TEXT as a fixed-width annotation column.
WIDTH is the target display width and FACE is applied to the whole column."
  (let* ((raw (format "%s" (or text "")))
         (truncated (truncate-string-to-width raw width 0 ?\s "…"))
         (padding (max 0 (- width (string-width truncated))))
         (value (concat truncated (make-string padding ?\s))))
    (if face
        (propertize value 'face face)
      value)))

(defun majutsu-marginalia--annotation (&rest fields)
  "Join FIELDS into one Marginalia annotation string.
Insert Marginalia's alignment marker before the first separator."
  (let ((fields (delq nil fields)))
    (when fields
      (concat (propertize " " 'marginalia--align t)
              marginalia-separator
              (let ((result (car fields)))
                (dolist (field (cdr fields))
                  (setq result (concat result marginalia-separator field)))
                result)))))

(defun majutsu-marginalia--label (label &optional face)
  "Format LABEL as a simple Marginalia annotation string."
  (majutsu-marginalia--annotation
   (majutsu-marginalia--field label (or face 'marginalia-type))))

(defun majutsu-marginalia--bookmark-remote-field (label remotes face)
  "Format LABEL for REMOTES using FACE."
  (when remotes
    (let* ((remotes (delete-dups (copy-sequence remotes)))
           (count (length remotes)))
      (majutsu-marginalia--field
       (if (= count 1)
           (format "%s@%s" label (car remotes))
         (format "%s@%s%s"
                 label
                 (string-join (seq-take remotes 2) ",")
                 (if (> count 2)
                     (format ",… (%d)" count)
                   "")))
       face))))

(defun majutsu-marginalia--minibuffer-default-directory ()
  "Return the minibuffer source buffer's `default-directory'."
  (or (when-let* ((win (active-minibuffer-window)))
        (with-current-buffer (window-buffer win)
          default-directory))
      default-directory))

(defun majutsu-marginalia--repo-root ()
  "Return the current repository root, if available."
  (ignore-errors
    (majutsu-toplevel (majutsu-marginalia--minibuffer-default-directory))))

(defun majutsu-marginalia-annotate-revision (cand)
  "Annotate revision candidate CAND."
  (or (majutsu-marginalia--orig-annotation cand)
      (majutsu-marginalia--label "revset" 'marginalia-key)))

(defun majutsu-marginalia-annotate-bookmark (cand)
  "Annotate bookmark candidate CAND."
  (if-let* ((entry (majutsu-marginalia--cached-entry 'majutsu-bookmark cand)))
      (majutsu-marginalia--annotation
       (majutsu-marginalia--column "bookmark" 9 'marginalia-key)
       (majutsu-marginalia--column
        (if (plist-get entry :local) "local" "remote only")
        11 'marginalia-type)
       (majutsu-marginalia--column
        (and (plist-get entry :synced) "synced")
        8 'success)
       (majutsu-marginalia--column
        (and (plist-get entry :conflict) "conflicted")
        10 'warning)
       (majutsu-marginalia--column
        (majutsu-marginalia--bookmark-remote-field
         "tracked" (plist-get entry :tracked-remotes) nil)
        22 'success)
       (majutsu-marginalia--column
        (majutsu-marginalia--bookmark-remote-field
         "untracked" (plist-get entry :untracked-remotes) nil)
        22 'marginalia-documentation))
    (or (majutsu-marginalia--cached-annotation 'majutsu-bookmark cand)
        (majutsu-marginalia--label "bookmark" 'marginalia-key))))

(defun majutsu-marginalia-annotate-tag (cand)
  "Annotate tag candidate CAND."
  (if-let* ((entry (majutsu-marginalia--cached-entry 'majutsu-tag cand)))
      (majutsu-marginalia--annotation
       (majutsu-marginalia--column "tag" 9 'marginalia-key)
       (majutsu-marginalia--column
        (if (plist-get entry :local) "local" "remote only")
        11 'marginalia-type)
       (majutsu-marginalia--column
        (and (plist-get entry :synced) "synced")
        8 'success)
       (majutsu-marginalia--column
        (and (plist-get entry :conflict) "conflicted")
        10 'warning)
       (majutsu-marginalia--column
        (majutsu-marginalia--bookmark-remote-field
         "tracked" (plist-get entry :tracked-remotes) nil)
        22 'success)
       (majutsu-marginalia--column
        (majutsu-marginalia--bookmark-remote-field
         "untracked" (plist-get entry :untracked-remotes) nil)
        22 'marginalia-documentation))
    (or (majutsu-marginalia--cached-annotation 'majutsu-tag cand)
        (majutsu-marginalia--label "tag" 'marginalia-key))))

(defun majutsu-marginalia-annotate-remote (cand)
  "Annotate Git remote candidate CAND."
  (if-let* ((entry (majutsu-marginalia--cached-entry 'majutsu-remote cand)))
      (let ((fetch (plist-get entry :fetch-url))
            (push (plist-get entry :push-url)))
        (majutsu-marginalia--annotation
         (majutsu-marginalia--column "git remote" 10 'marginalia-key)
         (majutsu-marginalia--column fetch 28 'marginalia-file-name)
         (majutsu-marginalia--field
          (and push (not (equal push fetch))
               (format "push:%s" push))
          'marginalia-documentation)))
    (majutsu-marginalia--label "git remote" 'marginalia-key)))

(defun majutsu-marginalia-annotate-workspace (cand)
  "Annotate workspace candidate CAND."
  (if-let* ((entry (majutsu-marginalia--cached-entry 'majutsu-workspace cand)))
      (majutsu-marginalia--annotation
       (majutsu-marginalia--column
        (if (plist-get entry :current) "current" "workspace")
        10 (if (plist-get entry :current) 'success 'marginalia-key))
       (majutsu-marginalia--column
        (when-let* ((root (plist-get entry :root)))
          (abbreviate-file-name root))
        28 'marginalia-file-name)
       (majutsu-marginalia--column
        (plist-get entry :change-id)
        8 'marginalia-number)
       (majutsu-marginalia--field
        (when-let* ((desc (plist-get entry :desc))
                    ((not (string-empty-p desc))))
          desc)
        'marginalia-documentation))
    (let ((current (ignore-errors
                     (majutsu-workspace-current-name
                      (majutsu-marginalia--minibuffer-default-directory)))))
      (if (and current (equal cand current))
          (majutsu-marginalia--label "current workspace" 'success)
        (majutsu-marginalia--label "workspace" 'marginalia-key)))))

(defun majutsu-marginalia--file-status-face (status)
  "Return a face for file STATUS."
  (pcase status
    ((or "added" "copied") 'success)
    ("deleted" 'error)
    ("renamed" 'warning)
    ("modified" 'marginalia-key)
    (_ 'marginalia-documentation)))

(defun majutsu-marginalia-annotate-file (cand)
  "Annotate repo-relative file candidate CAND."
  (if-let* ((entry (majutsu-marginalia--cached-entry 'majutsu-file cand)))
      (let ((status (plist-get entry :status))
            (file-type (or (plist-get entry :file-type) "file")))
        (majutsu-marginalia--annotation
         (majutsu-marginalia--column
          (if (plist-get entry :conflict) "conflict" file-type)
          10 (if (plist-get entry :conflict) 'warning 'marginalia-key))
         (majutsu-marginalia--column
          status 10 (majutsu-marginalia--file-status-face status))
         (majutsu-marginalia--column
          (and (plist-get entry :executable) "executable")
          10 'marginalia-type)))
    (if-let* ((root (majutsu-marginalia--repo-root))
              (path (expand-file-name cand root)))
        (marginalia-annotate-file path)
      (majutsu-marginalia--label "file" 'marginalia-key))))

(defun majutsu-marginalia--set-annotator (category function)
  "Register FUNCTION as the primary Marginalia annotator for CATEGORY."
  (setf (alist-get category marginalia-annotators)
        (list function 'none)))

(with-eval-after-load 'marginalia
  (majutsu-marginalia--set-annotator 'majutsu-revision #'majutsu-marginalia-annotate-revision)
  (majutsu-marginalia--set-annotator 'majutsu-bookmark #'majutsu-marginalia-annotate-bookmark)
  (majutsu-marginalia--set-annotator 'majutsu-tag #'majutsu-marginalia-annotate-tag)
  (majutsu-marginalia--set-annotator 'majutsu-remote #'majutsu-marginalia-annotate-remote)
  (majutsu-marginalia--set-annotator 'majutsu-workspace #'majutsu-marginalia-annotate-workspace)
  (majutsu-marginalia--set-annotator 'majutsu-file #'majutsu-marginalia-annotate-file))

(provide 'majutsu-marginalia)
;;; majutsu-marginalia.el ends here
