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
  "Cache of prewarmed candidate payloads keyed by completion category.")

(defun majutsu-marginalia-prewarm-candidate-data (category payload &optional _revset _directory)
  "Cache completion PAYLOAD for Marginalia CATEGORY.

REVSET and DIRECTORY are accepted for API compatibility with Majutsu callers,
but are currently not part of the cache key.  The last payload per CATEGORY is
used while the minibuffer is active."
  (puthash category payload majutsu-marginalia--payload-cache))

(defun majutsu-marginalia--cached-payload (category)
  "Return cached payload for CATEGORY, if any."
  (gethash category majutsu-marginalia--payload-cache))

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

(defun majutsu-marginalia--label (label &optional face)
  "Format LABEL as a simple Marginalia annotation string."
  (concat marginalia-separator
          (propertize label 'face (or face 'marginalia-type))))

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
  (or (majutsu-marginalia--cached-annotation 'majutsu-bookmark cand)
      (majutsu-marginalia--label "bookmark" 'marginalia-key)))

(defun majutsu-marginalia-annotate-tag (cand)
  "Annotate tag candidate CAND."
  (or (majutsu-marginalia--cached-annotation 'majutsu-tag cand)
      (majutsu-marginalia--label "tag" 'marginalia-key)))

(defun majutsu-marginalia-annotate-remote (_cand)
  "Annotate Git remote candidate _CAND."
  (majutsu-marginalia--label "git remote" 'marginalia-key))

(defun majutsu-marginalia-annotate-workspace (cand)
  "Annotate workspace candidate CAND."
  (let ((current (ignore-errors
                   (majutsu-workspace-current-name
                    (majutsu-marginalia--minibuffer-default-directory)))))
    (if (and current (equal cand current))
        (majutsu-marginalia--label "current workspace" 'success)
      (majutsu-marginalia--label "workspace" 'marginalia-key))))

(defun majutsu-marginalia-annotate-file (cand)
  "Annotate repo-relative file candidate CAND using file attributes.

If CAND does not resolve inside a repository, fall back to a simple file label."
  (if-let* ((root (majutsu-marginalia--repo-root))
            (path (expand-file-name cand root)))
      (marginalia-annotate-file path)
    (majutsu-marginalia--label "file" 'marginalia-key)))

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
