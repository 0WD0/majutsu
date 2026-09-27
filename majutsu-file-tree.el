;;; majutsu-file-tree.el --- Repository path trees  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Group repository-relative paths into collapsible Magit sections.  Callers
;; supply their own records and leaf renderers; this library neither queries
;; jj nor assigns diff, conflict, selection, or editor semantics to entries.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'subr-x)

(cl-defstruct majutsu-file-tree--node
  path children entry)

(defun majutsu-file-tree-build (entries path-function)
  "Group ENTRIES by repository-relative paths, preserving input order.
PATH-FUNCTION receives each non-nil entry and returns its complete file path.
A nil path leaves the entry ungrouped at the root, for display-only records
whose real path is unavailable.  Entries are retained without copying them."
  (let ((root (make-majutsu-file-tree--node))
        (directories (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (let ((path (funcall path-function entry))
            (parent root)
            (start 0)
            separator)
        (while (and path (setq separator (string-search "/" path start)))
          (setq start (1+ separator))
          (let ((prefix (substring path 0 start)))
            (setq parent
                  (or (gethash prefix directories)
                      (let ((node (make-majutsu-file-tree--node :path prefix)))
                        (puthash prefix node directories)
                        (push node (majutsu-file-tree--node-children parent))
                        node)))))
        (push (make-majutsu-file-tree--node :path path :entry entry)
              (majutsu-file-tree--node-children parent))))
    (setf (majutsu-file-tree--node-children root)
          (nreverse (majutsu-file-tree--node-children root)))
    (maphash (lambda (_path node)
               (setf (majutsu-file-tree--node-children node)
                     (nreverse (majutsu-file-tree--node-children node))))
             directories)
    root))

(defun majutsu-file-tree--compact-directory (node)
  "Return the last directory in NODE's single-child directory chain."
  (while (let ((children (majutsu-file-tree--node-children node)))
           (and children
                (null (cdr children))
                (not (majutsu-file-tree--node-entry (car children)))))
    (setq node (car (majutsu-file-tree--node-children node))))
  node)

(defun majutsu-file-tree-escape-label (label)
  "Keep LABEL on one display line without changing any path identity."
  (if (string-match-p "[\n\r\t]" label)
      (replace-regexp-in-string
       "[\n\r\t]" (lambda (character)
                     (pcase character
                       ("\n" "\\n")
                       ("\r" "\\r")
                       ("\t" "\\t")))
       label nil t)
    label))

(defun majutsu-file-tree-label-width (tree label-function)
  "Return the maximum leaf label width in TREE, including its indentation.
LABEL-FUNCTION receives an entry and returns its already formatted label.
Directory compaction and indentation match `majutsu-file-tree-insert'."
  (cl-labels
      ((width (node depth)
         (let ((maximum 0))
           (dolist (child (majutsu-file-tree--node-children node))
             (setq maximum
                   (max maximum
                        (if-let* ((entry (majutsu-file-tree--node-entry child)))
                            (+ (* 4 depth) 4
                               (string-width (funcall label-function entry)))
                          (width (majutsu-file-tree--compact-directory child)
                                 (1+ depth))))))
           maximum)))
    (width tree 0)))

(defun majutsu-file-tree-insert (tree insert-file &optional directory-face)
  "Insert TREE, using INSERT-FILE to render its leaf entries.
INSERT-FILE receives ENTRY, HEADING-PREFIX, and BODY-PREFIX.  It owns the leaf
section and its contents; navigable files use a `jj-file' section whose value
is the unmodified repository-relative path.

Directories use `jj-directory' sections with complete paths ending in /.
Single-child directory chains share one heading.  DIRECTORY-FACE defaults
to `magit-section-heading'.  The caller supplies the enclosing root section."
  (cl-labels
      ((visit (node prefix)
         (let ((children (majutsu-file-tree--node-children node))
               (parent-length (length (majutsu-file-tree--node-path node))))
           (while children
             (let* ((child (pop children))
                    (last (null children))
                    (heading-prefix (concat prefix (if last "└── " "├── ")))
                    (body-prefix (concat prefix (if last "    " "│   "))))
               (if-let* ((entry (majutsu-file-tree--node-entry child)))
                   (funcall insert-file entry heading-prefix body-prefix)
                 (let* ((directory (majutsu-file-tree--compact-directory child))
                        (path (majutsu-file-tree--node-path directory))
                        (label (majutsu-file-tree-escape-label
                                (substring path parent-length))))
                   (magit-insert-section (jj-directory path)
                     (magit-insert-heading
                       (propertize (concat heading-prefix label "\n")
                                   'font-lock-face
                                   (or directory-face 'magit-section-heading)))
                     (visit directory body-prefix)))))))))
    (visit tree "")))

(defun majutsu-file-tree-files (section)
  "Return file paths under SECTION in tree order, including hidden files.
Only `jj-file' section values are collected; no filesystem lookup is made."
  (let (files)
    (when section
      (magit-map-sections
       (lambda (child)
         (when (magit-section-match 'jj-file child)
           (push (oref child value) files)))
       section))
    (nreverse files)))

(provide 'majutsu-file-tree)
;;; majutsu-file-tree.el ends here
