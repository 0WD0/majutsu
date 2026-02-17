;;; majutsu-tag.el --- Tag commands for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library implements jj tag commands and integrates them
;; with Majutsu's transient UI.

;;; Code:

(require 'majutsu)

(require 'seq)
(require 'subr-x)

(declare-function majutsu-marginalia-prewarm-candidate-data
                  "majutsu-marginalia"
                  (category payload &optional revset directory))

(defvar-local majutsu-tag--list-all-remotes nil
  "Non-nil when the tag list includes remote tags.")

(defun majutsu--get-tag-names (&optional scope)
  "Return tag names for completion.

SCOPE controls what to return:

- nil or `local': local tag names (e.g. \"v1.0\")
- t or `remote': remote tag refs (e.g. \"v1.0@git\")"
  (let* ((scope (pcase scope
                  ((or 'nil 'local) 'local)
                  ((or 't 'remote) 'remote)
                  (_ (user-error "Unknown tag name scope: %S" scope))))
         (template (pcase scope
                     ('local
                      "if(!remote && present, name ++ \"\\n\", \"\")")
                     ('remote
                      "if(remote && present, name ++ \"@\" ++ remote ++ \"\\n\", \"\")")))
         (args (append '("tag" "list" "--quiet")
                       (and (eq scope 'remote) '("--all-remotes"))
                       (list "-T" template))))
    (delete-dups (majutsu-jj-lines args))))

(defun majutsu-tag-candidate-data (&optional directory)
  "Return completion payload for local tags in DIRECTORY.
The payload follows `(:candidates LIST :annotations HASH-TABLE)'."
  (let* ((default-directory (or directory default-directory))
         (tags (condition-case nil
                   (majutsu--get-tag-names 'local)
                 (error nil)))
         (annotations (make-hash-table :test #'equal)))
    (dolist (tag tags)
      (puthash tag "local tag" annotations))
    (list :candidates tags
          :annotations annotations)))

(defun majutsu-tag-parse-list-output (output)
  "Parse `jj tag list` OUTPUT into grouped entries.

Return a list of plists with keys:
- :name  tag name
- :lines rendered lines belonging to the tag"
  (let ((entries nil)
        current)
    (dolist (line (split-string (or output "") "\n" t))
      (if (string-match "^\\([^[:space:]:]+\\)\\(?: (conflicted)\\)?:" line)
          (progn
            (when current
              (push (list :name (car current) :lines (nreverse (cdr current))) entries))
            (setq current (cons (match-string 1 line) (list line))))
        (when current
          (setcdr current (cons line (cdr current))))))
    (when current
      (push (list :name (car current) :lines (nreverse (cdr current))) entries))
    (nreverse entries)))

(defun majutsu-tag--read-names (prompt)
  "Read tag names with PROMPT.
Allows entering both existing and new tag names."
  (let* ((payload (majutsu-tag-candidate-data))
         (candidates (plist-get payload :candidates)))
    (when (fboundp 'majutsu-marginalia-prewarm-candidate-data)
      (majutsu-marginalia-prewarm-candidate-data
       'majutsu-tag payload nil default-directory))
    (seq-filter (lambda (name) (not (string-empty-p name)))
                (majutsu-completing-read-multiple
                 prompt candidates nil nil nil nil nil 'majutsu-tag))))

(defun majutsu-tag--list-args ()
  "Return arguments for `jj tag list`."
  (append '("tag" "list" "--quiet")
          (and majutsu-tag--list-all-remotes '("--all-remotes"))))

(defun majutsu-tag--wash-list (_args)
  "Wash `jj tag list` output into tag sections."
  (let* ((entries (majutsu-tag-parse-list-output (buffer-string)))
         (inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (if (null entries)
        (magit-cancel-section)
      (dolist (entry entries)
        (let ((name (plist-get entry :name))
              (lines (plist-get entry :lines)))
          (magit-insert-section (jj-tag name t)
            (magit-insert-heading (car lines))
            (dolist (line (cdr lines))
              (insert line "\n")))))
      (insert "\n"))))

(defun majutsu-tag-list-refresh-buffer ()
  "Refresh the tag list buffer."
  (majutsu--assert-mode 'majutsu-tag-list-mode)
  (magit-insert-section (tag-list)
    (majutsu-jj-wash #'majutsu-tag--wash-list nil
      (majutsu-tag--list-args))))

(defvar-keymap majutsu-tag-list-mode-map
  :doc "Keymap for `majutsu-tag-list-mode'."
  :parent majutsu-mode-map)

(define-derived-mode majutsu-tag-list-mode majutsu-mode "Majutsu Tags"
  "Major mode for viewing jj tags."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

;;;###autoload
(defun majutsu-tag-list (&optional all-remotes)
  "List tags in a dedicated buffer.
With prefix ALL-REMOTES, include remote tags."
  (interactive "P")
  (majutsu-setup-buffer #'majutsu-tag-list-mode nil
    :buffer "*Majutsu Tags*"
    (majutsu-tag--list-all-remotes (and all-remotes t))))

;;;###autoload
(defun majutsu-tag-set (names revision &optional allow-move)
  "Create or update tag NAMES to point to REVISION.
When ALLOW-MOVE is non-nil, pass `--allow-move'."
  (interactive
   (let* ((default-revision (or (magit-section-value-if 'jj-commit) "@"))
          (names (majutsu-tag--read-names "Set tag(s)"))
          (revision (majutsu-read-string "Target revision" nil nil default-revision))
          (allow-move current-prefix-arg))
     (list names revision allow-move)))
  (when names
    (let ((args (append '("tag" "set")
                        (and allow-move '("--allow-move"))
                        (list "-r" revision)
                        names)))
      (when (zerop (apply #'majutsu-run-jj args))
        (message "Set tag(s) at %s: %s" revision (string-join names ", "))))))

;;;###autoload
(defun majutsu-tag-delete (names)
  "Delete tag NAMES.
NAMES are passed as jj string patterns."
  (interactive
   (list (majutsu-tag--read-names "Delete tag(s)/pattern(s)")))
  (when names
    (when (zerop (apply #'majutsu-run-jj (append '("tag" "delete") names)))
      (message "Deleted tag(s): %s" (string-join names ", ")))))

;;;###autoload
(defun majutsu-tag-move (names revision)
  "Move existing tag NAMES to REVISION.
This is a convenience wrapper around `jj tag set --allow-move'."
  (interactive
   (let* ((payload (majutsu-tag-candidate-data))
          (existing (plist-get payload :candidates))
          (default-revision (or (magit-section-value-if 'jj-commit) "@"))
          (_ (when (fboundp 'majutsu-marginalia-prewarm-candidate-data)
               (majutsu-marginalia-prewarm-candidate-data
                'majutsu-tag payload nil default-directory)))
          (names (seq-filter (lambda (name) (not (string-empty-p name)))
                             (majutsu-completing-read-multiple
                              "Move tag(s)" existing nil t nil nil nil 'majutsu-tag)))
          (revision (majutsu-read-string "Target revision" nil nil default-revision)))
     (list names revision)))
  (majutsu-tag-set names revision t))

(transient-define-prefix majutsu-tag ()
  "Internal transient for jj tag operations."
  :transient-non-suffix t
  ["Tag Operations"
   [
    ("l" "List tags" majutsu-tag-list
     :description "Show tag list")]
   [
    ("s" "Set tag(s)" majutsu-tag-set
     :description "Create/update tag(s)")
    ("m" "Move tag(s)" majutsu-tag-move
     :description "Move existing tags")]
   [
    ("d" "Delete tag(s)" majutsu-tag-delete
     :description "Delete tag(s)")]
   [("q" "Quit" transient-quit-one)]])

;;; _
(provide 'majutsu-tag)
;;; majutsu-tag.el ends here
