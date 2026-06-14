;;; majutsu-ref.el --- Shared CommitRef helpers for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shared helpers for bookmark/tag data modeled as jj CommitRef values.
;; This module only handles machine-readable completion/name queries and
;; candidate payload aggregation; higher-level readers, list buffers, and
;; command UIs stay in the domain modules.

;;; Code:

(require 'majutsu-base)
(require 'majutsu-completion)
(require 'majutsu-jj)
(require 'majutsu-template)

(require 'subr-x)

;;; CommitRef completion transport

(defconst majutsu-ref--completion-field-separator (string 31)
  "Separator inserted between structured CommitRef completion fields.")

(defun majutsu-ref--command (kind)
  "Return jj subcommand string for ref KIND."
  (pcase kind
    ('bookmark "bookmark")
    ('tag "tag")
    (_ (user-error "Unknown ref kind: %S" kind))))

(defun majutsu-ref--category (kind)
  "Return completion category symbol for ref KIND."
  (pcase kind
    ('bookmark 'majutsu-bookmark)
    ('tag 'majutsu-tag)
    (_ (user-error "Unknown ref kind: %S" kind))))

(defun majutsu-ref--normalize-scope (scope)
  "Normalize ref-name query SCOPE."
  (pcase scope
    ((or 'nil 'local) 'local)
    ((or 't 'remote) 'remote)
    ('remote-tracked 'remote-tracked)
    ('remote-untracked 'remote-untracked)
    (_ (user-error "Unknown ref name scope: %S" scope))))

(defun majutsu-ref--primary-local-name-line-form ()
  "Return a template form for one local primary ref-name line."
  '[:if [:and [:primary :present]
              [:not [:primary :remote]]]
       [[:primary :name] "\n"]
     ""])

(defun majutsu-ref--primary-remote-name-line-form ()
  "Return a template form for one remote primary ref-name line."
  '[:if [:and [:primary :present]
              [:primary :remote]]
       [[:primary :name] "@" [:primary :remote] "\n"]
     ""])

(defun majutsu-ref--tracked-remote-name-line-form ()
  "Return a template form for one tracked remote ref-name line."
  '[:if [:and [:present] [:remote]]
       [[:name] "@" [:remote] "\n"]
     ""])

(defun majutsu-ref--primary-completion-line-form ()
  "Return a template form for one structured primary completion line."
  `[:concat
    [:join ,majutsu-ref--completion-field-separator
           [:primary :name]
           [:if [:primary :remote] [:primary :remote] ""]
           [:if [:primary :conflict] "t" ""]
           [:if [:primary :present] "t" ""]
           [:if [:primary :tracked] "t" ""]
           [:if [:primary :synced] "t" ""]]
    "\n"])

(defun majutsu-ref--tracked-completion-line-form ()
  "Return a template form for one structured tracked-ref completion line."
  `[:concat
    [:join ,majutsu-ref--completion-field-separator
           [:name]
           [:if [:remote] [:remote] ""]
           [:if [:conflict] "t" ""]
           [:if [:present] "t" ""]
           [:if [:tracked] "t" ""]
           [:if [:synced] "t" ""]]
    "\n"])

(defun majutsu-ref--names-template-form (scope)
  "Return a RefListItem name template form for SCOPE."
  (pcase (majutsu-ref--normalize-scope scope)
    ('local
     (majutsu-ref--primary-local-name-line-form))
    ('remote
     `[:concat
       ,(majutsu-ref--primary-remote-name-line-form)
       [:map-join [:str ""] [:tracked_refs] ref
                  ,(majutsu-ref--tracked-remote-name-line-form)]])
    ('remote-tracked
     `[:map-join [:str ""] [:tracked_refs] ref
       ,(majutsu-ref--tracked-remote-name-line-form)])
    ('remote-untracked
     (majutsu-ref--primary-remote-name-line-form))))

(defconst majutsu-ref--names-templates
  (mapcar (lambda (scope)
            (cons scope
                  (majutsu-template-compile
                   (majutsu-ref--names-template-form scope)
                   'RefListItem)))
          '(local remote remote-tracked remote-untracked))
  "Compiled RefListItem name templates keyed by normalized scope.")

(defun majutsu-ref--names-template (scope)
  "Return compiled RefListItem name template for SCOPE."
  (alist-get (majutsu-ref--normalize-scope scope)
             majutsu-ref--names-templates))

(defconst majutsu-ref--completion-template
  (majutsu-template-compile
   `[:concat
     ,(majutsu-ref--primary-completion-line-form)
     [:map-join [:str ""] [:tracked_refs] ref
                ,(majutsu-ref--tracked-completion-line-form)]]
   'RefListItem)
  "Template used to collect structured CommitRef completion metadata.
This uses `join' rather than `separate' because empty fields are meaningful
in the machine transport format.")

(defun majutsu-ref-names (kind &optional scope)
  "Return ref names for KIND filtered by SCOPE.
KIND is `bookmark' or `tag'.  SCOPE is one of `local', `remote',
`remote-tracked', or `remote-untracked'."
  (let* ((scope (majutsu-ref--normalize-scope scope))
         (args (append (list (majutsu-ref--command kind) "list" "--quiet")
                       (pcase scope
                         ((or 'remote 'remote-untracked) '("--all-remotes"))
                         ('remote-tracked '("--tracked"))
                         (_ nil))
                       (list "-T" (majutsu-ref--names-template scope)))))
    (delete-dups (apply #'majutsu-jj-lines args))))

(defun majutsu-ref--split-completion-fields (value)
  "Split structured CommitRef completion VALUE into fields."
  (majutsu--split-fields value majutsu-ref--completion-field-separator))

(defun majutsu-ref--parse-completion-line (line)
  "Parse one structured CommitRef completion LINE into a plist."
  (let* ((fields (majutsu-ref--split-completion-fields (or line "")))
         (name (nth 0 fields))
         (remote (nth 1 fields)))
    (when (and (stringp name) (not (string-empty-p name)))
      (list :name name
            :remote (unless (string-empty-p remote) remote)
            :conflict (majutsu--field-bool-p (nth 2 fields))
            :present (majutsu--field-bool-p (nth 3 fields))
            :tracked (majutsu--field-bool-p (nth 4 fields))
            :synced (majutsu--field-bool-p (nth 5 fields))))))

(defun majutsu-ref-completion-entries (kind &optional directory)
  "Return structured completion entries for ref KIND in DIRECTORY."
  (let ((default-directory (or directory default-directory))
        entries)
    (dolist (line (majutsu-jj-lines (majutsu-ref--command kind) "list"
                                    "--quiet" "--all-remotes"
                                    "-T" majutsu-ref--completion-template))
      (when-let* ((entry (majutsu-ref--parse-completion-line line)))
        (push entry entries)))
    (nreverse entries)))

(defun majutsu-ref--completion-remote-field (label remotes face)
  "Format LABEL for REMOTES using FACE."
  (when remotes
    (let* ((remotes (delete-dups (copy-sequence remotes)))
           (count (length remotes)))
      (majutsu-completion-field
       (if (= count 1)
           (format "%s@%s" label (car remotes))
         (format "%s@%s%s"
                 label
                 (string-join (seq-take remotes 2) ",")
                 (if (> count 2)
                     (format ",… (%d)" count)
                   "")))
       face))))

(defun majutsu-ref--completion-suffix (kind entry)
  "Return aligned completion suffix for ref KIND and ENTRY."
  (majutsu-completion-annotation
   (majutsu-completion-column (majutsu-ref--command kind) 9 'majutsu-completion-key)
   (majutsu-completion-column
    (if (plist-get entry :local) "local" "remote only")
    11 'majutsu-completion-type)
   (majutsu-completion-column
    (and (plist-get entry :synced) "synced")
    8 'success)
   (majutsu-completion-column
    (and (plist-get entry :conflict) "conflicted")
    10 'warning)
   (majutsu-completion-column
    (majutsu-ref--completion-remote-field
     "tracked" (plist-get entry :tracked-remotes) nil)
    22 'success)
   (majutsu-completion-column
    (majutsu-ref--completion-remote-field
     "untracked" (plist-get entry :untracked-remotes) nil)
    22 'majutsu-completion-documentation)))

(defun majutsu-ref--completion-suffix-function (kind entries)
  "Return candidate suffix function for ref KIND using ENTRIES."
  (majutsu-completion-entry-suffix-function
   entries
   (lambda (entry)
     (majutsu-ref--completion-suffix kind entry))))

(defun majutsu-ref-candidate-data (kind &optional candidates directory)
  "Return completion payload for ref KIND in DIRECTORY.
When CANDIDATES is non-nil, use it instead of derived local ref candidates."
  (let ((default-directory (or directory default-directory))
        (entries (make-hash-table :test #'equal))
        local-candidates)
    (condition-case _
        (dolist (row (majutsu-ref-completion-entries kind default-directory))
          (let* ((name (plist-get row :name))
                 (remote (plist-get row :remote))
                 (entry (or (gethash name entries)
                            (list :name name
                                  :tracked-remotes nil
                                  :untracked-remotes nil))))
            (when (plist-get row :conflict)
              (setq entry (plist-put entry :conflict t)))
            (if remote
                (setq entry
                      (plist-put entry
                                 (if (plist-get row :tracked)
                                     :tracked-remotes
                                   :untracked-remotes)
                                 (majutsu--append-unique
                                  (plist-get entry
                                             (if (plist-get row :tracked)
                                                 :tracked-remotes
                                               :untracked-remotes))
                                  remote)))
              (setq entry (plist-put entry :local t))
              (when (plist-get row :synced)
                (setq entry (plist-put entry :synced t)))
              (setq local-candidates
                    (majutsu--append-unique local-candidates name)))
            (puthash name entry entries)))
      (error nil))
    (list :category (majutsu-ref--category kind)
          :candidates (or candidates local-candidates)
          :entries entries
          :annotation-suffix-function
          (majutsu-ref--completion-suffix-function kind entries))))

(defun majutsu-ref-read (kind prompt payload history &optional default require-match directory)
  "Read one ref of KIND with PROMPT from structured PAYLOAD.
HISTORY is the minibuffer history variable.  DEFAULT, REQUIRE-MATCH, and
DIRECTORY are forwarded to `majutsu-completing-read-payload'."
  (let ((default-directory (or directory default-directory)))
    (majutsu-completing-read-payload
     prompt payload nil require-match nil history default
     (majutsu-ref--category kind) nil default-directory)))

(defun majutsu-ref-read-multiple
    (kind prompt payload history &optional default require-match directory)
  "Read multiple refs of KIND with PROMPT from structured PAYLOAD.
HISTORY is the minibuffer history variable.  DEFAULT, REQUIRE-MATCH, and
DIRECTORY are forwarded to `majutsu-completing-read-multiple-payload'."
  (let ((default-directory (or directory default-directory)))
    (majutsu-completing-read-multiple-payload
     prompt payload nil require-match nil history default
     (majutsu-ref--category kind) nil default-directory)))

(provide 'majutsu-ref)
;;; majutsu-ref.el ends here
