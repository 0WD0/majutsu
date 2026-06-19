;;; majutsu-gerrit-data.el --- Gerrit data structures for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Typed wrappers around Gerrit's REST API alists.
;;
;; Every struct keeps the original raw alist.  Gerrit's REST API is
;; forward-compatible by adding fields, so higher-level UI should not
;; lose information just because M2 does not promote every nested object
;; into a dedicated struct yet.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(cl-defstruct (majutsu-gerrit-account
               (:constructor majutsu-gerrit-account-create))
  raw account-id name email username display-name)

(cl-defstruct (majutsu-gerrit-range
               (:constructor majutsu-gerrit-range-create))
  raw start-line start-character end-line end-character)

(cl-defstruct (majutsu-gerrit-comment
               (:constructor majutsu-gerrit-comment-create))
  raw id patch-set path side parent line range in-reply-to message updated
  author tag change-message-id unresolved commit-id context-lines
  source-content-type fix-suggestions draft-p)

(cl-defstruct (majutsu-gerrit-revision
               (:constructor majutsu-gerrit-revision-create))
  raw id number ref commit files uploader real-uploader fetch description current-p)

(cl-defstruct (majutsu-gerrit-change
               (:constructor majutsu-gerrit-change-create))
  raw id triplet-id number project branch full-branch topic hashtags change-id subject
  status created updated submitted owner submitter current-revision
  current-revision-number revisions labels reviewers pending-reviewers
  attention-set total-comment-count unresolved-comment-count work-in-progress
  has-review-started mergeable submittable more-changes-p)

(cl-defstruct (majutsu-gerrit-conversation
               (:constructor majutsu-gerrit-conversation-create))
  raw id path side parent patch-set line range comments unresolved draft-p)

;;;; Alist helpers

(defun majutsu-gerrit-data--assoc (alist key)
  "Return the first ALIST cell matching KEY.

KEY may be a symbol or string.  Gerrit JSON parsed with
`json-parse-string' and `:object-type \='alist' uses symbols, while tests
and hand-written payloads often use strings."
  (let* ((symbol-key (cond ((symbolp key) key)
                           ((stringp key) (intern-soft key))))
         (string-key (cond ((stringp key) key)
                           ((symbolp key) (symbol-name key)))))
    (or (and symbol-key (assq symbol-key alist))
        (and string-key (assoc string-key alist))
        (and string-key (assoc-string string-key alist t)))))

(defun majutsu-gerrit-data--get (alist key)
  "Return KEY from ALIST, accepting symbol and string keys."
  (cdr (majutsu-gerrit-data--assoc alist key)))

(defun majutsu-gerrit-data--key-name (key)
  "Return KEY as a string."
  (cond ((symbolp key) (symbol-name key))
        ((stringp key) key)
        (t (format "%s" key))))

(defun majutsu-gerrit-data--alist-map (alist fn)
  "Map FN over ALIST preserving stringified keys.
FN is called with (KEY VALUE)."
  (mapcar (lambda (cell)
            (cons (majutsu-gerrit-data--key-name (car cell))
                  (funcall fn (majutsu-gerrit-data--key-name (car cell))
                           (cdr cell))))
          alist))

(defun majutsu-gerrit-data--updated< (a b)
  "Return non-nil if comment A was updated before B."
  (string< (or (majutsu-gerrit-comment-updated a) "")
           (or (majutsu-gerrit-comment-updated b) "")))

;;;; Constructors

(defun majutsu-gerrit-account-from-alist (alist)
  "Return a `majutsu-gerrit-account' from ALIST."
  (when alist
    (majutsu-gerrit-account-create
     :raw alist
     :account-id (majutsu-gerrit-data--get alist '_account_id)
     :name (majutsu-gerrit-data--get alist 'name)
     :email (majutsu-gerrit-data--get alist 'email)
     :username (majutsu-gerrit-data--get alist 'username)
     :display-name (majutsu-gerrit-data--get alist 'display_name))))

(defun majutsu-gerrit-range-from-alist (alist)
  "Return a `majutsu-gerrit-range' from ALIST."
  (when alist
    (majutsu-gerrit-range-create
     :raw alist
     :start-line (majutsu-gerrit-data--get alist 'start_line)
     :start-character (majutsu-gerrit-data--get alist 'start_character)
     :end-line (majutsu-gerrit-data--get alist 'end_line)
     :end-character (majutsu-gerrit-data--get alist 'end_character))))

(defun majutsu-gerrit-comment--side (alist)
  "Return normalized side from comment ALIST."
  (let ((side (majutsu-gerrit-data--get alist 'side)))
    (cond
     ((or (null side) (equal side "REVISION") (eq side 'REVISION)) 'revision)
     ((or (equal side "PARENT") (eq side 'PARENT)) 'parent)
     (t side))))

(defun majutsu-gerrit-comment-from-alist (alist &optional path draft-p)
  "Return a `majutsu-gerrit-comment' from ALIST.
PATH is used for map-keyed comment responses that omit the path field.
DRAFT-P marks comments returned by draft endpoints."
  (when alist
    (majutsu-gerrit-comment-create
     :raw alist
     :id (majutsu-gerrit-data--get alist 'id)
     :patch-set (majutsu-gerrit-data--get alist 'patch_set)
     :path (or (majutsu-gerrit-data--get alist 'path) path)
     :side (majutsu-gerrit-comment--side alist)
     :parent (majutsu-gerrit-data--get alist 'parent)
     :line (majutsu-gerrit-data--get alist 'line)
     :range (majutsu-gerrit-range-from-alist
             (majutsu-gerrit-data--get alist 'range))
     :in-reply-to (majutsu-gerrit-data--get alist 'in_reply_to)
     :message (majutsu-gerrit-data--get alist 'message)
     :updated (majutsu-gerrit-data--get alist 'updated)
     :author (majutsu-gerrit-account-from-alist
              (majutsu-gerrit-data--get alist 'author))
     :tag (majutsu-gerrit-data--get alist 'tag)
     :change-message-id (majutsu-gerrit-data--get alist 'change_message_id)
     :unresolved (majutsu-gerrit-data--get alist 'unresolved)
     :commit-id (majutsu-gerrit-data--get alist 'commit_id)
     :context-lines (majutsu-gerrit-data--get alist 'context_lines)
     :source-content-type (majutsu-gerrit-data--get alist 'source_content_type)
     :fix-suggestions (majutsu-gerrit-data--get alist 'fix_suggestions)
     :draft-p draft-p)))

(defun majutsu-gerrit-revision-from-alist (id alist &optional current-id)
  "Return a `majutsu-gerrit-revision' for ID from ALIST.
CURRENT-ID marks which revision is current."
  (when alist
    (majutsu-gerrit-revision-create
     :raw alist
     :id id
     :number (majutsu-gerrit-data--get alist '_number)
     :ref (majutsu-gerrit-data--get alist 'ref)
     :commit (majutsu-gerrit-data--get alist 'commit)
     :files (majutsu-gerrit-data--get alist 'files)
     :uploader (majutsu-gerrit-account-from-alist
                (majutsu-gerrit-data--get alist 'uploader))
     :real-uploader (majutsu-gerrit-account-from-alist
                     (majutsu-gerrit-data--get alist 'real_uploader))
     :fetch (majutsu-gerrit-data--get alist 'fetch)
     :description (majutsu-gerrit-data--get alist 'description)
     :current-p (and current-id (equal id current-id)))))

(defun majutsu-gerrit-data--revisions-from-alist (revisions current-id)
  "Return typed revisions from Gerrit REVISIONS map."
  (when revisions
    (majutsu-gerrit-data--alist-map
     revisions
     (lambda (id revision)
       (majutsu-gerrit-revision-from-alist id revision current-id)))))

(defun majutsu-gerrit-change-from-alist (alist)
  "Return a `majutsu-gerrit-change' from ALIST."
  (when alist
    (let ((current-id (majutsu-gerrit-data--get alist 'current_revision)))
      (majutsu-gerrit-change-create
       :raw alist
       :id (majutsu-gerrit-data--get alist 'id)
       :triplet-id (majutsu-gerrit-data--get alist 'triplet_id)
       :number (majutsu-gerrit-data--get alist '_number)
       :project (majutsu-gerrit-data--get alist 'project)
       :branch (majutsu-gerrit-data--get alist 'branch)
       :full-branch (majutsu-gerrit-data--get alist 'full_branch)
       :topic (majutsu-gerrit-data--get alist 'topic)
       :hashtags (majutsu-gerrit-data--get alist 'hashtags)
       :change-id (majutsu-gerrit-data--get alist 'change_id)
       :subject (majutsu-gerrit-data--get alist 'subject)
       :status (majutsu-gerrit-data--get alist 'status)
       :created (majutsu-gerrit-data--get alist 'created)
       :updated (majutsu-gerrit-data--get alist 'updated)
       :submitted (majutsu-gerrit-data--get alist 'submitted)
       :owner (majutsu-gerrit-account-from-alist
               (majutsu-gerrit-data--get alist 'owner))
       :submitter (majutsu-gerrit-account-from-alist
                   (majutsu-gerrit-data--get alist 'submitter))
       :current-revision current-id
       :current-revision-number (majutsu-gerrit-data--get alist 'current_revision_number)
       :revisions (majutsu-gerrit-data--revisions-from-alist
                   (majutsu-gerrit-data--get alist 'revisions)
                   current-id)
       :labels (majutsu-gerrit-data--get alist 'labels)
       :reviewers (majutsu-gerrit-data--get alist 'reviewers)
       :pending-reviewers (majutsu-gerrit-data--get alist 'pending_reviewers)
       :attention-set (majutsu-gerrit-data--get alist 'attention_set)
       :total-comment-count (majutsu-gerrit-data--get alist 'total_comment_count)
       :unresolved-comment-count (majutsu-gerrit-data--get alist 'unresolved_comment_count)
       :work-in-progress (majutsu-gerrit-data--get alist 'work_in_progress)
       :has-review-started (majutsu-gerrit-data--get alist 'has_review_started)
       :mergeable (majutsu-gerrit-data--get alist 'mergeable)
       :submittable (majutsu-gerrit-data--get alist 'submittable)
       :more-changes-p (majutsu-gerrit-data--get alist '_more_changes)))))

;;;; Conversation grouping

(defun majutsu-gerrit-conversation--children (comment comments)
  "Return direct children of COMMENT from COMMENTS."
  (seq-filter (lambda (candidate)
                (equal (majutsu-gerrit-comment-in-reply-to candidate)
                       (majutsu-gerrit-comment-id comment)))
              comments))

(defun majutsu-gerrit-conversation--cluster (root comments)
  "Return ROOT and all recursive replies from COMMENTS, sorted chronologically."
  (let ((cluster (list root)))
    (dolist (child (majutsu-gerrit-conversation--children root comments))
      (setq cluster (append cluster
                            (majutsu-gerrit-conversation--cluster child comments))))
    (sort cluster #'majutsu-gerrit-data--updated<)))

(defun majutsu-gerrit-conversation--latest (comments)
  "Return latest comment in COMMENTS."
  (car (last (sort (copy-sequence comments) #'majutsu-gerrit-data--updated<))))

(defun majutsu-gerrit-conversation--from-cluster (comments &optional raw draft-p)
  "Return a conversation from clustered COMMENTS."
  (let* ((root (car comments))
         (latest (majutsu-gerrit-conversation--latest comments)))
    (majutsu-gerrit-conversation-create
     :raw raw
     :id (majutsu-gerrit-comment-id root)
     :path (majutsu-gerrit-comment-path root)
     :side (majutsu-gerrit-comment-side root)
     :parent (majutsu-gerrit-comment-parent root)
     :patch-set (majutsu-gerrit-comment-patch-set root)
     :line (majutsu-gerrit-comment-line root)
     :range (majutsu-gerrit-comment-range root)
     :comments comments
     :unresolved (majutsu-gerrit-comment-unresolved latest)
     :draft-p draft-p)))

(defun majutsu-gerrit-conversations-from-comments (comment-map &optional draft-p)
  "Return conversations grouped from COMMENT-MAP.
COMMENT-MAP is the Gerrit map from file path to CommentInfo lists."
  (let (conversations)
    (dolist (file-cell comment-map)
      (let* ((path (majutsu-gerrit-data--key-name (car file-cell)))
             (comments (mapcar (lambda (comment)
                                 (majutsu-gerrit-comment-from-alist
                                  comment path draft-p))
                               (cdr file-cell)))
             (roots (seq-remove #'majutsu-gerrit-comment-in-reply-to comments))
             ;; Ported draft comments can be replies without the parent
             ;; thread.  Treat orphaned replies as roots so they remain
             ;; visible instead of being dropped.
             (orphans (seq-filter
                       (lambda (comment)
                         (and (majutsu-gerrit-comment-in-reply-to comment)
                              (not (seq-find
                                    (lambda (candidate)
                                      (equal (majutsu-gerrit-comment-id candidate)
                                             (majutsu-gerrit-comment-in-reply-to comment)))
                                    comments))))
                       comments)))
        (dolist (root (append roots orphans))
          (push (majutsu-gerrit-conversation--from-cluster
                 (majutsu-gerrit-conversation--cluster root comments)
                 (list (cons path (cdr file-cell)))
                 draft-p)
                conversations))))
    (nreverse conversations)))

(provide 'majutsu-gerrit-data)

;;; majutsu-gerrit-data.el ends here
