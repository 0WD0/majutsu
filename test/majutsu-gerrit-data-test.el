;;; majutsu-gerrit-data-test.el --- Tests for majutsu-gerrit-data -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Unit tests for Gerrit data structures.

;;; Code:

(require 'ert)
(require 'majutsu-gerrit-data)

(ert-deftest majutsu-gerrit-change-from-alist/preserves-id-distinction-and-raw ()
  "ChangeInfo.id and change_id should remain distinct."
  (let* ((raw '((id . "majutsu~72")
                (_number . 72)
                (triplet_id . "majutsu~main~Iabc")
                (project . "majutsu")
                (branch . "main")
                (change_id . "Iabc")
                (subject . "Prefer section-aware command defaults.")
                (status . "NEW")
                (owner . ((_account_id . 1000000) (name . "Admin")))
                (current_revision . "deadbeef")
                (current_revision_number . 3)
                (revisions . ((deadbeef . ((_number . 3) (ref . "refs/changes/72/72/3")))
                              (cafebabe . ((_number . 2) (ref . "refs/changes/72/72/2")))))
                (_more_changes . nil)))
         (change (majutsu-gerrit-change-from-alist raw)))
    (should (eq (majutsu-gerrit-change-raw change) raw))
    (should (equal (majutsu-gerrit-change-id change) "majutsu~72"))
    (should (equal (majutsu-gerrit-change-number change) 72))
    (should (equal (majutsu-gerrit-change-change-id change) "Iabc"))
    (should (equal (majutsu-gerrit-account-name
                    (majutsu-gerrit-change-owner change))
                   "Admin"))
    (let ((revisions (majutsu-gerrit-change-revisions change)))
      (should (equal (mapcar #'car revisions) '("deadbeef" "cafebabe")))
      (should (majutsu-gerrit-revision-current-p (cdar revisions)))
      (should-not (majutsu-gerrit-revision-current-p (cdadr revisions))))))

(ert-deftest majutsu-gerrit-comment-from-alist/defaults-side-and-draft-author ()
  "Missing side means revision side; drafts may not have authors."
  (let* ((comment (majutsu-gerrit-comment-from-alist
                   '((id . "c1")
                     (patch_set . 3)
                     (line . 23)
                     (message . "nit")
                     (updated . "2026-06-19 00:00:00.000000000")
                     (range . ((start_line . 23)
                               (start_character . 2)
                               (end_line . 23)
                               (end_character . 5))))
                   "src/foo.el" t))
         (range (majutsu-gerrit-comment-range comment)))
    (should (eq (majutsu-gerrit-comment-side comment) 'revision))
    (should (majutsu-gerrit-comment-draft-p comment))
    (should-not (majutsu-gerrit-comment-author comment))
    (should (equal (majutsu-gerrit-comment-path comment) "src/foo.el"))
    (should (equal (majutsu-gerrit-range-start-line range) 23))
    (should (equal (majutsu-gerrit-range-start-character range) 2))
    (should (equal (majutsu-gerrit-range-end-character range) 5))))

(ert-deftest majutsu-gerrit-comment-from-alist/parent-side ()
  "Parent-side comments should normalize side and parent number."
  (let ((comment (majutsu-gerrit-comment-from-alist
                  '((id . "c1") (side . "PARENT") (parent . 2) (line . 10))
                  "src/foo.el")))
    (should (eq (majutsu-gerrit-comment-side comment) 'parent))
    (should (equal (majutsu-gerrit-comment-parent comment) 2))))

(ert-deftest majutsu-gerrit-conversations-from-comments/groups-thread-replies ()
  "Conversation grouping should follow in_reply_to links recursively."
  (let* ((comment-map '((src/foo.el
                         ((id . "root")
                          (patch_set . 1)
                          (line . 10)
                          (message . "root")
                          (updated . "2026-01-01 00:00:00.000000000")
                          (unresolved . t))
                         ((id . "reply")
                          (in_reply_to . "root")
                          (patch_set . 1)
                          (line . 10)
                          (message . "reply")
                          (updated . "2026-01-02 00:00:00.000000000")
                          (unresolved . nil)))))
         (conversations (majutsu-gerrit-conversations-from-comments comment-map))
         (conversation (car conversations)))
    (should (= (length conversations) 1))
    (should (equal (majutsu-gerrit-conversation-id conversation) "root"))
    (should (equal (majutsu-gerrit-conversation-path conversation) "src/foo.el"))
    (should (equal (majutsu-gerrit-conversation-line conversation) 10))
    (should-not (majutsu-gerrit-conversation-unresolved conversation))
    (should (equal (mapcar #'majutsu-gerrit-comment-id
                           (majutsu-gerrit-conversation-comments conversation))
                   '("root" "reply")))))

(ert-deftest majutsu-gerrit-conversations-from-comments/keeps-orphaned-drafts ()
  "Ported draft replies can be returned without their published parent."
  (let* ((comment-map '((src/foo.el
                         ((id . "draft")
                          (in_reply_to . "missing-parent")
                          (patch_set . 2)
                          (line . 7)
                          (message . "orphan")
                          (updated . "2026-01-03 00:00:00.000000000")))))
         (conversation (car (majutsu-gerrit-conversations-from-comments comment-map t))))
    (should (equal (majutsu-gerrit-conversation-id conversation) "draft"))
    (should (majutsu-gerrit-conversation-draft-p conversation))
    (should (majutsu-gerrit-comment-draft-p
             (car (majutsu-gerrit-conversation-comments conversation))))))

(provide 'majutsu-gerrit-data-test)

;;; majutsu-gerrit-data-test.el ends here
