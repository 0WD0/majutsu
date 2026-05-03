;;; majutsu-ref-test.el --- Tests for shared CommitRef helpers  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for shared bookmark/tag CommitRef completion helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'majutsu-ref)

(ert-deftest majutsu-ref-format-commit-summary/expands-template-alias ()
  (let ((template (majutsu-template-compile
                   '[:method [:self] :format_commit_summary_with_refs ""]
                   'Commit)))
    (should-not (string-match-p "format_commit_summary_with_refs" template))
    (should (string-match-p (regexp-quote "change_id().shortest(8)") template))
    (should (string-match-p (regexp-quote "commit_id().shortest(8)") template))
    (should (string-match-p (regexp-quote "description().first_line()") template))
    (should (string-match-p (regexp-quote "label(\"empty\", \"(empty)\")") template))))

(ert-deftest majutsu-ref-format-commit-summary/can-render-on-explicit-commit ()
  (let ((template (majutsu-template-compile
                   '[:method [:raw "c" :Commit] :format_commit_summary_with_refs ""]
                   'CommitRef)))
    (should (string-match-p (regexp-quote "c.change_id().shortest(8)") template))
    (should (string-match-p (regexp-quote "c.commit_id().shortest(8)") template))))

(ert-deftest majutsu-ref-names/bookmark-tracked-uses-command-flags ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args args)
                 '("main@origin"))))
      (should (equal (majutsu-ref-names 'bookmark 'remote-tracked)
                     '("main@origin")))
      (should (equal (seq-take seen-args 3)
                     '("bookmark" "list" "--quiet")))
      (should (member "--tracked" seen-args))
      (should (member "-T" seen-args)))))

(ert-deftest majutsu-ref-names/tag-remote-uses-command-flags ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args args)
                 '("v1.0@git"))))
      (should (equal (majutsu-ref-names 'tag 'remote)
                     '("v1.0@git")))
      (should (equal (seq-take seen-args 3)
                     '("tag" "list" "--quiet")))
      (should (member "--all-remotes" seen-args))
      (should (member "-T" seen-args)))))

(ert-deftest majutsu-ref-candidate-data/aggregates-local-and-remote-state ()
  (let ((sep majutsu-ref--completion-field-separator))
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest _args)
                 (list (concat "main" sep "" sep "" sep "t" sep "" sep "t")
                       (concat "main" sep "origin" sep "" sep "t" sep "t" sep "t")
                       (concat "feature" sep "fork" sep "t" sep "t" sep "" sep "")))))
      (let* ((payload (majutsu-ref-candidate-data 'bookmark '("main" "feature")))
             (entries (plist-get payload :entries))
             (main (gethash "main" entries))
             (feature (gethash "feature" entries)))
        (should (eq (plist-get payload :category) 'majutsu-bookmark))
        (should (equal (plist-get payload :candidates) '("main" "feature")))
        (should (plist-get main :local))
        (should (plist-get main :synced))
        (should (equal (plist-get main :tracked-remotes) '("origin")))
        (should (plist-get feature :conflict))
        (should (equal (plist-get feature :untracked-remotes) '("fork")))))))

(ert-deftest majutsu-ref-read/forwards-category-and-history ()
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-completing-read-payload)
               (lambda (&rest args)
                 (setq seen args)
                 "main")))
      (should (equal (majutsu-ref-read 'bookmark "Bookmark"
                                       '(:candidates ("main"))
                                       'majutsu-bookmark-name-history
                                       "main" 'any "/tmp/repo/")
                     "main"))
      (should (equal (nth 0 seen) "Bookmark"))
      (should (equal (nth 1 seen) '(:candidates ("main"))))
      (should (eq (nth 5 seen) 'majutsu-bookmark-name-history))
      (should (equal (nth 6 seen) "main"))
      (should (eq (nth 7 seen) 'majutsu-bookmark))
      (should (equal (nth 9 seen) "/tmp/repo/")))))

(ert-deftest majutsu-ref-read-multiple/forwards-category-and-history ()
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-completing-read-multiple-payload)
               (lambda (&rest args)
                 (setq seen args)
                 '("v1.0"))))
      (should (equal (majutsu-ref-read-multiple 'tag "Tags"
                                                '(:candidates ("v1.0"))
                                                'majutsu-tag-name-history
                                                "v1.0" 'any "/tmp/repo/")
                     '("v1.0")))
      (should (equal (nth 0 seen) "Tags"))
      (should (equal (nth 1 seen) '(:candidates ("v1.0"))))
      (should (eq (nth 5 seen) 'majutsu-tag-name-history))
      (should (equal (nth 6 seen) "v1.0"))
      (should (eq (nth 7 seen) 'majutsu-tag))
      (should (equal (nth 9 seen) "/tmp/repo/")))))

(provide 'majutsu-ref-test)
;;; majutsu-ref-test.el ends here
