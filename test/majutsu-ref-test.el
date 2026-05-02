;;; majutsu-ref-test.el --- Tests for shared CommitRef helpers  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for shared bookmark/tag CommitRef completion helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'majutsu-ref)

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

(provide 'majutsu-ref-test)
;;; majutsu-ref-test.el ends here
