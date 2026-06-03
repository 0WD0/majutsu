;;; majutsu-bookmark-test.el --- Tests for bookmark helpers  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for bookmark parsing and transient argument behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'majutsu-bookmark)

(ert-deftest majutsu-bookmark-split-remote-ref/basic ()
  (should (equal (majutsu--bookmark-split-remote-ref "main@origin")
                 '("main" . "origin"))))

(ert-deftest majutsu-bookmark-split-remote-ref/last-at ()
  (should (equal (majutsu--bookmark-split-remote-ref "foo@bar@origin")
                 '("foo@bar" . "origin"))))

(ert-deftest majutsu-bookmark-split-remote-ref/no-remote ()
  (should (equal (majutsu--bookmark-split-remote-ref "main")
                 '("main" . nil))))

(ert-deftest majutsu-bookmark-get-bookmark-names/local-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("main" "feature"))))
      (should (equal (majutsu--get-bookmark-names 'local) '("main" "feature")))
      (should (equal (seq-take seen-args 3) '("bookmark" "list" "--quiet")))
      (should-not (member "--all-remotes" seen-args))
      (should-not (member "--tracked" seen-args))
      (should (member "-T" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "!remote") template))
        (should (string-match-p (regexp-quote "present") template))
        (should (string-match-p (regexp-quote "\\n") template))))))

(ert-deftest majutsu-bookmark-get-bookmark-names/remote-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("main@origin" "dev@upstream"))))
      (should (equal (majutsu--get-bookmark-names 'remote) '("main@origin" "dev@upstream")))
      (should (member "--all-remotes" seen-args))
      (should (member "-T" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "remote && present") template))
        (should (string-match-p (regexp-quote "\"@\"") template))))))

(ert-deftest majutsu-bookmark-get-bookmark-names/remote-tracked-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("main@origin"))))
      (should (equal (majutsu--get-bookmark-names 'remote-tracked) '("main@origin")))
      (should (member "--tracked" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "tracked") template))))))

(ert-deftest majutsu-bookmark-get-bookmark-names/remote-untracked-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("topic@origin"))))
      (should (equal (majutsu--get-bookmark-names 'remote-untracked) '("topic@origin")))
      (should (member "--all-remotes" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "!tracked") template))))))

(ert-deftest majutsu-bookmark-list-args/uses-structured-template ()
  (let ((majutsu-bookmark--list-all nil))
    (let ((args (majutsu-bookmark--list-args)))
      (should (equal (seq-take args 3) '("bookmark" "list" "--quiet")))
      (should (member "-T" args))
      (should (equal (cadr (member "-T" args)) majutsu-bookmark--list-template))
      (should-not (member "--all-remotes" args)))))

(ert-deftest majutsu-bookmark-list-args/all-remotes ()
  (let ((majutsu-bookmark--list-all t))
    (should (member "--all-remotes" (majutsu-bookmark--list-args)))))

(ert-deftest majutsu-bookmark-parse-list-output/basic ()
  (let* ((sep majutsu-bookmark--list-field-separator)
         (line (string-join '("main" "" "1" "0" "0" "0" "1"
                              "kzsquqlr" "32730a15" "Arthur Heymans"
                              "2 weeks ago" "Add libgfxinit support" "" "")
                            sep)))
    (should (equal (majutsu-bookmark-parse-list-output (concat line "\n"))
                   '((:name "main"
                      :remote nil
                      :present t
                      :conflict nil
                      :tracked nil
                      :tracking-present nil
                      :synced t
                      :change-id "kzsquqlr"
                      :commit-id "32730a15"
                      :author "Arthur Heymans"
                      :age "2 weeks ago"
                      :description "Add libgfxinit support"
                      :ahead nil
                      :behind nil))))))

(ert-deftest majutsu-bookmark-parse-list-output/remote-tracked ()
  (let* ((sep majutsu-bookmark--list-field-separator)
         (line (string-join '("main" "origin" "1" "0" "1" "1" "0"
                              "kzsquqlr" "32730a15" "Arthur Heymans"
                              "2 weeks ago" "Add libgfxinit support" "2" "1")
                            sep))
         (entry (car (majutsu-bookmark-parse-list-output line))))
    (should (equal (plist-get entry :remote) "origin"))
    (should (plist-get entry :tracked))
    (should (equal (majutsu-bookmark--entry-ref entry) "main@origin"))
    (should (equal (majutsu-bookmark--entry-state entry) "+2/-1"))))


(ert-deftest majutsu-bookmark-track/reads-patterns-and-calls-jj ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu--bookmark-remote-name-candidates)
               (lambda () '("main" "dev")))
              ((symbol-function 'majutsu--bookmark-git-remote-candidates)
               (lambda () '("origin" "upstream")))
              ((symbol-function 'completing-read-multiple)
               (lambda (prompt _collection &rest _args)
                 (cond
                  ((string-match-p "Track bookmark name" prompt)
                   '("main" "glob:\"feat*\""))
                  ((string-match-p "Remote.*pattern" prompt)
                   '("origin" "upstream"))
                  (t nil))))
              ((symbol-function 'majutsu-call-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (majutsu-bookmark-track)
      (should (equal called
                     '("bookmark" "track"
                       "main" "glob:\"feat*\""
                       "--remote" "origin"
                       "--remote" "upstream"))))))

(ert-deftest majutsu-bookmark-untrack/builds-remote-args ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-call-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (majutsu-bookmark-untrack '("main" "glob:\"ci/*\"") '("origin" "upstream"))
      (should (equal called
                     '("bookmark" "untrack"
                       "main" "glob:\"ci/*\""
                       "--remote" "origin"
                       "--remote" "upstream"))))))

;;; majutsu-bookmark-test.el ends here
