;;; majutsu-marginalia-test.el --- Tests for Marginalia integration -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for optional Marginalia integration helpers.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-marginalia)

(defvar marginalia-separator)
(defvar marginalia--metadata)

(ert-deftest majutsu-marginalia-prewarm-candidate-data/caches-annotations-per-directory ()
  (let ((marginalia-separator "  ")
        (majutsu-marginalia--payload-cache (make-hash-table :test #'equal)))
    (let ((annotations-a (make-hash-table :test #'equal))
          (annotations-b (make-hash-table :test #'equal)))
      (puthash "v1.0" "repo-a tag" annotations-a)
      (puthash "v1.0" "repo-b tag" annotations-b)
      (majutsu-marginalia-prewarm-candidate-data
       'majutsu-tag
       (list :candidates '("v1.0") :annotations annotations-a)
       nil "/tmp/repo-a/")
      (majutsu-marginalia-prewarm-candidate-data
       'majutsu-tag
       (list :candidates '("v1.0") :annotations annotations-b)
       nil "/tmp/repo-b/")
      (let ((default-directory "/tmp/repo-a/"))
        (should (equal (majutsu-marginalia--cached-annotation 'majutsu-tag "v1.0")
                       "  repo-a tag")))
      (let ((default-directory "/tmp/repo-b/"))
        (should (equal (majutsu-marginalia--cached-annotation 'majutsu-tag "v1.0")
                       "  repo-b tag"))))))

(ert-deftest majutsu-marginalia-annotate-revision/prefers-original-annotation ()
  (let ((marginalia-separator "  ")
        (marginalia--metadata 'dummy))
    (cl-letf (((symbol-function 'marginalia--orig-completion-metadata-get)
               (lambda (_metadata prop)
                 (when (eq prop 'annotation-function)
                   (lambda (cand)
                     (format "  [%s]" cand))))))
      (should (equal (majutsu-marginalia-annotate-revision "main")
                     "  [main]")))))

(ert-deftest majutsu-marginalia-annotate-bookmark/uses-structured-entry ()
  (let ((marginalia-separator "  ")
        (default-directory "/tmp/repo/")
        (majutsu-marginalia--payload-cache (make-hash-table :test #'equal)))
    (let ((entries (make-hash-table :test #'equal)))
      (puthash "main"
               '(:local t :synced t :conflict t
                 :tracked-remotes ("origin")
                 :untracked-remotes ("fork"))
               entries)
      (majutsu-marginalia-prewarm-candidate-data
       'majutsu-bookmark (list :entries entries) nil default-directory)
      (let ((annotation (majutsu-marginalia-annotate-bookmark "main")))
        (should (string-match-p "bookmark" annotation))
        (should (string-match-p "local" annotation))
        (should (string-match-p "synced" annotation))
        (should (string-match-p "conflicted" annotation))
        (should (string-match-p "tracked@origin" annotation))
        (should (string-match-p "untracked@fork" annotation))))))

(ert-deftest majutsu-marginalia-annotate-remote/uses-cached-urls ()
  (let ((marginalia-separator "  ")
        (default-directory "/tmp/repo/")
        (majutsu-marginalia--payload-cache (make-hash-table :test #'equal)))
    (let ((entries (make-hash-table :test #'equal)))
      (puthash "rad"
               '(:name "rad"
                 :fetch-url "rad://z4fugm4aenykjpk8tpvvqjvwtzvwj"
                 :push-url "rad://z4fugm4aenykjpk8tpvvqjvwtzvwj/z6Mk...")
               entries)
      (majutsu-marginalia-prewarm-candidate-data
       'majutsu-remote (list :entries entries) nil default-directory)
      (let ((annotation (majutsu-marginalia-annotate-remote "rad")))
        (should (string-match-p "git remote" annotation))
        (should (string-match-p "rad://z4fugm4aenykjpk8tpvvqjvwtzvwj" annotation))
        (should (string-match-p "push:rad://z4fugm4aenykjpk8tpvvqjvwtzvwj/z6Mk" annotation))))))

(ert-deftest majutsu-marginalia-annotate-workspace/uses-cached-entry ()
  (let ((marginalia-separator "  ")
        (default-directory "/tmp/repo/")
        (majutsu-marginalia--payload-cache (make-hash-table :test #'equal)))
    (let ((entries (make-hash-table :test #'equal)))
      (puthash "default"
               '(:current t :root "/tmp/repo/default/"
                 :change-id "wnurqwps" :desc "Main workspace")
               entries)
      (majutsu-marginalia-prewarm-candidate-data
       'majutsu-workspace (list :entries entries) nil default-directory)
      (let ((annotation (majutsu-marginalia-annotate-workspace "default")))
        (should (string-match-p "current" annotation))
        (should (string-match-p "/tmp/repo/default/" annotation))
        (should (string-match-p "wnurqwps" annotation))
        (should (string-match-p "Main workspace" annotation))))))

(ert-deftest majutsu-marginalia-annotate-workspace/marks-current-workspace-without-cache ()
  (let ((marginalia-separator "  "))
    (cl-letf (((symbol-function 'majutsu-workspace-current-name)
               (lambda (&optional _directory)
                 "default")))
      (should (string-match-p "current workspace"
                              (majutsu-marginalia-annotate-workspace "default")))
      (should (string-match-p "workspace"
                              (majutsu-marginalia-annotate-workspace "feature"))))))

(ert-deftest majutsu-marginalia-annotate-file/expands-against-repo-root ()
  (let ((marginalia-separator "  ")
        seen-path)
    (cl-letf (((symbol-function 'majutsu-toplevel)
               (lambda (&optional _directory)
                 "/tmp/repo/"))
              ((symbol-function 'marginalia-annotate-file)
               (lambda (path)
                 (setq seen-path path)
                 "  file-attrs")))
      (should (equal (majutsu-marginalia-annotate-file "src/main.el")
                     "  file-attrs"))
      (should (equal seen-path "/tmp/repo/src/main.el")))))

(provide 'majutsu-marginalia-test)
;;; majutsu-marginalia-test.el ends here
