;;; majutsu-real-jj-test.el --- Real jj integration tests  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Opt-in integration tests.  Run with:
;;   MAJUTSU_TEST_REAL_JJ=1 MAJUTSU_TEST_JJ=jjj eask test ert test/integration/*.el
;; Omit MAJUTSU_TEST_JJ to use `majutsu-jj-executable'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'majutsu-bookmark)
(require 'majutsu-diff)
(require 'majutsu-file)
(require 'majutsu-log)
(require 'majutsu-remote)
(require 'majutsu-workspace)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'majutsu-integration-helpers)

(ert-deftest majutsu-integration-toplevel/inside-and-outside-repo ()
  "Real jj repository discovery should find roots only inside a repo."
  (majutsu-integration-skip-unless-enabled)
  (majutsu-integration-with-temp-dir
    (should-not (majutsu-toplevel default-directory))
    (majutsu-integration-run-jj "git" "init" "repo")
    (let* ((root (file-name-as-directory (expand-file-name "repo" default-directory)))
           (subdir (expand-file-name "dir/sub" root)))
      (make-directory subdir t)
      (should (equal (majutsu-toplevel root) root))
      (should (equal (majutsu-toplevel subdir) root)))))

(ert-deftest majutsu-integration-bookmark/candidate-data-sees-real-bookmark ()
  "Bookmark completion data should include bookmarks created by real jj."
  (majutsu-integration-with-jj-repo
    (majutsu-integration-create-change "bookmark target" "file.txt" "content\n")
    (majutsu-integration-run-jj "bookmark" "create" "main" "-r" "@")
    (let* ((payload (majutsu-bookmark-candidate-data nil default-directory))
           (entries (plist-get payload :entries))
           (entry (gethash "main" entries)))
      (should (member "main" (plist-get payload :candidates)))
      (should (plist-get entry :local)))))

(ert-deftest majutsu-integration-remote/candidate-data-parses-real-remote ()
  "Remote completion data should include real jj Git remotes."
  (majutsu-integration-with-jj-repo
    (majutsu-integration-run-jj "git" "remote" "add" "origin" "https://example.invalid/repo.git")
    (let* ((payload (majutsu-remote-candidate-data default-directory))
           (entry (gethash "origin" (plist-get payload :entries))))
      (should (equal (plist-get payload :candidates) '("origin")))
      (should (equal (plist-get entry :fetch-url) "https://example.invalid/repo.git")))))

(ert-deftest majutsu-integration-log/parses-real-log-output ()
  "Log parser should parse real jj log output generated with Majutsu templates."
  (majutsu-integration-with-jj-repo
    (let* ((change-id (majutsu-integration-create-change "log target" "file.txt" "content\n"))
           (majutsu-log--compiled-template-cache nil)
           (compiled (majutsu-log--compile-layout))
           (raw (majutsu-jj-buffer-string "log" "--no-graph" "-r" "@" "-T"
                                          (plist-get compiled :template)))
           (entries (with-temp-buffer
                      (insert raw)
                      (goto-char (point-min))
                      (majutsu-row-parse-buffer compiled)))
           (entry (car entries)))
      (should (= (length entries) 1))
      (should (equal (majutsu-row-column entry 'description) "log target"))
      (should (equal (majutsu-row-column entry 'change-id) change-id)))))

(ert-deftest majutsu-integration-diff/washes-real-diff-output ()
  "Diff washer should sectionize real jj diff output."
  (majutsu-integration-with-jj-repo
    (majutsu-integration-create-change "base" "file.txt" "one\n")
    (majutsu-integration-run-jj "new" "-m" "edit")
    (majutsu-integration-write-file "file.txt" "two\n")
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t)
            (majutsu-process-apply-ansi-colors nil))
        (magit-insert-section (diffbuf)
          (majutsu-jj-wash #'majutsu-diff-wash-diffs nil "diff" "--git" "--stat")))
      (goto-char (point-min))
      (should (search-forward "file.txt" nil t))
      (should (seq-some (lambda (section)
                          (eq (oref section type) 'jj-file))
                        (oref (car (oref magit-root-section children)) children))))))

(ert-deftest majutsu-integration-file/find-file-noselect-loads-real_blob ()
  "Blob file visiting should load content from a real jj revision."
  (majutsu-integration-with-jj-repo
    (majutsu-integration-create-change "blob target" "dir/file.txt" "hello\n")
    (let ((buffer (majutsu-find-file-noselect "@" "dir/file.txt" t)))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal (buffer-string) "hello\n"))
            (should (equal majutsu-buffer-blob-path "dir/file.txt")))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest majutsu-integration-workspace/list-entries-sees-added-workspace ()
  "Workspace list parser should see workspaces created by real jj."
  (majutsu-integration-with-jj-repo
    (let* ((main-root default-directory)
           (workspace-root (file-name-as-directory
                            (expand-file-name "../feature" main-root))))
      (majutsu-integration-run-jj "workspace" "add" workspace-root)
      (let* ((entries (majutsu-workspace-list-entries main-root))
             (names (mapcar (lambda (entry) (plist-get entry :name)) entries)))
        (should (member "default" names))
        (should (member "feature" names))))))

(provide 'majutsu-real-jj-test)
;;; majutsu-real-jj-test.el ends here
