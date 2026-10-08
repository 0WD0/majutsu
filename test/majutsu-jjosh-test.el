;;; majutsu-jjosh-test.el --- Tests for fixed backend builds -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'majutsu-log)
(require 'majutsu-jj-integration)

(ert-deftest majutsu-jjosh/real-template-parser-and-renderer ()
  "Exercise the row protocol with either real jj or project-aware jjosh."
  (majutsu-jj-integration-with-repo repo
    (let* ((majutsu-log-commit-columns
            (eval (car (get 'majutsu-log-commit-columns 'standard-value)) t))
           (majutsu-log--compiled-template-cache nil)
           (supported (eq majutsu-build-backend 'jjosh)))
      (when supported
        (dolist (name '("beta" "alpha"))
          (majutsu-jj-integration-call repo "project" "add" name "--path" name)
          (make-directory (expand-file-name name repo))
          (majutsu-jj-integration-write-text
           (expand-file-name (concat name "/file.txt") repo) "project edit\n")))
      (majutsu-jj-integration-write-text (expand-file-name "root.txt" repo) "root edit\n")
      (majutsu-jj-integration-call repo "describe" "-m" "Actual description [not a project]")
      (let* ((compiled (majutsu-log--ensure-template))
             (column (seq-find (lambda (c) (eq (plist-get c :field) 'touched-projects))
                               (plist-get compiled :columns)))
             (commit-id (string-trim (majutsu-jj-integration-output
                                     repo "--ignore-working-copy" "log" "--no-graph"
                                     "-r" "@" "-T" "commit_id")))
             (raw (majutsu-jj-integration-output
                   repo "--ignore-working-copy" "--color=never" "log" "-r" "@"
                   "-T" (plist-get compiled :template))))
        (should (eq (not (null column)) supported))
        (with-temp-buffer
          (magit-section-mode)
          (let ((inhibit-read-only t))
            (magit-insert-section (root)
              (insert raw)
              (goto-char (point-min))
              (let* ((entries (majutsu-row-wash-buffer compiled))
                     (entry (car entries)))
                (should (= (length entries) 1))
                (should (equal (majutsu-row-column entry 'commit-id) commit-id))
                (should (equal (majutsu-row-column entry 'description)
                               "Actual description [not a project]"))
                (when supported
                  (should (equal (majutsu-row-column-value entry column) "[alpha, beta]"))
                  (goto-char (point-min))
                  (search-forward "[alpha, beta]")
                  (should (eq (get-text-property (1- (point)) 'majutsu-row-field)
                              'touched-projects)))))))
        ;; Empty and root-only changes must not acquire project badges.
        (majutsu-jj-integration-call repo "new" "-m" "Empty")
        (dolist (root-edit '(nil t))
          (when root-edit
            (majutsu-jj-integration-write-text (expand-file-name "root.txt" repo) "root only\n"))
          (let* ((raw (majutsu-jj-integration-output
                       repo "--color=never" "log" "--no-graph" "-r" "@"
                       "-T" (plist-get compiled :template)))
                 (entry (with-temp-buffer
                          (insert raw)
                          (car (majutsu-row-parse-buffer compiled)))))
            (should entry)
            (when column
              (should (equal (majutsu-row-column-value entry column) "")))))))))

(ert-deftest majutsu-jjosh/remote-forget-preserves-local-bookmarks ()
  "Only the jjosh build offers remote forget; it preserves local bookmarks."
  (majutsu-jj-integration-with-sandbox sandbox
    (let* ((source (majutsu-jj-integration-init sandbox "source"))
           (repo (expand-file-name "clone" sandbox))
           (majutsu-jj-executable majutsu-jj-integration--jj))
      (majutsu-jj-integration-call source "describe" "-m" "remote base")
      (majutsu-jj-integration-call source "bookmark" "create" "main")
      (majutsu-jj-integration-call source "git" "export")
      (majutsu-jj-integration-call sandbox "git" "clone" source repo)
      (majutsu-jj-integration-call repo "bookmark" "track" "main@origin")
      (let* ((default-directory (file-name-as-directory repo))
             (supported (eq majutsu-build-backend 'jjosh))
             (candidates (majutsu--bookmark-forget-name-candidates)))
        (should (member "main" candidates))
        (should (eq (not (null (member "main@origin" candidates))) supported))
        (if supported
            (let ((process (majutsu-bookmark-forget-ref "main@origin")))
              (while (process-live-p process)
                (accept-process-output process 0.1))
              (should (= (process-exit-status process) 0))
              (should-not (member "main@origin" (majutsu--get-bookmark-names 'remote)))
              (should (member "main" (majutsu--get-bookmark-names 'local)))
              (majutsu-jj-integration-call repo "git" "fetch")
              (should (member "main@origin" (majutsu--get-bookmark-names 'remote))))
          (should-not (fboundp 'majutsu-bookmark-forget-ref))
          (should (member "main@origin" (majutsu--get-bookmark-names 'remote))))
        ;; Local forget remains an upstream command, not a fork-only feature.
        (majutsu-bookmark-forget '("main"))
        (should-not (member "main" (majutsu--get-bookmark-names 'local)))
        (should (member "main@origin" (majutsu--get-bookmark-names 'remote-untracked)))))))

(provide 'majutsu-jjosh-test)
;;; majutsu-jjosh-test.el ends here
