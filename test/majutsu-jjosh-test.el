;;; majutsu-jjosh-test.el --- Tests for fixed backend builds -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'bug-reference)
(require 'majutsu-jjdescription)
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

;;; Bug references

(defun majutsu-jjosh-test--bug-references ()
  "Return (TEXT . URL) for the bug references in the current buffer, by TEXT."
  (bug-reference-fontify (point-min) (point-max))
  (sort (mapcan (lambda (overlay)
                  (and (eq (overlay-get overlay 'category) 'bug-reference)
                       (list (cons (buffer-substring-no-properties
                                    (overlay-start overlay) (overlay-end overlay))
                                   (overlay-get overlay 'bug-reference-url)))))
                (overlays-in (point-min) (point-max)))
        (lambda (a b) (string< (car a) (car b)))))

(ert-deftest majutsu-bug-reference/real-repository-prefers-named-remotes ()
  "References mean the tracker behind the first preferred remote."
  (majutsu-jj-integration-with-repo repo
    (majutsu-jj-integration-call repo "git" "remote" "add" "fork"
                                "git@github.com:me/thing.git")
    (majutsu-jj-integration-call repo "git" "remote" "add" "upstream"
                                "https://codeberg.org/them/thing.git")
    (with-temp-buffer
      (majutsu-mode)
      (let ((inhibit-read-only t))
        (insert "Fix #12 like other/tool#3\n"))
      (bug-reference-mode 1)
      (should (equal (majutsu-jjosh-test--bug-references)
                     '(("#12" . "https://codeberg.org/them/thing/issues/12")
                       ("other/tool#3"
                        . "https://codeberg.org/other/tool/issues/3")))))))

(ert-deftest majutsu-bug-reference/real-jjosh-resolves-by-project ()
  "A reference means the tracker of the project that its revision touches."
  (skip-unless (eq majutsu-build-backend 'jjosh))
  (majutsu-jj-integration-with-repo repo
    (pcase-dolist (`(,name ,path ,url)
                   '(("outer" "outer" "https://github.com/acme/outer.git")
                     ("inner" "outer/inner" "git@github.com:acme/inner.git")
                     ("second" "second" "https://codeberg.org/acme/second.git")
                     ("other" "other" "https://tracker.example/other")
                     ("bare" "bare" nil)))
      (make-directory (expand-file-name path repo) t)
      (majutsu-jj-integration-call repo "project" "add" name "--path" path)
      (when url
        (majutsu-jj-integration-call repo "git" "remote" "add" "origin" url
                                    "--project" name "--whole")))
    (pcase-dolist (`(,message . ,files)
                   '(("Inner #1" "outer/inner/file")
                     ("Outer #2" "outer/file")
                     ("Other T3, not #4" "other/file")
                     ("Two trackers #5" "outer/file" "second/file")
                     ("No tracker #6" "bare/file" "root")
                     ("Tracker and none T7" "bare/file" "other/file")))
      (dolist (file files)
        (majutsu-jj-integration-write-text (expand-file-name file repo) message))
      (majutsu-jj-integration-call repo "commit" "-m" message))
    ;; A merge that changes nothing stands for what its parents touch.
    (majutsu-jj-integration-call repo "new" "root()" "-m" "Side #8")
    (majutsu-jj-integration-write-text (expand-file-name "side" repo) "side")
    (majutsu-jj-integration-call
     repo "new" "description(substring:'Inner #1')" "@" "-m" "Merge pull request #9")
    (let ((majutsu-log-commit-columns
           (eval (car (get 'majutsu-log-commit-columns 'standard-value)) t))
          (majutsu-log--compiled-template-cache nil)
          (majutsu-log-mode-hook '(bug-reference-mode))
          ;; A tracker with a syntax of its own.
          (bug-reference-setup-from-vc-alist
           '(("tracker\\.example/" "\\(\\bT\\([0-9]+\\)\\)"
              "https://tracker.example/T%s"))))
      (save-window-excursion
        (with-current-buffer (majutsu-log)
          (unwind-protect
              (should (equal (majutsu-jjosh-test--bug-references)
                             '(("#1" . "https://github.com/acme/inner/issues/1")
                               ("#2" . "https://github.com/acme/outer/issues/2")
                               ("#9" . "https://github.com/acme/inner/issues/9")
                               ("T3" . "https://tracker.example/T3")
                               ("T7" . "https://tracker.example/T7"))))
            (kill-buffer)))))
    ;; A description buffer names its revision by change id.
    (with-temp-buffer
      (insert "Fixes #10\n\nJJ: Change ID: "
              (string-trim (majutsu-jj-integration-output
                            repo "log" "--no-graph" "-T" "change_id"
                            "-r" "description(substring:'Outer #2')"))
              "\n")
      (majutsu-jjdescription-mode 1)
      (bug-reference-mode 1)
      (should (equal (majutsu-jjosh-test--bug-references)
                     '(("#10" . "https://github.com/acme/outer/issues/10")))))))

(provide 'majutsu-jjosh-test)
;;; majutsu-jjosh-test.el ends here
