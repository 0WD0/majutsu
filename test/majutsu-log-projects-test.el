;;; majutsu-log-projects-test.el --- Optional project log columns -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'majutsu-log)
(require 'majutsu-jj-integration)

(ert-deftest majutsu-log-projects/capabilities-are-cached-per-program-context ()
  "Do not reuse a capability across hosts or binary replacements."
  (let ((majutsu-log--touched-projects-support-cache (make-hash-table :test #'equal))
        (key '(local old))
        supported calls)
    (cl-letf (((symbol-function 'majutsu-log--template-program-key)
               (lambda () key))
              ((symbol-function 'majutsu-process-jj)
               (lambda (_destination &rest args)
                 (setq args (flatten-tree args))
                 (push args calls)
                 (should (member "--ignore-working-copy" args))
                 (should (member "--at-op=@" args))
                 (should (member "none()" args))
                 (if (or supported (equal (car (last args)) "\"\"")) 0 1))))
      (should-not (majutsu-log--touched-projects-supported-p))
      (should-not (majutsu-log--touched-projects-supported-p))
      (should (= (length calls) 2))
      (setq key '(remote old) supported t)
      (should (majutsu-log--touched-projects-supported-p))
      (should (majutsu-log--touched-projects-supported-p))
      (should (= (length calls) 3))
      (setq key '(local rebuilt))
      (should (majutsu-log--touched-projects-supported-p))
      (should (= (length calls) 4)))))

(ert-deftest majutsu-log-projects/probe-errors-are-not-cached-as-unsupported ()
  "A temporarily unreadable repository must not permanently disable the column."
  (let ((majutsu-log--touched-projects-support-cache (make-hash-table :test #'equal))
        (status 128) (calls 0))
    (cl-letf (((symbol-function 'majutsu-log--template-program-key)
               (lambda () '(context)))
              ((symbol-function 'majutsu-process-jj)
               (lambda (&rest _) (cl-incf calls) status)))
      (should-not (majutsu-log--touched-projects-supported-p))
      (should (= (hash-table-count majutsu-log--touched-projects-support-cache) 0))
      (setq status 0)
      (should (majutsu-log--touched-projects-supported-p))
      (should (majutsu-log--touched-projects-supported-p))
      (should (= calls 3)))))

(ert-deftest majutsu-log-projects/program-key-follows-in-place-upgrades ()
  "Replacing a binary at the same configured path invalidates its capability."
  (let ((binary (make-temp-file "majutsu-program-")))
    (unwind-protect
        (let ((majutsu-jj-executable binary))
          (set-file-modes binary #o700)
          (let ((before (majutsu-log--template-program-key)))
            (should before)
            (with-temp-file binary (insert "new executable contents\n"))
            (should-not (equal before (majutsu-log--template-program-key)))))
      (delete-file binary))))

(ert-deftest majutsu-log-projects/compiler-switches-schema-and-honors-opt-out ()
  "Template and parser must agree when switching between jjosh and plain jj."
  (let ((majutsu-log-commit-columns
         '((:field touched-projects :module heading
            :template majutsu-log-template-touched-projects)
           (:field id :module metadata :template [:commit_id])))
        (majutsu-log--compiled-template-cache nil)
        (supported t))
    (cl-letf (((symbol-function 'majutsu-log--touched-projects-supported-p)
               (lambda () supported)))
      (let ((with-projects (majutsu-log--ensure-template)))
        (should (string-match-p "self.touched_projects()" (plist-get with-projects :template)))
        (should (= (length (plist-get with-projects :columns)) 2))
        (should (eq with-projects (majutsu-log--ensure-template)))
        (setq supported nil)
        (let ((plain (majutsu-log--ensure-template)))
          (should-not (string-match-p "touched_projects" (plist-get plain :template)))
          (should (= (length (plist-get plain :columns)) 1)))
        (setq supported t)
        (should (plist-get (majutsu-log--ensure-template) :touched-projects))))
    (let ((majutsu-log-commit-columns (cdr majutsu-log-commit-columns)))
      (cl-letf (((symbol-function 'majutsu-log--touched-projects-supported-p)
                 (lambda () (ert-fail "No probe needed without the project column"))))
        (should-not (plist-get (majutsu-log--ensure-template) :touched-projects))))))

(ert-deftest majutsu-log-projects/real-template-parser-and-renderer ()
  "Exercise the row protocol with either real jj or project-aware jjosh."
  (majutsu-jj-integration-with-repo repo
    (let* ((majutsu-log-commit-columns
            (eval (car (get 'majutsu-log-commit-columns 'standard-value)) t))
           (majutsu-log--compiled-template-cache nil)
           (supported (majutsu-log--touched-projects-supported-p)))
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

(provide 'majutsu-log-projects-test)
;;; majutsu-log-projects-test.el ends here
