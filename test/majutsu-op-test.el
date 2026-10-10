;;; majutsu-op-test.el --- Tests for operation buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <me@0wd0.com>
;; Maintainer: 0WD0 <me@0wd0.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for jj operation integration.

;;; Code:

(require 'ert)
(require 'majutsu-op)
(require 'transient)
(require 'majutsu-evolog)
(require 'majutsu-jj-integration)

(defun majutsu-op-test--summary (&optional change-id change-short commit-id commit-short flags desc)
  "Return a structured op diff commit summary for tests."
  (let ((flags (or flags '("" "" ""))))
    (string-join (list (or change-id "change-full")
                       (or change-short "change-short")
                       (or commit-id "commit-full")
                       (or commit-short "commit-short")
                       (nth 0 flags)
                       (nth 1 flags)
                       (nth 2 flags)
                       (or desc "description"))
                 majutsu-op--field-separator)))

(defun majutsu-op-test--colored-summary ()
  "Return a face-propertized structured op diff commit summary for tests."
  (string-join (list (propertize "change-full" 'font-lock-face 'font-lock-variable-name-face)
                     (propertize "change-short" 'font-lock-face 'font-lock-variable-name-face)
                     (propertize "commit-full" 'font-lock-face 'font-lock-function-name-face)
                     (propertize "commit-short" 'font-lock-face 'font-lock-function-name-face)
                     ""
                     ""
                     "empty"
                     (propertize "description" 'font-lock-face 'font-lock-string-face))
               majutsu-op--field-separator))

(defun majutsu-op-test--log-payload (&rest fields)
  "Join visible operation log row FIELDS."
  (mapconcat
   (lambda (field)
     (replace-regexp-in-string
      "[\x1d-\x1f]" (lambda (s) (substring (json-serialize s) 1 -1))
      field t t))
   fields majutsu-row-field-separator))

(defun majutsu-op-test--log-raw-entry (&rest kvs)
  "Return one row encoded operation log entry from KVS."
  (let* ((graph-prefix (or (plist-get kvs :graph-prefix) ""))
         (body-prefix (or (plist-get kvs :body-prefix) ""))
         (op-id (or (plist-get kvs :op-id) "full-id"))
         (op-id-short (or (plist-get kvs :op-id-short) "short-id"))
         (root-marker (or (plist-get kvs :root-marker) ""))
         (user (or (plist-get kvs :user) "user"))
         (workspace (or (plist-get kvs :workspace) "workspace"))
         (time (or (plist-get kvs :time) "2026-05-02 04:50:00"))
         (time-ago (or (plist-get kvs :time-ago) "2 minutes ago"))
         (duration (or (plist-get kvs :duration) "3 milliseconds"))
         (time-range (or (plist-get kvs :time-range)
                         (concat time-ago ", lasted " duration)))
         (kind (or (plist-get kvs :kind) "op"))
         (current (or (plist-get kvs :current) ""))
         (desc (or (plist-get kvs :desc) "description"))
         (attributes (or (plist-get kvs :attributes) "args: jj op")))
    (concat graph-prefix
            majutsu-row-start-token
            (majutsu-op-test--log-payload
             op-id-short root-marker user workspace time-range)
            majutsu-row-tail-token
            majutsu-row-body-token
            (mapconcat
             (lambda (field)
               (if (string-blank-p field) ""
                 (concat "\n" body-prefix
                         (replace-regexp-in-string
                          "\n" (concat "\n" body-prefix)
                          (majutsu-op-test--log-payload field)
                          t t))))
             (list desc attributes) majutsu-row-field-separator)
            majutsu-row-meta-token
            (mapconcat #'json-serialize
                       (list op-id op-id-short kind current user workspace
                             time time-ago duration desc attributes)
                       majutsu-row-field-separator)
            majutsu-row-end-token
            "\n")))

(defun majutsu-op-test--parse-log-entries (raw)
  "Parse operation row protocol RAW without running jj."
  (with-temp-buffer
    (insert raw)
    (goto-char (point-min))
    (majutsu-row-parse-buffer (majutsu-op-log--ensure-template))))

(ert-deftest majutsu-op-parse-log-entries/preserves-canonical-metadata ()
  "Parser should keep machine metadata and complete operation attributes."
  (let* ((attributes (concat "args: jj op" majutsu-row-field-separator
                             "log\nclient: test"))
         (raw (majutsu-op-test--log-raw-entry
               :graph-prefix "@  " :body-prefix "│  "
               :op-id (propertize "full-operation-id" 'font-lock-face 'error)
               :op-id-short (propertize "short-id" 'font-lock-face 'success)
               :kind "snapshot"
               :current "t"
               :attributes attributes))
         (entry (car (majutsu-op-test--parse-log-entries raw)))
         (short-column
          (seq-find (lambda (column)
                      (and (eq (plist-get column :field) 'op-id-short)
                           (eq (plist-get column :module) 'heading)))
                    (plist-get (majutsu-op-log--ensure-template) :columns)))
         (short (majutsu-row-column-value entry short-column)))
    (should (equal (majutsu-row-column entry 'op-id) "full-operation-id"))
    (should (equal (majutsu-row-column entry 'op-id-short) "short-id"))
    (should (equal short "short-id"))
    (should (get-text-property 0 'font-lock-face short))
    (should (equal (majutsu-row-column entry 'time) "2026-05-02 04:50:00"))
    (should (equal (majutsu-row-column entry 'duration) "3 milliseconds"))
    (should (equal (majutsu-row-column entry 'kind) "snapshot"))
    (should (equal (majutsu-row-column entry 'current) "t"))
    (should (equal (majutsu-row-column entry 'description) "description"))
    (should (equal (majutsu-row-column entry 'attributes) attributes))))

(ert-deftest majutsu-op-parse-log-entries/renders-root-like-jj ()
  "Root operations should omit meaningless user, workspace, and time data."
  (let* ((raw (majutsu-op-test--log-raw-entry
               :op-id "0000000000000000"
               :op-id-short "000000000000"
               :root-marker "root()"
               :user ""
               :workspace ""
               :time-range ""
               :kind "root"
               :desc ""
               :attributes ""))
         (entry (car (majutsu-op-test--parse-log-entries raw)))
         (compiled (majutsu-op-log--ensure-template)))
    (should (equal (substring-no-properties
                    (majutsu-row-render-heading-content entry compiled))
                   "000000000000 root()"))
    (should-not (majutsu-row-render-body entry compiled))
    (should (equal (majutsu-row-column entry 'kind) "root"))))

(ert-deftest majutsu-op-parse-log-entries/preserves-graph-transitions ()
  "Graph-only transitions remain plain events between operation rows."
  (let* ((raw (concat
               (majutsu-op-test--log-raw-entry
                :graph-prefix "@    " :body-prefix "│    "
                :op-id "current" :op-id-short "cur")
               "├─╮\n"
               (majutsu-op-test--log-raw-entry
                :graph-prefix "○ │  " :body-prefix "│ │  "
                :op-id "parent" :op-id-short "par")))
         (parsed (with-temp-buffer
                   (insert raw)
                   (majutsu-row-read-buffer (majutsu-op-log--ensure-template))))
         (entries (plist-get parsed :entries)))
    (should (= (length entries) 2))
    (should (equal (plist-get parsed :events)
                   (list (car entries) "├─╮\n" (cadr entries))))
    (should (equal (plist-get (cadr entries) :heading-prefixes) '("○ │  ")))
    (should (equal (mapcar (lambda (entry)
                             (majutsu-row-column entry 'op-id))
                           entries)
                   '("current" "parent")))))

(ert-deftest majutsu-op-parse-log-entries/rejects-malformed-records ()
  "Malformed records should be ignored instead of creating bad sections."
  (should-not (majutsu-op-test--parse-log-entries "only-one-field\n")))

(ert-deftest majutsu-op-log-validate-args/normalizes-native-list-options ()
  "Operation log should accept only its native list-formatting options."
  (should (equal (majutsu-op-log--validate-args
                  '("--limit=0" "--reversed" "-G"))
                 '("--limit=0" "--reversed" "--no-graph")))
  (should (equal (majutsu-op-log--validate-args '("-n" "02"))
                 '("--limit=02")))
  (should (equal (majutsu-op-log--validate-args '("--limit" "3"))
                 '("--limit=3"))))

(ert-deftest majutsu-op-log-validate-args/rejects-protocol-options ()
  "Operation log should reject options that can corrupt Majutsu rows."
  (dolist (args '(("--limit=-1")
                  ("--limit=x")
                  ("--limit")
                  ("-n")
                  ("-n" "-1")
                  ("-T" "builtin_op_log_compact")
                  ("--template=x")
                  ("--patch")
                  ("--git")
                  ("--color=never")
                  ("--limit=2" "--limit=3")
                  ("--reversed" "--reversed")
                  ("-G" "--no-graph")))
    (should-error (majutsu-op-log--validate-args args) :type 'user-error)))

(ert-deftest majutsu-op-parse-metadata-line/parses-record ()
  "The internal metadata parser should keep full operation ids."
  (let* ((line (string-join '("full-id" "short-id" "user" "workspace"
                              "start" "end" "duration" "op" "desc")
                            majutsu-op--field-separator))
         (entry (majutsu-op--parse-metadata-line line)))
    (should (equal (plist-get entry :op-id) "full-id"))
    (should (equal (plist-get entry :op-id-short) "short-id"))
    (should (equal (plist-get entry :start-time) "start"))
    (should (equal (plist-get entry :desc) "desc"))))

(ert-deftest majutsu-op-metadata/skips-diagnostics-before-record ()
  "Warnings before metadata do not become the operation identity."
  (cl-letf (((symbol-function 'majutsu-jj-buffer-string)
             (lambda (&rest _args)
               (concat "warning before metadata\n"
                       (string-join
                        '("full-id" "short-id" "user" ""
                          "start" "end" "duration" "op" "desc")
                        majutsu-op--field-separator)
                       "\n"))))
    (let ((metadata (majutsu-op--metadata "abc")))
      (should (equal (plist-get metadata :op-id) "full-id"))
      (should (equal (plist-get metadata :workspace) "")))))

(ert-deftest majutsu-op-wash-diff-output/parses-colored-commit-line ()
  "The production washer preserves colors and actionable commit identity."
  (with-temp-buffer
    (majutsu-op-diff-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (root)
        (insert "From operation: from\n  To operation: to\n\n"
                "Changed commits:\n"
                (propertize "+" 'font-lock-face 'diff-added)
                " " (majutsu-op-test--colored-summary) "\n")
        (majutsu-op--wash-diff-output nil)))
    (let* ((group (car (oref magit-root-section children)))
           (entry (car (oref group children)))
           (value (oref entry value)))
      (should (eq (plist-get (oref group value) :kind) 'commits))
      (should (eq (oref entry type) 'jj-op-commit-line))
      (should (equal (plist-get value :marker) "+"))
      (should (equal (plist-get value :change-id) "change-full"))
      (should (equal (plist-get value :commit-id) "commit-full"))
      (should (plist-get value :empty))
      (goto-char (point-min))
      (forward-line 1)
      (should (equal (majutsu-revision-at-point) "commit-full"))
      (search-forward "change-short")
      (should (eq (get-text-property (1- (point)) 'font-lock-face)
                  'font-lock-variable-name-face))
      (should-not (string-match-p "From operation:" (buffer-string)))
      (should-not (string-match-p (regexp-quote majutsu-op--field-separator)
                                  (buffer-string))))))

(ert-deftest majutsu-op-parse-commit-summary/preserves-description-separators ()
  "The final description field should retain embedded record separators."
  (let* ((description (concat "part one" majutsu-op--field-separator
                              "part two"))
         (entry (majutsu-op--parse-commit-summary
                 (majutsu-op-test--summary nil nil nil nil nil description))))
    (should (equal (plist-get entry :description) description))))

(ert-deftest majutsu-op-wash-diff-output/parses-ref-prefixes-and-absent ()
  "The production washer creates ref groups, target rows and absent rows."
  (with-temp-buffer
    (majutsu-op-diff-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (root)
        (insert "Changed remote bookmarks:\n"
                "main@origin:\n"
                "+ tracked " (majutsu-op-test--summary) "\n"
                "- untracked (absent)\n"
                "other@origin:\n"
                "+ tracked " (majutsu-op-test--summary
                               nil nil "other-commit" "other-short") "\n"
                "\nChanged local tags:\n"
                "v1.0:\n+ (added) " (majutsu-op-test--summary) "\n")
        (majutsu-op--wash-diff-output nil)))
    (let* ((groups (oref magit-root-section children))
           (group (car groups))
           (refs (oref group children))
           (ref (car refs))
           (entries (oref ref children))
           (added (oref (nth 0 entries) value))
           (removed (oref (nth 1 entries) value)))
      (should (equal (length groups) 2))
      (should (eq (plist-get (oref group value) :kind) 'remote-bookmarks))
      (should (equal (mapcar (lambda (section) (oref section value)) refs)
                     '("main@origin" "other@origin")))
      (should (equal (plist-get added :ref-name) "main@origin"))
      (should (equal (plist-get added :prefix) "tracked"))
      (should (equal (plist-get added :commit-id) "commit-full"))
      (should (equal (plist-get removed :prefix) "untracked"))
      (should (plist-get removed :absent))
      (should-not (oref (nth 1 entries) children))
      (goto-char (oref (nth 0 entries) start))
      (should (equal (majutsu-revision-at-point) "commit-full"))
      (goto-char (oref (car (oref (nth 1 refs) children)) start))
      (should (equal (majutsu-revision-at-point) "other-commit")))))

(ert-deftest majutsu-op-format-diff-line/hides-machine-separators ()
  "Formatted operation diff lines should not expose machine separators."
  (let* ((value (append '(:marker "+")
                        (majutsu-op--parse-commit-summary
                         (majutsu-op-test--summary))))
         (line (majutsu-op--format-diff-line value)))
    (should (string-match-p "change-short commit-short" line))
    (should-not (string-match-p (regexp-quote majutsu-op--field-separator)
                                line))))

(ert-deftest majutsu-op-log/rejects-unsafe-args-before-opening-buffer ()
  "Operation log should validate row-protocol ownership before setup."
  (let (opened)
    (cl-letf (((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (&rest _args) (setq opened t))))
      (should-error (majutsu-op-log '("--template=bad")) :type 'user-error)
      (should-not opened))))

(ert-deftest majutsu-op-diff-evolog-at-point/uses-change-and-commit-id ()
  "Evolog action should pass the union revset for rewritten/hidden changes."
  (let ((value '(:marker "+" :change-id "full-change" :commit-id "full-commit")))
    (with-temp-buffer
      (majutsu-op-diff-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (jj-op-commit-line value)
          (magit-insert-heading "line")))
      (goto-char (point-min))
      (let (captured)
        (cl-letf (((symbol-function 'majutsu-evolog)
                   (lambda (revset &rest _)
                     (setq captured revset))))
          (majutsu-op-diff-evolog-at-point))
        (should (equal captured "change_id(full-change) | commit_id(full-commit)"))))))

(ert-deftest majutsu-op-diff-default-action/dispatches-by-section ()
  "The default action should open evolog only for changed revision lines."
  (let (actions)
    (cl-letf (((symbol-function 'majutsu-op-diff-evolog-at-point)
               (lambda () (push 'evolog actions)))
              ((symbol-function 'majutsu-visit-thing)
               (lambda () (push 'visit actions))))
      (with-temp-buffer
        (majutsu-op-diff-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (jj-op-commit-line '(:commit-id "commit"))
            (magit-insert-heading "changed line")))
        (goto-char (point-min))
        (majutsu-op-diff-default-action))
      (with-temp-buffer
        (majutsu-op-diff-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (jj-op-group 'commits)
            (magit-insert-heading "Changed commits:")))
        (goto-char (point-min))
        (majutsu-op-diff-default-action)))
    (should (equal (nreverse actions) '(evolog visit)))))

(ert-deftest majutsu-op-diff-refresh-buffer/renders-explicit-endpoint-metadata ()
  "Explicit operation ranges should show metadata for both endpoints."
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-op--metadata)
               (lambda (operation)
                 (push operation seen)
                 (list :op-id (concat operation "-full")
                       :op-id-short (concat operation "-short")
                       :user "user"
                       :workspace "workspace"
                       :end-time "end"
                       :desc "description")))
              ((symbol-function 'majutsu-op--insert-operation-diff)
               #'ignore))
      (with-temp-buffer
        (majutsu-op-diff-mode)
        (setq majutsu-op-diff--args '("--from=a" "--to=b"))
        (let ((inhibit-read-only t))
          (majutsu-op-diff-refresh-buffer))
        (let ((content (buffer-substring-no-properties
                        (point-min) (point-max))))
          (should (string-match-p "From operation a-short" content))
          (should (string-match-p "To operation b-short" content))
          (should (string-match-p "Id: a-full" content))
          (should (string-match-p "Description: description" content)))
        (goto-char (point-min))
        (search-forward "From operation")
        (should (equal (majutsu-op--operation-at-point) "a-full"))
        (search-forward "To operation")
        (should (equal (majutsu-op--operation-at-point) "b-full"))))
    (should (equal (nreverse seen) '("a" "b")))))

(ert-deftest majutsu-history-views/integration-do-not-snapshot-working-copy ()
  "History views render stored identities and patches without recording edits."
  (majutsu-jj-integration-with-repo repo
    (let* ((file (majutsu-jj-integration-file repo "f.txt"))
           (before (majutsu-jj-operation-id repo)))
      (majutsu-jj-integration-write-text file "recorded\n")
      (majutsu-jj-integration-call repo "status")
      (let ((commit (majutsu-jj-integration-output
                     repo "log" "--no-graph" "-r" "@" "-T" "commit_id"))
            (operation (majutsu-jj-operation-id repo))
            (process-environment (cons "COLUMNS=40" process-environment))
            (majutsu-jj-global-arguments
             '("--no-pager" "--color=always" "--config=ui.log-word-wrap=true")))
        (majutsu-jj-integration-write-text file "pending local edits\n")
        (pcase-dolist
            (`(,mode ,refresh ,type ,target)
             `((majutsu-op-log-mode majutsu-op-log-refresh-buffer jj-op ,operation)
               (majutsu-evolog-mode majutsu-evolog-refresh-buffer jj-evolog-entry ,commit)))
          (with-temp-buffer
            (funcall mode)
            (if (eq mode 'majutsu-op-log-mode)
                (setq-local majutsu-op-log--args '("--limit=1"))
              (setq-local majutsu-evolog--revset "@"
                          majutsu-evolog--args '("--limit=1")))
            (let ((inhibit-read-only t)
                  targets)
              (funcall refresh)
              (magit-map-sections
               (lambda (section)
                 (when (magit-section-match type section)
                   (push (oref section value) targets)))
               magit-root-section)
              (should (equal targets (list target))))))
        (with-temp-buffer
          (majutsu-op-diff-mode)
          (setq-local majutsu-op-diff--args
                      (list (concat "--from=" before) (concat "--to=" operation)))
          (let ((inhibit-read-only t)
                commits endpoints)
            (majutsu-op-diff-refresh-buffer)
            (magit-map-sections
             (lambda (section)
               (pcase (oref section type)
                 ('jj-op-commit-line
                  (push (plist-get (oref section value) :commit-id) commits))
                 ('jj-op-diff-endpoint
                  (push (plist-get (oref section value) :op-id) endpoints))))
             magit-root-section)
            (should (member commit commits))
            (should (equal (sort endpoints #'string-lessp)
                           (sort (list before operation) #'string-lessp)))))
        (with-temp-buffer
          (majutsu-evolog-diff-mode)
          (setq-local majutsu-evolog-diff--commit-id commit)
          (let ((inhibit-read-only t))
            (majutsu-evolog-diff-refresh-buffer))
          (should (magit-get-section
                   `((jj-file . "f.txt") (diff-root) (jj-evolog-diff . ,commit))))
          (should (string-match-p "recorded" (buffer-string)))
          (should-not (string-match-p "pending" (buffer-string))))
        (should (equal
                 (majutsu-jj-integration-output
                  repo "--ignore-working-copy" "op" "log" "--no-graph" "-n" "1" "-T" "id")
                 operation))))))

(ert-deftest majutsu-op/integration-confirmation-controls-mutations ()
  "Declining preserves repository state; accepting actually restores/reverts it."
  (majutsu-jj-integration-with-sandbox sandbox
    (dolist (function '(majutsu-op-restore majutsu-op-revert))
      (let* ((repo (majutsu-jj-integration-init sandbox (symbol-name function)))
             (default-directory (file-name-as-directory repo))
             (majutsu-jj-executable majutsu-jj-integration--jj)
             (before (majutsu-jj-operation-id repo)))
        (majutsu-jj-integration-call repo "describe" "-m" "changed")
        (let* ((after (majutsu-jj-operation-id repo))
               (target (if (eq function 'majutsu-op-restore) before after)))
          (cl-letf (((symbol-function 'majutsu-confirm) (lambda (&rest _) nil)))
            (funcall function target))
          (should (equal (majutsu-jj-operation-id repo) after))
          (cl-letf (((symbol-function 'majutsu-confirm) (lambda (&rest _) t)))
            (funcall function target))
          (should (equal
                   (majutsu-jj-integration-output
                    repo "--ignore-working-copy" "log" "--no-graph" "-r" "@" "-T" "description")
                   "")))))))

;;; _
(provide 'majutsu-op-test)
;;; majutsu-op-test.el ends here
