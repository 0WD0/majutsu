;;; majutsu-op-test.el --- Tests for operation buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for jj operation integration.

;;; Code:

(require 'ert)
(require 'majutsu-op)
(require 'transient)

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
  "Join operation log row FIELDS."
  (string-join fields majutsu-row-field-separator))

(defun majutsu-op-test--log-raw-entry (&rest kvs)
  "Return one row encoded operation log entry from KVS."
  (let* ((op-id (or (plist-get kvs :op-id) "full-id"))
         (op-id-short (or (plist-get kvs :op-id-short) "short-id"))
         (current (or (plist-get kvs :current) "@"))
         (user (or (plist-get kvs :user) "user"))
         (workspace (or (plist-get kvs :workspace) "workspace"))
         (time (or (plist-get kvs :time) "2026-05-02 04:50:00"))
         (time-ago (or (plist-get kvs :time-ago) "2 minutes ago"))
         (duration (or (plist-get kvs :duration) "3 milliseconds"))
         (kind (or (plist-get kvs :kind) "op"))
         (desc (or (plist-get kvs :desc) "description"))
         (tags (or (plist-get kvs :tags) "args: jj op")))
    (concat majutsu-row-start-token
            (majutsu-op-test--log-payload current op-id-short kind desc)
            majutsu-row-tail-token
            majutsu-row-body-token
            (majutsu-op-test--log-payload
             (concat "Id: " op-id)
             (concat "User: " user)
             (concat "Workspace: " workspace)
             (concat "Time: " time " (" time-ago "), lasted " duration)
             tags)
            majutsu-row-meta-token
            (majutsu-op-test--log-payload
             op-id user workspace time time-ago duration)
            majutsu-row-end-token
            "\n")))

(ert-deftest majutsu-op-log-template/carries-rich-line-fields ()
  "Operation log template should carry ids, current marker, time, and duration."
  (let ((template (plist-get (majutsu-op-log--ensure-template) :template)))
    (should (string-match-p (regexp-quote "\\x1dS") (prin1-to-string template)))
    (should (string-match-p "self.id()" template))
    (should (string-match-p "self.id().short()" template))
    (should (string-match-p "self.current_operation()" template))
    (should (string-match-p "self.time().end().format" template))
    (should (string-match-p "self.time().duration()" template))
    (should (string-match-p "self.description().first_line()" template))
    (should (string-match-p "self.tags().first_line()" template))
    (should-not (string-match-p "separate(" template))))

(ert-deftest majutsu-op-commit-summary-template/carries-full-ids ()
  "Operation diff commit summary should carry full ids for actions."
  (should (string-match-p "self.change_id()"
                          majutsu-op--commit-summary-template))
  (should (string-match-p "self.change_id().short()"
                          majutsu-op--commit-summary-template))
  (should (string-match-p "self.commit_id()"
                          majutsu-op--commit-summary-template))
  (should (string-match-p "self.commit_id().short()"
                          majutsu-op--commit-summary-template))
  (should-not (string-match-p (regexp-quote "\"(empty)\"")
                              majutsu-op--commit-summary-template))
  (should-not (string-match-p "separate(" majutsu-op--commit-summary-template)))

(ert-deftest majutsu-op-parse-log-entries/strips-machine-id-and-keeps-display-face ()
  "Parser should strip machine ids and keep display field faces."
  (let* ((raw (majutsu-op-test--log-raw-entry
               :op-id (propertize "full-operation-id" 'font-lock-face 'error)
               :op-id-short (propertize "short-id" 'font-lock-face 'success)))
         (entry (car (majutsu-parse-op-log-entries nil raw)))
         (short (plist-get entry :op-id-short)))
    (should (equal (plist-get entry :op-id) "full-operation-id"))
    (should (equal short "short-id"))
    (should (get-text-property 0 'font-lock-face short))
    (should (equal (plist-get entry :current) "@"))
    (should (equal (plist-get entry :time) "2026-05-02 04:50:00"))
    (should (equal (plist-get entry :duration) "3 milliseconds"))
    (should (equal (plist-get entry :kind) "op"))
    (should (equal (plist-get entry :desc) "description"))
    (should (equal (plist-get entry :tags) "args: jj op"))))

(ert-deftest majutsu-op-parse-log-entries/rejects-malformed-records ()
  "Malformed records should be ignored instead of creating bad sections."
  (should-not (majutsu-parse-op-log-entries nil "only-one-field\n")))

(ert-deftest majutsu-op-log-command-args/uses-read-only-top-level-args ()
  "Operation log command args should avoid snapshotting the working copy."
  (let ((args (majutsu-op--log-command-args '("--limit=2" "--reversed"))))
    (should (member "--at-op=@" args))
    (should (member "--ignore-working-copy" args))
    (should (< (cl-position "--at-op=@" args :test #'equal)
               (cl-position "op" args :test #'equal)))
    (should (member "--limit=2" args))
    (should (member "--reversed" args))))

(ert-deftest majutsu-op-parse-log-entries/uses-buffer-args-for-jj-call ()
  "Operation log parser should pass remembered buffer args to jj."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu-jj-buffer-string)
               (lambda (&rest args)
                 (setq captured args)
                 (majutsu-op-test--log-raw-entry
                  :op-id "full"
                  :op-id-short "short"
                  :current ""
                  :time "time"
                  :time-ago "ago"
                  :duration "duration"
                  :desc "desc"))))
      (with-temp-buffer
        (majutsu-op-log-mode)
        (setq majutsu-op-log--args '("--limit=2" "--reversed"))
        (should (majutsu-parse-op-log-entries))
        (should (equal captured
                       (majutsu-op--log-command-args
                        '("--limit=2" "--reversed"))))))))

(ert-deftest majutsu-op-log-insert-entries/renders-multiline-expanded-entries ()
  "Operation log entries should render useful multiline details by default."
  (let ((entry (list :op-id "full-id"
                     :op-id-short "short-id"
                     :current "@"
                     :kind "snapshot"
                     :user "user"
                     :workspace "workspace"
                     :time "2026-05-02 04:50:00"
                     :time-ago "2 minutes ago"
                     :duration "3 milliseconds"
                     :desc "description"
                     :tags "args: jj op")))
    (cl-letf (((symbol-function 'majutsu-jj-wash)
               (lambda (washer _keep-error &rest _args)
                 (insert (majutsu-op-test--log-raw-entry
                          :op-id "full-id"
                          :op-id-short "short-id"
                          :current "@"
                          :kind "snapshot"))
                 (funcall washer nil)
                 0)))
      (with-temp-buffer
        (majutsu-op-log-mode)
        (let ((inhibit-read-only t))
          (majutsu-op-log-insert-entries))
        (let ((content (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "@ short-id" content))
          (should (string-match-p "description" content))
          (should (string-match-p "Id: full-id" content))
          (should (string-match-p "Time: 2026-05-02 04:50:00" content))
          (should (string-match-p "args: jj op" content))
          (should-not (string-match-p "\n\n\n" content)))
        (goto-char (point-min))
        (search-forward "short-id")
        (should-not (oref (magit-current-section) hidden))))))

(ert-deftest majutsu-op-parse-show-line/parses-metadata-record ()
  "Operation show metadata parser should keep full operation ids."
  (let* ((line (string-join '("full-id" "short-id" "user" "workspace"
                              "start" "end" "duration" "op" "desc")
                            majutsu-op--field-separator))
         (entry (majutsu-op--parse-show-line line)))
    (should (equal (plist-get entry :op-id) "full-id"))
    (should (equal (plist-get entry :op-id-short) "short-id"))
    (should (equal (plist-get entry :start-time) "start"))
    (should (equal (plist-get entry :desc) "desc"))))

(ert-deftest majutsu-op-show-output/uses-read-only-top-level-args ()
  "Operation show metadata/body commands should avoid working-copy snapshots."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu-jj-buffer-string)
               (lambda (&rest args)
                 (setq captured args)
                 "")))
      (majutsu-op--show-output "abc" "template")
      (should (member "--at-op=@" captured))
      (should (member "--ignore-working-copy" captured))
      (should (< (cl-position "--at-op=@" captured :test #'equal)
                 (cl-position "op" captured :test #'equal)))
      (should (equal (last captured 6)
                     '("op" "show" "abc" "--no-op-diff" "-T" "template"))))))

(ert-deftest majutsu-op-parse-diff-output/parses-colored-commit-line ()
  "Operation diff parser should use plain marker detection and colored fields."
  (let* ((line (concat (propertize "+" 'font-lock-face 'diff-added)
                       " "
                       (majutsu-op-test--colored-summary)))
         (nodes (majutsu-op--parse-diff-output
                 (concat "From operation: from\n  To operation: to\n\n"
                         "Changed commits:\n" line "\n")))
         (group (car nodes))
         (entry (car (plist-get group :children)))
         (value (plist-get entry :value)))
    (should (equal (plist-get group :kind) 'commits))
    (should (equal (plist-get entry :type) 'commit-line))
    (should (equal (plist-get value :marker) "+"))
    (should (equal (plist-get value :change-id) "change-full"))
    (should (equal (plist-get value :commit-id) "commit-full"))
    (should (plist-get value :empty))
    (should (get-text-property 0 'font-lock-face
                               (plist-get value :change-id-short-display)))))

(ert-deftest majutsu-op-parse-diff-output/parses-ref-prefixes-and-absent ()
  "Operation diff parser should parse ref target prefixes and absent values."
  (let* ((nodes (majutsu-op--parse-diff-output
                 (concat "Changed remote bookmarks:\n"
                         "main@origin:\n"
                         "+ tracked " (majutsu-op-test--summary) "\n"
                         "- untracked (absent)\n")))
         (group (car nodes))
         (ref (car (plist-get group :children)))
         (entries (plist-get ref :children))
         (added (plist-get (nth 0 entries) :value))
         (removed (plist-get (nth 1 entries) :value)))
    (should (equal (plist-get group :kind) 'remote-bookmarks))
    (should (equal (plist-get ref :name) "main@origin"))
    (should (equal (plist-get added :prefix) "tracked"))
    (should (equal (plist-get added :commit-id) "commit-full"))
    (should (equal (plist-get removed :prefix) "untracked"))
    (should (plist-get removed :absent))))

(ert-deftest majutsu-op-format-diff-line/hides-machine-separators ()
  "Formatted operation diff lines should not expose machine separators."
  (let* ((value (append '(:marker "+")
                        (majutsu-op--parse-commit-summary
                         (majutsu-op-test--summary))))
         (line (majutsu-op--format-diff-line value)))
    (should (string-match-p "change-short commit-short" line))
    (should-not (string-match-p (regexp-quote majutsu-op--field-separator)
                                line))))

(ert-deftest majutsu-op-log/passes-args-binding-to-buffer-setup ()
  "majutsu-op-log should remember transient args in the buffer."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (mode locked bindings &rest kwargs)
                 (setq captured (list mode locked bindings kwargs))
                 'buffer)))
      (should (eq (majutsu-op-log '("--limit=2" "--reversed")) 'buffer))
      (should (eq (nth 0 captured) #'majutsu-op-log-mode))
      (should-not (nth 1 captured))
      (should (equal (nth 2 captured)
                     '((majutsu-op-log--args ("--limit=2" "--reversed")))))
      (should (equal (nth 3 captured)
                     '(:buffer "*majutsu-op: repo*" :directory "/repo/"))))))

(ert-deftest majutsu-op-log/defaults-to-mode-arguments ()
  "Operation log should fall back to per-mode default arguments."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (mode locked bindings &rest kwargs)
                 (setq captured (list mode locked bindings kwargs))
                 'buffer)))
      (should (eq (majutsu-op-log nil) 'buffer))
      (should (equal (nth 2 captured)
                     `((majutsu-op-log--args
                        ,(get 'majutsu-op-log-mode
                              'majutsu-op-log-default-arguments))))))))

(ert-deftest majutsu-op-log-mode/default-limit-stored-on-symbol ()
  "Operation log mode should store --limit=64 as its default argument."
  (should (equal (get 'majutsu-op-log-mode 'majutsu-op-log-default-arguments)
                 '("--limit=64"))))

(ert-deftest majutsu-op-log-transient/init-value-uses-mode-defaults ()
  "Operation log transient should display the mode default limit."
  (let ((obj (make-instance 'majutsu-op-log-prefix
                            :command 'majutsu-op-log-transient)))
    (transient-init-value obj)
    (should (equal (oref obj value) '("--limit=64")))))

(ert-deftest majutsu-op-show/passes-operation-binding-to-buffer-setup ()
  "majutsu-op-show should pass operation as a buffer-local binding."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-op-show--buffer-name)
               (lambda (_operation) "*op*"))
              ((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (mode locked bindings &rest kwargs)
                 (setq captured (list mode locked bindings kwargs))
                 'buffer)))
      (should (eq (majutsu-op-show "abc") 'buffer))
      (should (eq (nth 0 captured) #'majutsu-op-show-mode))
      (should-not (nth 1 captured))
      (should (equal (nth 2 captured)
                     '((majutsu-op-show--operation "abc"))))
      (should (equal (nth 3 captured)
                     '(:buffer "*op*" :directory "/repo/"))))))

(ert-deftest majutsu-op-show-refresh-buffer/inserts-metadata-and-diff-body ()
  "Operation show refresh should insert metadata and body content."
  (cl-letf (((symbol-function 'majutsu-op--show-metadata)
             (lambda (_operation)
               (list :op-id "full-id"
                     :op-id-short "short-id"
                     :user "user"
                     :workspace "workspace"
                     :start-time "start"
                     :end-time "end"
                     :duration "duration"
                     :kind "op")))
            ((symbol-function 'majutsu-op--operation-body)
             (lambda (_operation template)
               (if (string-match-p "tags" template)
                   "args: jj op\n"
                 "description body\n")))
            ((symbol-function 'majutsu-jj-wash)
             (lambda (washer _keep-error &rest _args)
               (insert (concat "Changed commits:\n+ "
                               (majutsu-op-test--summary)
                               "\n"))
               (funcall washer nil)
               0)))
    (with-temp-buffer
      (majutsu-op-show-mode)
      (setq majutsu-op-show--operation "@")
      (let ((inhibit-read-only t))
        (majutsu-op-show-refresh-buffer))
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "User: user" content))
        (should (string-match-p "description body" content))
        (should (string-match-p "args: jj op" content))
        (should (string-match-p "Changed commits" content))
        (should (string-match-p "change-short commit-short" content))
        (should-not (string-match-p (regexp-quote majutsu-op--field-separator)
                                    content)))
      (goto-char (point-min))
      (search-forward "Metadata")
      (should-not (oref (magit-current-section) hidden))
      (search-forward "Changed commits")
      (should-not (oref (magit-current-section) hidden)))))

(ert-deftest majutsu-op-show-refresh-buffer/inserts-line-level-sections ()
  "Operation show refresh should attach section values to parsed diff lines."
  (cl-letf (((symbol-function 'majutsu-op--show-metadata)
             (lambda (_operation)
               (list :op-id "full-id"
                     :op-id-short "short-id"
                     :user "user"
                     :workspace "workspace"
                     :start-time "start"
                     :end-time "end"
                     :duration "duration"
                     :kind "op")))
            ((symbol-function 'majutsu-op--operation-body)
             (lambda (&rest _) ""))
            ((symbol-function 'majutsu-jj-wash)
             (lambda (washer _keep-error &rest _args)
               (insert (concat "From operation: old\n  To operation: new\n\n"
                               "Changed commits:\n"
                               "+ " (majutsu-op-test--summary
                                     "change-full" "change-short"
                                     "commit-full" "commit-short") "\n\n"
                               "Changed local bookmarks:\n"
                               "main:\n"
                               "+ (added) " (majutsu-op-test--summary
                                             "ref-change-full" "ref-change-short"
                                             "ref-commit-full" "ref-commit-short") "\n"
                               "- (absent)\n"))
               (funcall washer nil)
               0)))
    (with-temp-buffer
      (majutsu-op-show-mode)
      (setq majutsu-op-show--operation "@")
      (let ((inhibit-read-only t))
        (majutsu-op-show-refresh-buffer))
      (let ((case-fold-search nil)
            (content (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (string-match-p "^Description$" content))
        (should-not (string-match-p "^Tags$" content))
        (should-not (string-match-p "\n\n\n" content)))
      (goto-char (point-min))
      (search-forward "commit-short")
      (let ((value (majutsu-op--diff-line-at-point)))
        (should (equal (plist-get value :commit-id) "commit-full"))
        (should-not (plist-get value :absent)))
      (search-forward "ref-commit-short")
      (let ((value (majutsu-op--diff-line-at-point)))
        (should (equal (plist-get value :prefix) "(added)"))
        (should (equal (plist-get value :commit-id) "ref-commit-full")))
      (search-forward "(absent)")
      (let ((value (majutsu-op--diff-line-at-point)))
        (should (plist-get value :absent))))))

(ert-deftest majutsu-op-show-diff-at-point/uses-full-commit-id ()
  "Diff action should pass full commit-id revsets to ordinary diff."
  (let ((value '(:marker "+" :commit-id "full-commit")))
    (with-temp-buffer
      (majutsu-op-show-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (jj-op-commit-line value)
          (magit-insert-heading "line")))
      (goto-char (point-min))
      (let (captured)
        (cl-letf (((symbol-function 'majutsu-diff-revset)
                   (lambda (revset &rest _)
                     (setq captured revset))))
          (majutsu-op-show-diff-at-point))
        (should (equal captured "commit_id(full-commit)"))))))

(ert-deftest majutsu-op-show-evolog-at-point/uses-change-and-commit-id ()
  "Evolog action should pass the union revset for rewritten/hidden changes."
  (let ((value '(:marker "+" :change-id "full-change" :commit-id "full-commit")))
    (with-temp-buffer
      (majutsu-op-show-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (jj-op-commit-line value)
          (magit-insert-heading "line")))
      (goto-char (point-min))
      (let (captured)
        (cl-letf (((symbol-function 'majutsu-evolog)
                   (lambda (revset &rest _)
                     (setq captured revset))))
          (majutsu-op-show-evolog-at-point))
        (should (equal captured "change_id(full-change) | commit_id(full-commit)"))))))

(ert-deftest majutsu-op-show-mode-map/line-actions ()
  "Operation show mode should expose line-level actions."
  (should (eq (lookup-key majutsu-op-show-mode-map (kbd "RET"))
              'majutsu-op-show-default-action))
  (should (eq (lookup-key majutsu-op-show-mode-map (kbd "d"))
              'majutsu-diff))
  (should (eq (lookup-key majutsu-op-show-mode-map (kbd "v"))
              'majutsu-op-show-evolog-at-point)))

(ert-deftest majutsu-op-diff-command-args/uses-read-only-top-level-args ()
  "Operation diff command args should avoid snapshotting the working copy."
  (let ((args (majutsu-op--diff-command-args '("--operation=abc"))))
    (should (member "--at-op=@" args))
    (should (member "--ignore-working-copy" args))
    (should (member "--config" args))
    (should (< (cl-position "--at-op=@" args :test #'equal)
               (cl-position "op" args :test #'equal)))
    (should (member "--operation=abc" args))))

(ert-deftest majutsu-op-diff/passes-args-binding-to-buffer-setup ()
  "majutsu-op-diff should remember diff args in the buffer."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-op-diff--buffer-name)
               (lambda () "*op-diff*"))
              ((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (mode locked bindings &rest kwargs)
                 (setq captured (list mode locked bindings kwargs))
                 'buffer)))
      (should (eq (majutsu-op-diff '("--from=a" "--to=b")) 'buffer))
      (should (eq (nth 0 captured) #'majutsu-op-diff-mode))
      (should-not (nth 1 captured))
      (should (equal (nth 2 captured)
                     '((majutsu-op-diff--args ("--from=a" "--to=b")))))
      (should (equal (nth 3 captured)
                     '(:buffer "*op-diff*" :directory "/repo/"))))))

(ert-deftest majutsu-op-diff-refresh-buffer/renders-parsed-diff ()
  "Operation diff refresh should reuse the parsed operation diff sections."
  (cl-letf (((symbol-function 'majutsu-jj-wash)
             (lambda (washer _keep-error &rest _args)
               (insert (concat "Changed commits:\n+ "
                               (majutsu-op-test--summary)
                               "\n"))
               (funcall washer nil)
               0)))
    (with-temp-buffer
      (majutsu-op-diff-mode)
      (setq majutsu-op-diff--args '("--operation=abc"))
      (let ((inhibit-read-only t))
        (majutsu-op-diff-refresh-buffer))
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Operation Diff abc" content))
        (should (string-match-p "change-short commit-short" content))))))

(ert-deftest majutsu-op-restore/runs-confirmed-command ()
  "Operation restore should run jj op restore after confirmation."
  (let (command refreshed)
    (cl-letf (((symbol-function 'majutsu-confirm)
               (lambda (&rest _) t))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq command args)
                 0))
              ((symbol-function 'majutsu-refresh)
               (lambda ()
                 (setq refreshed t))))
      (majutsu-op-restore "abc")
      (should (equal command '("op" "restore" "abc")))
      (should refreshed))))

(ert-deftest majutsu-op-revert/runs-confirmed-command ()
  "Operation revert should run jj op revert after confirmation."
  (let (command refreshed)
    (cl-letf (((symbol-function 'majutsu-confirm)
               (lambda (&rest _) t))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq command args)
                 0))
              ((symbol-function 'majutsu-refresh)
               (lambda ()
                 (setq refreshed t))))
      (majutsu-op-revert "abc")
      (should (equal command '("op" "revert" "abc")))
      (should refreshed))))

(ert-deftest majutsu-op-transient/exposes-operation-family-actions ()
  "Operation transient should expose operation family actions."
  (should (transient-get-suffix 'majutsu-op-transient "l"))
  (should (transient-get-suffix 'majutsu-op-transient "s"))
  (should (transient-get-suffix 'majutsu-op-transient "d"))
  (should (transient-get-suffix 'majutsu-op-transient "u"))
  (should (transient-get-suffix 'majutsu-op-transient "r"))
  (should (transient-get-suffix 'majutsu-op-transient "R"))
  (should (transient-get-suffix 'majutsu-op-transient "V")))

(ert-deftest majutsu-op-log-transient/exposes-log-options ()
  "Operation log transient should expose log-specific options."
  (should (transient-get-suffix 'majutsu-op-log-transient "-n"))
  (should (transient-get-suffix 'majutsu-op-log-transient "-r"))
  (should (transient-get-suffix 'majutsu-op-log-transient "l")))

(ert-deftest majutsu-op-diff-transient/exposes-diff-selection ()
  "Operation diff transient should expose operation range options."
  (should (transient-get-suffix 'majutsu-op-diff-transient "-o"))
  (should (transient-get-suffix 'majutsu-op-diff-transient "-f"))
  (should (transient-get-suffix 'majutsu-op-diff-transient "-t"))
  (should (transient-get-suffix 'majutsu-op-diff-transient "d")))

(ert-deftest majutsu-op-log-mode-map/show-at-point ()
  "Operation log mode should make RET open operation details."
  (should (eq (lookup-key majutsu-op-log-mode-map (kbd "RET"))
              'majutsu-op-log-show-at-point))
  (should (eq (lookup-key majutsu-mode-map (kbd "X"))
              'majutsu-op-transient)))

;;; _
(provide 'majutsu-op-test)
;;; majutsu-op-test.el ends here
