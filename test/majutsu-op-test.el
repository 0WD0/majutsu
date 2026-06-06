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

(require 'ansi-color)
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
         (user (or (plist-get kvs :user) "user"))
         (workspace (or (plist-get kvs :workspace) "workspace"))
         (time (or (plist-get kvs :time) "2026-05-02 04:50:00"))
         (time-ago (or (plist-get kvs :time-ago) "2 minutes ago"))
         (duration (or (plist-get kvs :duration) "3 milliseconds"))
         (kind (or (plist-get kvs :kind) "op"))
         (desc (or (plist-get kvs :desc) "description"))
         (attributes (or (plist-get kvs :attributes)
                         (plist-get kvs :tags)
                         "args: jj op")))
    (concat majutsu-row-start-token
            (majutsu-op-test--log-payload op-id-short kind desc)
            majutsu-row-tail-token
            majutsu-row-body-token
            (majutsu-op-test--log-payload
             (concat "Id: " op-id)
             (concat "User: " user)
             (concat "Workspace: " workspace)
             (concat "Time: " time " (" time-ago "), lasted " duration)
             attributes)
            majutsu-row-meta-token
            (majutsu-op-test--log-payload
             op-id user workspace time time-ago duration)
            majutsu-row-end-token
            "\n")))

(ert-deftest majutsu-op-log-template/carries-rich-line-fields ()
  "Operation log template should carry ids, time, duration, and details."
  (let ((template (plist-get (majutsu-op-log--ensure-template) :template)))
    (should (string-match-p (regexp-quote "\\x1dS") (prin1-to-string template)))
    (should (string-match-p "self.id()" template))
    (should (string-match-p "self.id().short()" template))
    (should-not (string-match-p "self.current_operation()" template))
    (should (string-match-p "self.time().end().format" template))
    (should (string-match-p "self.time().duration()" template))
    (should (string-match-p "self.description().first_line()" template))
    (should (string-match-p "self.attributes().first_line()" template))
    (should-not (string-match-p "self.tags()" template))
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
    (should-not (plist-member entry :current))
    (should (equal (plist-get entry :time) "2026-05-02 04:50:00"))
    (should (equal (plist-get entry :duration) "3 milliseconds"))
    (should (equal (plist-get entry :kind) "op"))
    (should (equal (plist-get entry :desc) "description"))
    (should (equal (plist-get entry :attributes) "args: jj op"))))

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
                     :kind "snapshot"
                     :user "user"
                     :workspace "workspace"
                     :time "2026-05-02 04:50:00"
                     :time-ago "2 minutes ago"
                     :duration "3 milliseconds"
                     :desc "description"
                     :attributes "args: jj op")))
    (cl-letf (((symbol-function 'majutsu-jj-wash)
               (lambda (washer _keep-error &rest _args)
                 (insert (majutsu-op-test--log-raw-entry
                          :op-id "full-id"
                          :op-id-short "short-id"
                          :kind "snapshot"))
                 (funcall washer nil)
                 0)))
      (with-temp-buffer
        (majutsu-op-log-mode)
        (let ((inhibit-read-only t))
          (majutsu-op-log-insert-entries))
        (let ((content (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "short-id" content))
          (should (string-match-p "description" content))
          (should (string-match-p "Id: full-id" content))
          (should (string-match-p "Time: 2026-05-02 04:50:00" content))
          (should (string-match-p "args: jj op" content))
          (should-not (string-match-p "\n\n\n" content)))
        (goto-char (point-min))
        (search-forward "short-id")
        (should-not (oref (magit-current-section) hidden))))))

(ert-deftest majutsu-op-parse-metadata-line/parses-metadata-record ()
  "Operation metadata parser should keep full operation ids."
  (let* ((line (string-join '("full-id" "short-id" "user" "workspace"
                              "start" "end" "duration" "op" "desc")
                            majutsu-op--field-separator))
         (entry (majutsu-op--parse-metadata-line line)))
    (should (equal (plist-get entry :op-id) "full-id"))
    (should (equal (plist-get entry :op-id-short) "short-id"))
    (should (equal (plist-get entry :start-time) "start"))
    (should (equal (plist-get entry :desc) "desc"))))

(ert-deftest majutsu-op-metadata-output/uses-read-only-top-level-args ()
  "Operation metadata queries should avoid working-copy snapshots."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu-jj-buffer-string)
               (lambda (&rest args)
                 (setq captured args)
                 "")))
      (majutsu-op--metadata-output "abc" "template")
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

(ert-deftest majutsu-op-wash-diff-output/parses-actual-ansi-colored-marker ()
  "Operation diff washer should parse marker lines after real ANSI conversion."
  (with-temp-buffer
    (magit-section-mode)
    (setq buffer-read-only nil)
    (insert (concat "Changed commits:\n"
                    "\e[38;5;2m+\e[39m "
                    (majutsu-op-test--summary)
                    "\n"))
    (let ((ansi-color-apply-face-function
           #'ansi-color-apply-text-property-face))
      (ansi-color-apply-on-region (point-min) (point-max)))
    (goto-char (point-min))
    (majutsu-op--wash-diff-output nil)
    (goto-char (point-min))
    (search-forward "commit-short")
    (let ((value (majutsu-op--diff-line-at-point)))
      (should (equal (plist-get value :marker) "+"))
      (should (equal (plist-get value :change-id) "change-full"))
      (should (equal (plist-get value :commit-id) "commit-full")))))

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
    (cl-letf (((symbol-function 'majutsu-repository-config-id) #'ignore))
      (transient-init-value obj))
    (should (equal (oref obj value) '("--limit=64")))))

(ert-deftest majutsu-op-diff-line-evolog-revset/ignores-absent-targets ()
  "Absent operation-diff targets should not produce evolog revsets."
  (let ((value '(:absent t :change-id "change" :commit-id "commit")))
    (should-not (majutsu-op--diff-line-evolog-revset value))))

(ert-deftest majutsu-op-diff-default-action/on-diff-line-opens-evolog ()
  "Default action on operation diff lines should open evolog."
  (let ((value '(:marker "+" :change-id "full-change" :commit-id "full-commit")))
    (with-temp-buffer
      (majutsu-op-diff-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (jj-op-commit-line value)
          (magit-insert-section (jj-commit "full-commit")
            (magit-insert-heading "+ line"))))
      (goto-char (point-min))
      (let (captured)
        (cl-letf (((symbol-function 'majutsu-evolog)
                   (lambda (revset &rest _)
                     (setq captured revset))))
          (majutsu-op-diff-default-action))
        (should (equal captured "change_id(full-change) | commit_id(full-commit)"))))))

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

(ert-deftest majutsu-op-diff-evolog-at-point/errors-on-absent-target ()
  "Evolog action should reject absent ref targets."
  (let ((value '(:marker "-" :absent t)))
    (with-temp-buffer
      (majutsu-op-diff-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (jj-op-ref-line value)
          (magit-insert-heading "- (absent)")))
      (goto-char (point-min))
      (should-error (majutsu-op-diff-evolog-at-point) :type 'user-error))))

(ert-deftest majutsu-op-diff-mode-map/line-actions ()
  "Operation diff mode should expose evolog without shadowing diff."
  (should (eq (lookup-key majutsu-op-diff-mode-map (kbd "RET"))
              'majutsu-op-diff-default-action))
  (should (eq (lookup-key majutsu-op-diff-mode-map (kbd "d"))
              'majutsu-diff))
  (should (eq (lookup-key majutsu-op-diff-mode-map (kbd "v"))
              'majutsu-op-diff-evolog-at-point)))

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

(ert-deftest majutsu-op-diff-refresh-buffer/renders-from-to-metadata ()
  "Operation diff refresh should render explicit endpoint metadata."
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-op--show-metadata)
               (lambda (operation)
                 (push operation seen)
                 (list :op-id (concat operation "-full")
                       :op-id-short (concat operation "-short")
                       :user "user"
                       :workspace "workspace"
                       :end-time "end"
                       :desc "desc")))
              ((symbol-function 'majutsu-jj-wash)
               (lambda (washer _keep-error &rest _args)
                 (insert "Changed commits:\n")
                 (funcall washer nil)
                 0)))
      (with-temp-buffer
        (majutsu-op-diff-mode)
        (setq majutsu-op-diff--args '("--from=a" "--to=b"))
        (let ((inhibit-read-only t))
          (majutsu-op-diff-refresh-buffer))
        (let ((content (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "From operation a-short" content))
          (should (string-match-p "To operation b-short" content))
          (should (string-match-p "Id: a-full" content))
          (should (string-match-p "Description: desc" content)))
        (should (equal (nreverse seen) '("a" "b")))))))

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

(ert-deftest majutsu-op-log-actions/use-operation-at-point ()
  "Operation log direct actions should use the full operation id at point."
  (with-temp-buffer
    (majutsu-op-log-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (jj-op "full-op")
        (magit-insert-heading "op")))
    (goto-char (point-min))
    (let (calls)
      (cl-letf (((symbol-function 'majutsu-op-restore)
                 (lambda (op) (push (list 'restore op) calls)))
                ((symbol-function 'majutsu-op-revert)
                 (lambda (op) (push (list 'revert op) calls))))
        (majutsu-op-log-restore-at-point)
        (majutsu-op-log-revert-at-point))
      (should (equal (nreverse calls)
                     '((restore "full-op")
                       (revert "full-op")))))))

(ert-deftest majutsu-op-target-prefix/init-value-uses-operation-at-point ()
  "Operation transients should prefill from the operation at point."
  (with-temp-buffer
    (majutsu-op-log-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (jj-op "full-op")
        (magit-insert-heading "op")))
    (goto-char (point-min))
    (let ((obj (make-instance 'majutsu-op-target-prefix
                              :command 'majutsu-op-diff-transient)))
      (transient-init-value obj)
      (should (equal (oref obj value) '("--operation=full-op"))))))

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

(ert-deftest majutsu-op-log-mode-map/diff-and-operation-actions ()
  "Operation log mode should expose diff and point actions."
  (should (eq (lookup-key majutsu-op-log-mode-map (kbd "RET"))
              'majutsu-visit-thing))
  (should (eq (lookup-key majutsu-op-log-mode-map (kbd "d"))
              'majutsu-op-diff-transient))
  (should (eq (lookup-key majutsu-op-log-mode-map (kbd "u"))
              'majutsu-op-log-restore-at-point))
  (should (eq (lookup-key majutsu-op-log-mode-map (kbd "r"))
              'majutsu-op-log-revert-at-point))
  (should (eq (lookup-key majutsu-mode-map (kbd "X"))
              'majutsu-op-transient))
  (should (eq (lookup-key majutsu-mode-map (kbd "v"))
              'majutsu-evolog))
  (should (transient-get-suffix 'majutsu-dispatch 'majutsu-evolog)))

;;; _
(provide 'majutsu-op-test)
;;; majutsu-op-test.el ends here
