;;; majutsu-jj-test.el --- Tests for majutsu-jj helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for majutsu-jj helpers.

;;; Code:

(require 'ert)
(require 'majutsu-jj)

(ert-deftest majutsu-jj-fileset-quote-single-quote ()
  "Single quotes should be preserved inside fileset strings."
  (should (equal (majutsu-jj-fileset-quote "test'file")
                 "file:\"test'file\"")))

(ert-deftest majutsu-jj-fileset-quote-escapes-specials ()
  "Double quotes, backslashes, and newlines should be escaped."
  (let* ((input "a\"b\\c\n")
         (expected "file:\"a\\\"b\\\\c\\n\""))
    (should (equal (majutsu-jj-fileset-quote input) expected))))

;; Tests for majutsu-jj-string (new behavior - returns first line only)

(ert-deftest majutsu-jj-string/returns-first-line ()
  "majutsu-jj-string should return only the first line of output."
  (cl-letf (((symbol-function 'majutsu--jj-insert)
             (lambda (_return-error &rest _args)
               (insert "first line\nsecond line\nthird line") 0)))
    (should (equal (majutsu-jj-string "log" "-r" "@") "first line"))))

(ert-deftest majutsu-jj-string/returns-nil-for-empty-output ()
  "majutsu-jj-string should return nil when there is no output."
  (cl-letf (((symbol-function 'majutsu--jj-insert)
             (lambda (_return-error &rest _args) 0)))
    (should (null (majutsu-jj-string "log" "-r" "@")))))

(ert-deftest majutsu-jj-string/returns-empty-string-for-newline-start ()
  "majutsu-jj-string should return empty string if output starts with newline."
  (cl-letf (((symbol-function 'majutsu--jj-insert)
             (lambda (_return-error &rest _args)
               (insert "\nsecond line") 0)))
    (should (equal (majutsu-jj-string "log" "-r" "@") ""))))



;; Tests for majutsu-jj-lines

(ert-deftest majutsu-jj-lines/splits-output-into-lines ()
  "majutsu-jj-lines should split output into a list of lines."
  (cl-letf (((symbol-function 'majutsu--jj-insert)
             (lambda (_return-error &rest _args)
               (insert "line1\nline2\nline3") 0)))
    (should (equal (majutsu-jj-lines "log" "-r" "@")
                   '("line1" "line2" "line3")))))

(ert-deftest majutsu-jj-lines/omits-empty-lines ()
  "majutsu-jj-lines should omit empty lines from result."
  (cl-letf (((symbol-function 'majutsu--jj-insert)
             (lambda (_return-error &rest _args)
               (insert "line1\n\nline2\n\n") 0)))
    (should (equal (majutsu-jj-lines "log" "-r" "@")
                   '("line1" "line2")))))

;; Tests for majutsu-jj-items

(ert-deftest majutsu-jj-items/splits-by-null-bytes ()
  "majutsu-jj-items should split output by null bytes."
  (cl-letf (((symbol-function 'majutsu--jj-insert)
             (lambda (_return-error &rest _args)
               (insert "item1\0item2\0item3") 0)))
    (should (equal (majutsu-jj-items "file" "list" "-z")
                   '("item1" "item2" "item3")))))

(ert-deftest majutsu-jj-items/omits-empty-items ()
  "majutsu-jj-items should omit empty items from result."
  (cl-letf (((symbol-function 'majutsu--jj-insert)
             (lambda (_return-error &rest _args)
               (insert "item1\0\0item2\0") 0)))
    (should (equal (majutsu-jj-items "file" "list" "-z")
                   '("item1" "item2")))))

;; Tests for majutsu-jj-insert

(ert-deftest majutsu-jj-insert/inserts-output-at-point ()
  "majutsu-jj-insert should insert output at point and return exit code."
  (cl-letf (((symbol-function 'majutsu--jj-insert)
             (lambda (return-error &rest _args)
               (insert "output text") 0)))
    (with-temp-buffer
      (should (equal (majutsu-jj-insert "log" "-r" "@") 0))
      (should (equal (buffer-string) "output text")))))

;; Tests for majutsu--jj-insert error handling

(ert-deftest majutsu--jj-insert/returns-exit-code-on-success ()
  "majutsu--jj-insert should return 0 on success when return-error is nil."
  (cl-letf (((symbol-function 'process-file)
             (lambda (_program _infile _destination _display &rest _args) 0)))
    (with-temp-buffer
      (should (equal (majutsu--jj-insert nil "log" "-r" "@") 0)))))

(ert-deftest majutsu--jj-insert/forces-wide-columns-for-diffstat ()
  "Diffstat commands should run with widened `COLUMNS'."
  (let ((majutsu-jj-diffstat-columns 80)
        seen-columns)
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program _infile _destination _display &rest _args)
                 (setq seen-columns (getenv "COLUMNS"))
                 0)))
      (with-temp-buffer
        (let ((process-environment (cons "COLUMNS=10" process-environment)))
          (should (equal (majutsu--jj-insert nil "diff" "--stat") 0))
          (should (equal seen-columns "80")))))))

(ert-deftest majutsu--jj-insert/keeps-columns-for-non-diffstat ()
  "Non-diffstat commands should keep inherited `COLUMNS'."
  (let ((majutsu-jj-diffstat-columns 80)
        seen-columns)
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program _infile _destination _display &rest _args)
                 (setq seen-columns (getenv "COLUMNS"))
                 0)))
      (with-temp-buffer
        (let ((process-environment (cons "COLUMNS=10" process-environment)))
          (should (equal (majutsu--jj-insert nil "log" "-r" "@") 0))
          (should (equal seen-columns "10")))))))

(ert-deftest majutsu-process-environment/overrides-columns-for-diffstat ()
  "Environment helper should replace inherited COLUMNS for diffstat commands."
  (let ((majutsu-jj-diffstat-columns 80)
        (majutsu-jj-environment '("INSIDE_EMACS=test,majutsu"))
        (process-environment '("COLUMNS=10" "FOO=bar")))
    (should (equal (car (majutsu-process-environment '("diff" "--stat")))
                   "COLUMNS=80"))
    (should (member "FOO=bar" (majutsu-process-environment '("diff" "--stat"))))
    (should (member "INSIDE_EMACS=test,majutsu"
                    (majutsu-process-environment '("diff" "--stat"))))
    (should-not (member "COLUMNS=10" (majutsu-process-environment '("diff" "--stat"))))))

(ert-deftest majutsu-process-environment/preserves-columns-for-non-diffstat ()
  "Environment helper should keep inherited COLUMNS for non-diffstat commands."
  (let ((majutsu-jj-diffstat-columns 80)
        (majutsu-jj-environment nil)
        (process-environment '("COLUMNS=10" "FOO=bar")))
    (should (equal (majutsu-process-environment '("log" "-r" "@"))
                   '("COLUMNS=10" "FOO=bar")))))

(ert-deftest majutsu-jj-wash/forces-wide-columns-for-diffstat ()
  "`majutsu-jj-wash' should run diffstat with widened `COLUMNS'."
  (let ((majutsu-jj-diffstat-columns 80)
        seen-columns)
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program _infile _destination _display &rest _args)
                 (setq seen-columns (getenv "COLUMNS"))
                 (insert "x\n")
                 0)))
      (with-temp-buffer
        (let ((process-environment (cons "COLUMNS=10" process-environment)))
          (should (equal (majutsu-jj-wash (lambda (&rest _) nil)
                             'wash-anyway
                           "diff"
                           "--stat")
                         0))
          (should (equal seen-columns "80")))))))

(ert-deftest majutsu--jj-insert/returns-error-message-on-failure ()
  "majutsu--jj-insert should return error message when return-error is t and command fails."
  (cl-letf (((symbol-function 'process-file)
             (lambda (_program _infile _destination _display &rest _args)
               ;; Simulate error by writing to stderr file
               1))
            ((symbol-function 'make-nearby-temp-file)
             (lambda (_prefix) "/tmp/test-err"))
            ((symbol-function 'insert-file-contents)
             (lambda (file) (insert "Error: something went wrong")))
            ((symbol-function 'delete-file)
             (lambda (_file) nil)))
    (with-temp-buffer
      (let ((result (majutsu--jj-insert t "log" "-r" "invalid")))
        (should (stringp result))
        (should (string-match-p "something went wrong" result))))))

(ert-deftest majutsu-jj--executable/picks-remote-value ()
  "Executable selection should use remote override on TRAMP paths."
  (let ((default-directory "/ssh:demo:/tmp/")
        (majutsu-jj-executable "jj-local")
        (majutsu-remote-jj-executable "jj-remote"))
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (path &optional identification _connected)
                 (when (and (equal path default-directory)
                            (null identification))
                   "/ssh:demo:"))))
      (should (equal (majutsu-jj--executable) "jj-remote")))))

(ert-deftest majutsu-jj-expand-filename-from-jj/preserves-remote-prefix ()
  "Absolute paths from jj output should keep TRAMP host prefix."
  (let ((default-directory "/ssh:demo:/tmp/"))
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (path &optional identification _connected)
                 (when (and (equal path default-directory)
                            (null identification))
                   "/ssh:demo:"))))
      (should (equal (majutsu-jj-expand-filename-from-jj "/home/demo/repo")
                     "/ssh:demo:/home/demo/repo")))))

(ert-deftest majutsu-jj-convert-filename-for-jj/strips-tramp-prefix ()
  "Paths passed to remote jj tools should drop TRAMP prefix."
  (cl-letf (((symbol-function 'file-remote-p)
             (lambda (path &optional identification _connected)
               (when (and (equal path "/ssh:demo:/tmp/patch.diff")
                          (eq identification 'localname))
                 "/tmp/patch.diff"))))
    (should (equal (majutsu-convert-filename-for-jj "/ssh:demo:/tmp/patch.diff")
                   "/tmp/patch.diff"))))

(ert-deftest majutsu-jj--editor-command-from-env/parses-sleeping-editor-wrapper ()
  "Sleeping editor env should parse as PROGRAM -c SCRIPT, not `wait'."
  (let* ((majutsu-with-editor-envvar "JJ_EDITOR")
         (process-environment
          (cons (format "JJ_EDITOR=%s" with-editor-sleeping-editor)
                process-environment))
         (command (majutsu-jj--editor-command-from-env)))
    (should (equal (car command) "sh"))
    (should (equal (cadr command) "-c"))
    (should (string-match-p "WITH-EDITOR: \\\$\\\$ OPEN" (nth 2 command)))))

(ert-deftest majutsu-toplevel/preserves-remote-prefix ()
  "`majutsu-toplevel' should return remote workspace roots on TRAMP."
  (let ((default-directory "/ssh:demo:/tmp/"))
    (cl-letf (((symbol-function 'majutsu--safe-default-directory)
               (lambda (&optional _file) default-directory))
              ((symbol-function 'file-remote-p)
               (lambda (path &optional identification _connected)
                 (when (and (equal path default-directory)
                            (null identification))
                   "/ssh:demo:")))
              ((symbol-function 'process-file)
               (lambda (_program _infile _destination _display &rest _args)
                 (insert "/home/demo/repo\n")
                 0)))
      (should (equal (majutsu-toplevel)
                     "/ssh:demo:/home/demo/repo/")))))

(ert-deftest majutsu--assert-usable-jj/uses-remote-aware-executable-find ()
  "Remote executable assertion should use `executable-find' with REMOTE.
This mirrors Magit's behavior."
  (let ((default-directory "/ssh:demo:/tmp/")
        (majutsu-jj-executable "jj-local")
        (majutsu-remote-jj-executable "jj-remote")
        seen)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program &optional remote)
                 (setq seen (list program remote))
                 "/usr/bin/jj"))
              ((symbol-function 'file-remote-p)
               (lambda (path &optional identification _connected)
                 (when (and (equal path default-directory)
                            (null identification))
                   "/ssh:demo:"))))
      (should-not (majutsu--assert-usable-jj))
      (should (equal seen '("jj-remote" t))))))

(ert-deftest majutsu--assert-usable-jj/signals-when-remote-executable-missing ()
  "Remote executable assertion should signal not-found on lookup failure."
  (let ((default-directory "/ssh:demo:/tmp/")
        (majutsu-jj-executable "jj-local")
        (majutsu-remote-jj-executable "jj-remote"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program &optional _remote) nil))
              ((symbol-function 'file-remote-p)
               (lambda (path &optional identification _connected)
                 (when (and (equal path default-directory)
                            (null identification))
                   "/ssh:demo:"))))
      (should-error (majutsu--assert-usable-jj)
                    :type 'majutsu-jj-executable-not-found))))

(ert-deftest majutsu-jj-revset-candidates/includes-workspaces-bookmarks-tags ()
  "Revset candidates should include common refs and deduplicate values."
  (cl-letf (((symbol-function 'majutsu-jj--safe-lines)
             (lambda (&rest args)
               (pcase args
                 (`("workspace" "list" "-T" "name ++ \"\\n\"") '("ws-a" "ws-b"))
                 (`("bookmark" "list" "--quiet" "-T" "name ++ \"\\n\"") '("main" "feature"))
                 (`("tag" "list" "--quiet" "-T" "name ++ \"\\n\"") '("v1.0" "main"))
                 (_ nil)))))
    (should (equal (majutsu-jj-revset-candidates "main")
                   '("main" "@" "@-" "@+" "ws-a@" "ws-b@" "feature" "v1.0")))))

(ert-deftest majutsu-jj-completion-items/prefers-machine-readable-completion ()
  "Native completion should prefer `jj util complete` when available."
  (let ((majutsu-jj-executable "/usr/bin/jj")
        seen-program
        seen-args
        seen-complete)
    (cl-letf (((symbol-function 'majutsu-process-file)
               (lambda (program _infile destination _display &rest args)
                 (setq seen-program program
                       seen-args args
                       seen-complete (getenv "COMPLETE"))
                 (when (eq destination t)
                   (insert "[{\"value\":\"main\",\"help\":\"Main bookmark\",\"tag\":\"--revset\",\"display_order\":0,\"hidden\":false}]") )
                 0)))
      (should (equal (majutsu-jj-completion-items '("log" "-r" "ma"))
                     '(("main" . "Main bookmark"))))
      (should (equal seen-program "/usr/bin/jj"))
      (should (equal seen-args '("util" "complete" "--format" "json"
                                 "--index" "3" "--" "jj" "log" "-r" "ma")))
      (should-not seen-complete))))

(ert-deftest majutsu-jj-completion-items/falls-back-to-fish-completion ()
  "Native completion should fall back to fish output on older jj versions."
  (let ((majutsu-jj-executable "/usr/bin/jj")
        calls)
    (cl-letf (((symbol-function 'majutsu-process-file)
               (lambda (_program _infile destination _display &rest args)
                 (setq calls (append calls (list (list :args args
                                                      :complete (getenv "COMPLETE")))))
                 (pcase (length calls)
                   (1 1)
                   (2 (when (eq destination t)
                        (insert "main\tMain bookmark\n"))
                      0)
                   (_ (ert-fail "Unexpected extra completion invocation"))))))
      (should (equal (majutsu-jj-completion-items '("log" "-r" "ma"))
                     '(("main" . "Main bookmark"))))
      (should (equal calls
                     '((:args ("util" "complete" "--format" "json"
                               "--index" "3" "--" "jj" "log" "-r" "ma")
                        :complete nil)
                       (:args ("--" "jj" "log" "-r" "ma")
                        :complete "fish")))))))

(ert-deftest majutsu-jj-completion-items/does-not-fallback-on-empty-machine-result ()
  "Machine-readable completion should not fall back when it returns no items."
  (let ((majutsu-jj-executable "/usr/bin/jj")
        calls)
    (cl-letf (((symbol-function 'majutsu-process-file)
               (lambda (_program _infile destination _display &rest args)
                 (setq calls (1+ (or calls 0)))
                 (when (eq destination t)
                   (insert "[]"))
                 (should (equal args '("util" "complete" "--format" "json"
                                       "--index" "3" "--" "jj" "log" "-r" "ma")))
                 0)))
      (should-not (majutsu-jj-completion-items '("log" "-r" "ma")))
      (should (= calls 1)))))

(ert-deftest majutsu-jj-completion-table/exposes-annotations-and-default ()
  "Native completion tables should expose metadata annotations."
  (let ((annotations (make-hash-table :test #'equal)))
    (puthash "main" "Main bookmark" annotations)
    (cl-letf (((symbol-function 'majutsu-jj--completion-payload)
               (lambda (args category)
                 (should (equal args '("log" "-r" "")))
                 (should (eq category 'majutsu-revision))
                 (list :category 'majutsu-revision
                       :candidates '("main")
                       :annotations annotations))))
      (let* ((table (majutsu-jj--completion-table '("log" "-r")
                                                  'majutsu-revision
                                                  "@"))
             (metadata (funcall table "" nil 'metadata))
             (annotation (cdr (assq 'annotation-function (cdr metadata)))))
        (should (equal (all-completions "" table) '("@" "main")))
        (should (eq (cdr (assq 'category (cdr metadata))) 'majutsu-revision))
        (should (equal (funcall annotation "main") " Main bookmark"))
        (should-not (assq 'affixation-function (cdr metadata)))
        (should-not (funcall annotation "@"))))))

(ert-deftest majutsu-jj-completion-table/completes-revset-expressions-dynamically ()
  "Native completion tables should send the current revset expression to jj."
  (let (calls prewarmed-contexts)
    (cl-letf (((symbol-function 'majutsu-jj--completion-payload)
               (lambda (args category)
                 (push args calls)
                 (should (eq category 'majutsu-revision))
                 (let* ((input (car (last args)))
                        (candidate (pcase input
                                     ("main | " "main | dev")
                                     ("trunk()..ma" "trunk()..main")
                                     (_ nil)))
                        (annotations (make-hash-table :test #'equal)))
                   (when candidate
                     (puthash candidate (concat "Help for " candidate) annotations))
                   (list :category 'majutsu-revision
                         :candidates (and candidate (list candidate))
                         :annotations annotations))))
              ((symbol-function 'majutsu-completion-prewarm-payload)
               (lambda (_payload _category context _directory)
                 (push context prewarmed-contexts))))
      (let* ((table (majutsu-jj--completion-table '("diff" "-r")
                                                  'majutsu-revision))
             (metadata (funcall table "" nil 'metadata))
             (annotation (cdr (assq 'annotation-function (cdr metadata)))))
        (should (equal (all-completions "main | " table)
                       '("main | dev")))
        (should (equal (all-completions "trunk()..ma" table)
                       '("trunk()..main")))
        (should (member '("diff" "-r" "main | ") calls))
        (should (member '("diff" "-r" "trunk()..ma") calls))
        (should (member "main | " prewarmed-contexts))
        (should (member "trunk()..ma" prewarmed-contexts))
        (should (equal (funcall annotation "main | dev")
                       " Help for main | dev"))))))

(ert-deftest majutsu-jj-revset-candidate-data/provides-source-annotations ()
  "Candidate data should preserve source kinds for metadata annotations."
  (cl-letf (((symbol-function 'majutsu-jj--safe-lines)
             (lambda (&rest args)
               (pcase args
                 (`("workspace" "list" "-T" "name ++ \"\\n\"") '("ws-a"))
                 (`("bookmark" "list" "--quiet" "-T" "name ++ \"\\n\"") '("main"))
                 (`("tag" "list" "--quiet" "-T" "name ++ \"\\n\"") '("main" "v1.0"))
                 (_ nil)))))
    (let* ((data (majutsu-jj-revset-candidate-data "main"))
           (sources (plist-get data :sources))
           (annotations (plist-get data :annotations))
           (entries (plist-get data :entries))
           (annotation (majutsu-jj--revset-annotation-function sources)))
      (should (eq (plist-get data :category) 'majutsu-revision))
      (should (equal (funcall annotation "@") "  [pseudo]"))
      (should (equal (funcall annotation "ws-a@") "  [workspace]"))
      (should (equal (funcall annotation "main") "  [bookmark,tag]"))
      (should (equal (gethash "main" annotations) "  [bookmark,tag]"))
      (should (equal (plist-get (gethash "main" entries) :kind) 'bookmark))
      (should (equal (plist-get (gethash "main" entries) :sources)
                     '(bookmark tag))))))

(ert-deftest majutsu-jj-revset-completion-at-point/uses-current-expression ()
  "Revset CAPF should ask jj about the current full expression."
  (let (calls prewarmed-contexts)
    (with-temp-buffer
      (insert "main | ")
      (goto-char (point-max))
      (let ((majutsu-jj--revset-completion-args '("diff" "-r"))
            (majutsu-jj--revset-completion-default "@"))
        (cl-letf (((symbol-function 'minibuffer-prompt-end)
                   (lambda () 1))
                  ((symbol-function 'majutsu-jj--completion-payload)
                   (lambda (args category)
                     (push args calls)
                     (should (eq category 'majutsu-revision))
                     (let ((annotations (make-hash-table :test #'equal))
                           (entries (make-hash-table :test #'equal)))
                       (puthash "main | dev" "Union with dev" annotations)
                       (puthash "main | dev" '(:kind bookmark :help "Union with dev") entries)
                       (list :category 'majutsu-revision
                             :candidates '("main | dev")
                             :annotations annotations
                             :entries entries))))
                  ((symbol-function 'majutsu-completion-prewarm-payload)
                   (lambda (_payload _category context _directory)
                     (push context prewarmed-contexts))))
          (pcase-let ((`(,beg ,end ,table . ,props)
                       (majutsu-jj-revset-completion-at-point)))
            (should (= beg 1))
            (should (= end (point-max)))
            (should (equal (plist-get props :exclusive) 'no))
            (should (eq (plist-get props :category) 'majutsu-revision))
            (should (functionp (plist-get props :annotation-function)))
            (should (functionp (plist-get props :company-doc-buffer)))
            (should (hash-table-p (plist-get props :majutsu-revision-entries)))
            (should (equal (all-completions "main | " table)
                           '("main | dev")))
            (should (member '("diff" "-r" "main | ") calls))
            (should (member "main | " prewarmed-contexts))))))))

(ert-deftest majutsu-jj--revset-minibuffer-setup/installs-capf ()
  "Revset minibuffer setup should install the CAPF helper locally."
  (with-temp-buffer
    (majutsu-jj--revset-minibuffer-setup)
    (should (memq #'majutsu-jj-revset-completion-at-point
                  completion-at-point-functions))))

(ert-deftest majutsu-jj--corfu-revision-margin-formatter/uses-entry-kind ()
  "Corfu margin formatter should expose a fixed-width revision kind label."
  (let ((completion-extra-properties nil))
    (let ((entries (make-hash-table :test #'equal)))
      (puthash "main" '(:kind bookmark) entries)
      (let* ((completion-extra-properties (list :majutsu-revision-entries entries))
             (formatter (majutsu-jj--corfu-revision-margin-formatter
                         '(metadata (category . majutsu-revision))))
             (label (funcall formatter "main")))
        (should (functionp formatter))
        (should (equal (substring-no-properties label) "Bm"))
        (should (= (string-width label) 2))))))

(ert-deftest majutsu-read-revset/uses-read-from-minibuffer-and-allows-free-form ()
  "Revset reader should use plain minibuffer input and allow free-form text."
  (let (seen-keymap seen-history seen-default)
    (cl-letf (((symbol-function 'majutsu-jj-revset-candidate-data)
               (lambda (_default)
                 (list :category 'majutsu-revision
                       :candidates '("@" "main"))))
              ((symbol-function 'majutsu-completion-prewarm-payload)
               (lambda (&rest _args)))
              ((symbol-function 'read-from-minibuffer)
               (lambda (_prompt _initial keymap _read hist default &optional _inherit)
                 (setq seen-keymap keymap
                       seen-history hist
                       seen-default default)
                 "main")))
      (should (equal (majutsu-read-revset "Rev" "@") "main"))
      (should (eq seen-keymap majutsu-read-revset-map))
      (should (eq seen-history 'majutsu-read-revset-history))
      (should (equal seen-default "@")))))

(ert-deftest majutsu-read-revset/empty-input-accepts-default ()
  "Required revset reader should accept DEFAULT on empty input."
  (cl-letf (((symbol-function 'majutsu-jj-revset-candidate-data)
             (lambda (_default)
               (list :category 'majutsu-revision :candidates '("@"))))
            ((symbol-function 'majutsu-completion-prewarm-payload)
             (lambda (&rest _args)))
            ((symbol-function 'read-from-minibuffer)
             (lambda (&rest _args) "")))
    (should (equal (majutsu-read-revset "Rev" "@") "@"))))

(ert-deftest majutsu-read-optional-revset/uses-read-from-minibuffer-and-allows-empty ()
  "Optional revset reader should use minibuffer input and accept empty input."
  (let (seen-initial seen-history seen-default seen-keymap)
    (cl-letf (((symbol-function 'majutsu-jj-revset-candidate-data)
               (lambda (_default)
                 (list :category 'majutsu-revision
                       :candidates '("@" "main"))))
              ((symbol-function 'majutsu-completion-prewarm-payload)
               (lambda (&rest _args)))
              ((symbol-function 'read-from-minibuffer)
               (lambda (_prompt initial keymap _read hist default &optional _inherit)
                 (setq seen-initial initial
                       seen-keymap keymap
                       seen-history hist
                       seen-default default)
                 "")))
      (should-not (majutsu-read-optional-revset "Rev" nil "current"))
      (should (equal seen-initial "current"))
      (should (eq seen-keymap majutsu-read-revset-map))
      (should (eq seen-history 'majutsu-read-revset-history))
      (should (null seen-default)))))

(provide 'majutsu-jj-test)
