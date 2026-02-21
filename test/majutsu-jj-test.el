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
           (annotation (majutsu-jj--revset-annotation-function sources)))
      (should (equal (funcall annotation "@") "  [pseudo]"))
      (should (equal (funcall annotation "ws-a@") "  [workspace]"))
      (should (equal (funcall annotation "main") "  [bookmark,tag]")))))

(ert-deftest majutsu-read-revset/uses-completion-and-allows-free-form ()
  "Revset reader should use completion metadata and allow free-form input."
  (let (seen-require-match seen-default seen-category seen-history seen-annotation)
    (let ((sources (make-hash-table :test #'equal)))
      (puthash "main" '(bookmark tag) sources)
      (cl-letf (((symbol-function 'majutsu-jj-revset-candidate-data)
                 (lambda (_default)
                   (list :candidates '("@" "main")
                         :sources sources)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table _predicate require-match _initial hist def)
                   (let ((metadata (funcall table "" nil 'metadata)))
                     (setq seen-require-match require-match
                           seen-history hist
                           seen-default def
                           seen-category (cdr (assq 'category (cdr metadata)))
                           seen-annotation (cdr (assq 'annotation-function (cdr metadata)))))
                   "main")))
        (should (equal (majutsu-read-revset "Rev" "@") "main"))
        (should (null seen-require-match))
        (should (eq seen-history 'majutsu-read-revset-history))
        (should (equal seen-default "@"))
        (should (eq seen-category 'majutsu-revision))
        (should (equal (funcall seen-annotation "main")
                       "  [bookmark,tag]"))))))

(provide 'majutsu-jj-test)
