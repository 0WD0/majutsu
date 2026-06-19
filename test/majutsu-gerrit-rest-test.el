;;; majutsu-gerrit-rest-test.el --- Tests for majutsu-gerrit-rest -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Unit tests for the Gerrit REST layer.  The tests mock curl by
;; replacing `majutsu-process-file'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-gerrit-rest)

(defconst majutsu-gerrit-rest-test--spec
  '(:host "example.com:8080" :scheme "http" :path "" :prefix "/a" :user "alice"))

(defun majutsu-gerrit-rest-test--mock-curl (body status &optional seen-fn)
  "Return a mock `majutsu-process-file' function inserting BODY and STATUS.
When SEEN-FN is non-nil, call it with the received argument list."
  (lambda (_program _infile _destination _display &rest args)
    (when seen-fn (funcall seen-fn args))
    (insert body)
    (insert "\nMAJUTSU_GERRIT_STATUS:" (number-to-string status) "\n")
    0))

(ert-deftest majutsu-gerrit-rest-params/repeated-and-bare-flags ()
  "Query params should support repeated keys and bare boolean flags."
  (should (equal (majutsu-gerrit-rest-params
                  '(("q" . "is:open") ("q" . "owner:self")
                    ("o" . "LABELS") ("raw" . t)))
                 "q=is%3Aopen&q=owner%3Aself&o=LABELS&raw")))

(ert-deftest majutsu-gerrit-rest-encode-id/encodes-path-as-one-segment ()
  "File paths must be encoded as one path segment."
  (should (equal (majutsu-gerrit-rest-encode-id "/COMMIT_MSG")
                 "%2FCOMMIT_MSG"))
  (should (equal (majutsu-gerrit-rest-encode-id "src/foo bar.el")
                 "src%2Ffoo%20bar.el")))

(ert-deftest majutsu-gerrit-rest-json/builds-compact-json-request ()
  "JSON helpers should add Accept and pp=0 while preserving repeated params."
  (let (seen-args)
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _args) nil))
              ((symbol-function 'majutsu-process-file)
               (majutsu-gerrit-rest-test--mock-curl
                ")]}'\n[{\"id\":\"demo~1\"}]" 200
                (lambda (args) (setq seen-args args)))))
      (should (equal (majutsu-gerrit-rest-change-query
                      '("is:open" "owner:self") 25 nil '("LABELS")
                      majutsu-gerrit-rest-test--spec)
                     '(((id . "demo~1")))))
      (should (member "Accept: application/json" seen-args))
      (should (member "http://example.com:8080/a/changes/?q=is%3Aopen&q=owner%3Aself&n=25&o=LABELS&pp=0"
                      seen-args)))))

(ert-deftest majutsu-gerrit-rest-request/uses-auth-source-when-present ()
  "Requests should pass auth-source credentials to curl without prompting."
  (let (seen-args)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (should (equal (plist-get args :host) "example.com:8080"))
                 (should (equal (plist-get args :user) "alice"))
                 (should-not (plist-get args :create))
                 (list (list :user "alice" :secret (lambda () "secret")))))
              ((symbol-function 'majutsu-process-file)
               (majutsu-gerrit-rest-test--mock-curl ")]}'\n\"3.14\"" 200
                                                       (lambda (args) (setq seen-args args)))))
      (should (equal (majutsu-gerrit-rest-config-version
                      majutsu-gerrit-rest-test--spec)
                     "3.14"))
      (should (member "--user" seen-args))
      (should (member "alice:secret" seen-args)))))

(ert-deftest majutsu-gerrit-rest-json/preserves-error-body ()
  "HTTP errors should signal with Gerrit's plaintext body."
  (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _args) nil))
            ((symbol-function 'majutsu-process-file)
             (majutsu-gerrit-rest-test--mock-curl "invalid query" 400)))
    (let ((err (should-error
                (majutsu-gerrit-rest-change-query
                 "assignee:self" nil nil nil majutsu-gerrit-rest-test--spec)
                :type 'majutsu-gerrit-rest-error)))
      (should (equal (cdr err) '(400 "invalid query"
                                     "http://example.com:8080/a/changes/?q=assignee%3Aself&pp=0"))))))

(ert-deftest majutsu-gerrit-rest-text/does-not-json-parse ()
  "Text helpers should return raw bodies unchanged."
  (let (seen-args)
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _args) nil))
              ((symbol-function 'majutsu-process-file)
               (majutsu-gerrit-rest-test--mock-curl "diff --git a/x b/x\n" 200
                                                       (lambda (args) (setq seen-args args)))))
      (should (equal (majutsu-gerrit-rest-revision-patch
                      "proj~1" "current" majutsu-gerrit-rest-test--spec)
                     "diff --git a/x b/x\n"))
      (should (member "http://example.com:8080/a/changes/proj~1/revisions/current/patch?raw"
                      seen-args)))))

(ert-deftest majutsu-gerrit-rest-normalize-spec/from-shared-spec ()
  "Shared majutsu-gerrit specs should normalize into REST specs."
  (should (equal (majutsu-gerrit-rest-normalize-spec
                  '(:gerrit-host "example.com" :gerrit-prefix "/review/a" :ssl t))
                 '(:host "example.com" :scheme "https" :path "" :prefix "/review/a" :user nil))))

(provide 'majutsu-gerrit-rest-test)

;;; majutsu-gerrit-rest-test.el ends here
