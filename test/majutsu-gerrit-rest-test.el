;;; majutsu-gerrit-rest-test.el --- Tests for majutsu-gerrit-rest -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Unit tests for the Gerrit REST layer.  Tests mock `plz' and
;; `auth-source-search' so they do not perform network IO.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-gerrit-rest)

(defconst majutsu-gerrit-rest-test--spec
  '(:host "example.com:8080" :scheme "http" :path "" :prefix "/a" :user "alice"))

(defun majutsu-gerrit-rest-test--mock-plz-response (status body)
  "Return a mock `plz-response' with STATUS and BODY."
  (make-plz-response :version "1.1" :status status :headers nil :body body))

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
  (let (seen)
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _args) nil))
              ((symbol-function 'plz)
               (lambda (method url &rest args)
                 (setq seen (list method url (plist-get args :headers)
                                  (plist-get args :body)))
                 (majutsu-gerrit-rest-test--mock-plz-response
                  200 ")]}'\n[{\"id\":\"demo~1\"}]"))))
      (should (equal (majutsu-gerrit-rest-change-query
                      '("is:open" "owner:self") 25 nil '("LABELS")
                      majutsu-gerrit-rest-test--spec)
                     '(((id . "demo~1")))))
      (should (eq (car seen) 'get))
      (should (equal (cadr seen)
                     "http://example.com:8080/a/changes/?q=is%3Aopen&q=owner%3Aself&n=25&o=LABELS&pp=0"))
      (should (member '("Accept" . "application/json") (nth 2 seen))))))

(ert-deftest majutsu-gerrit-rest-json/preserves-false-and-null ()
  "Raw REST alists should distinguish JSON false, null and absence."
  (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _args) nil))
            ((symbol-function 'plz)
             (lambda (_method _url &rest _args)
               (majutsu-gerrit-rest-test--mock-plz-response
                200 ")]}'\n[{\"unresolved\":false,\"nullable\":null}]"))))
    (should (equal (majutsu-gerrit-rest-change-query
                    "is:open" nil nil nil majutsu-gerrit-rest-test--spec)
                   '(((unresolved . :json-false) (nullable . :json-null)))))))

(ert-deftest majutsu-gerrit-rest-request/uses-auth-source-when-present ()
  "Requests should pass auth-source credentials via Basic header."
  (let (seen-headers)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (should (equal (plist-get args :host) "example.com:8080"))
                 (should (equal (plist-get args :user) "alice"))
                 (should-not (plist-get args :create))
                 (list (list :user "alice" :secret (lambda () "secret")))))
              ((symbol-function 'plz)
               (lambda (_method _url &rest args)
                 (setq seen-headers (plist-get args :headers))
                 (majutsu-gerrit-rest-test--mock-plz-response 200 ")]}'\n\"3.14\""))))
      (should (equal (majutsu-gerrit-rest-config-version
                      majutsu-gerrit-rest-test--spec)
                     "3.14"))
      (should (member '("Authorization" . "Basic YWxpY2U6c2VjcmV0")
                      seen-headers)))))

(ert-deftest majutsu-gerrit-rest-json/preserves-error-body ()
  "HTTP errors should signal with Gerrit's plaintext body."
  (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _args) nil))
            ((symbol-function 'plz)
             (lambda (_method _url &rest _args)
               ;; Mirror plz's real signal shape:
               ;; (signal 'plz-http-error (list "HTTP error" <plz-error>))
               (signal 'plz-http-error
                       (list "HTTP error"
                             (make-plz-error
                              :response (majutsu-gerrit-rest-test--mock-plz-response
                                         400 "invalid query")))))))
    (let ((err (should-error
                (majutsu-gerrit-rest-change-query
                 "assignee:self" nil nil nil majutsu-gerrit-rest-test--spec)
                :type 'majutsu-gerrit-rest-error)))
      (should (equal (cdr err) '(400 "invalid query"
                                 "http://example.com:8080/a/changes/?q=assignee%3Aself&pp=0"))))))

(ert-deftest majutsu-gerrit-rest-request/returns-status-zero-on-curl-error ()
  "Transport/TLS failures (plz-curl-error) should yield (:status 0 ...)
instead of propagating a raw error, so callers get a uniform plist."
  (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _args) nil))
            ((symbol-function 'plz)
             (lambda (_method _url &rest _args)
               ;; Real plz signals curl errors with a plz-error that has no
               ;; :response (only :curl-error / :message).
               (signal 'plz-curl-error
                       (list "Curl error"
                             (make-plz-error :message "could not resolve host"))))))
    (let ((response (majutsu-gerrit-rest-request
                     "GET" "/config/server/version" nil
                     majutsu-gerrit-rest-test--spec)))
      (should (equal (plist-get response :status) 0))
      (should (equal (plist-get response :body) ""))
      (should (equal (plist-get response :url)
                     "http://example.com:8080/a/config/server/version")))))

(ert-deftest majutsu-gerrit-rest-text/does-not-json-parse ()
  "Text helpers should return raw bodies unchanged."
  (let (seen-url)
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _args) nil))
              ((symbol-function 'plz)
               (lambda (_method url &rest _args)
                 (setq seen-url url)
                 (majutsu-gerrit-rest-test--mock-plz-response
                  200 "diff --git a/x b/x\n"))))
      (should (equal (majutsu-gerrit-rest-text
                      "GET" "/plain" nil majutsu-gerrit-rest-test--spec
                      '(("raw" . t)))
                     "diff --git a/x b/x\n"))
      (should (equal seen-url
                     "http://example.com:8080/a/plain?raw")))))

(ert-deftest majutsu-gerrit-rest-normalize-spec/from-shared-spec ()
  "Shared majutsu-gerrit specs should split web path from the /a prefix."
  (should (equal (majutsu-gerrit-rest-normalize-spec
                  '(:gerrit-host "example.com" :gerrit-prefix "/review/a" :ssl t))
                 '(:host "example.com" :scheme "https" :path "/review"
                   :prefix "/a" :user nil))))

(provide 'majutsu-gerrit-rest-test)

;;; majutsu-gerrit-rest-test.el ends here
