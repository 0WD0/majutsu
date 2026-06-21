;;; majutsu-gerrit-rest.el --- REST client for Gerrit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Curl-based Gerrit REST client for Majutsu.
;;
;; This module intentionally exposes a raw request primitive plus JSON and
;; text helpers.  Gerrit JSON responses start with an XSSI prefix, but
;; error bodies and patch/content endpoints are plain text or binary.  A
;; single "always parse JSON" API would lose useful server errors.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)

(require 'majutsu-process)

(declare-function majutsu-gerrit--remote-user "majutsu-gerrit"
                  (&optional remote directory))
(declare-function majutsu-gerrit--shared-spec "majutsu-gerrit"
                  (&optional remote directory))

(defgroup majutsu-gerrit-rest nil
  "Curl-based REST client for Gerrit."
  :group 'majutsu-gerrit)

(defcustom majutsu-gerrit-rest-endpoint-prefix "/a"
  "Default prefix for authenticated Gerrit REST endpoints."
  :type 'string
  :group 'majutsu-gerrit-rest)

(defcustom majutsu-gerrit-rest-curl-extra-args nil
  "Extra command line flags prepended to every curl invocation."
  :type '(repeat string)
  :group 'majutsu-gerrit-rest)

(defcustom majutsu-gerrit-rest-debug nil
  "When non-nil, log Gerrit REST requests without credentials."
  :type 'boolean
  :group 'majutsu-gerrit-rest)

(define-error 'majutsu-gerrit-rest-error "Gerrit REST request failed")

(defconst majutsu-gerrit-rest--xssi-prefix ")]}'\n"
  "Gerrit's JSON response prefix.")

(defconst majutsu-gerrit-rest--status-marker "MAJUTSU_GERRIT_STATUS:"
  "Marker appended by curl --write-out so we can parse HTTP status.")

;;;; Spec handling

(defun majutsu-gerrit-rest--bool-scheme (ssl)
  "Return a scheme string for SSL."
  (if ssl "https" "http"))

(defun majutsu-gerrit-rest-normalize-spec (spec)
  "Return normalized REST SPEC.

The normalized plist has keys :host, :scheme, :path, :prefix and :user.
This accepts both the normalized shape and the legacy/shared shape
returned by `majutsu-gerrit--shared-spec'."
  (cond
   ((null spec) nil)
   ((plist-get spec :host)
    (list :host (plist-get spec :host)
          :scheme (let ((scheme (plist-get spec :scheme)))
                    (cond ((symbolp scheme) (symbol-name scheme))
                          ((stringp scheme) scheme)
                          ((plist-member spec :ssl)
                           (majutsu-gerrit-rest--bool-scheme (plist-get spec :ssl)))
                          (t "https")))
          :path (or (plist-get spec :path) "")
          :prefix (or (plist-get spec :prefix)
                      majutsu-gerrit-rest-endpoint-prefix)
          :user (plist-get spec :user)))
   ((plist-get spec :gerrit-host)
    ;; `majutsu-gerrit--shared-spec' folds the web path into :gerrit-prefix
    ;; as PATH/a.  Split it back into :path (web context) and the /a REST
    ;; prefix so both REST URLs and browser URLs are correct.
    (let* ((prefix (or (plist-get spec :gerrit-prefix)
                       majutsu-gerrit-rest-endpoint-prefix))
           (path (if (string-suffix-p "/a" prefix)
                     (string-remove-suffix "/a" prefix)
                   "")))
      (list :host (plist-get spec :gerrit-host)
            :scheme (majutsu-gerrit-rest--bool-scheme (plist-get spec :ssl))
            :path path
            :prefix "/a"
            :user (plist-get spec :user))))
   (t
    (error "majutsu-gerrit-rest: cannot normalize spec %S" spec))))

(defun majutsu-gerrit-rest-current-spec (&optional remote directory)
  "Return a normalized REST spec discovered from REMOTE in DIRECTORY."
  (when (fboundp 'majutsu-gerrit--shared-spec)
    (when-let* ((shared (majutsu-gerrit--shared-spec remote directory)))
      (let ((spec (majutsu-gerrit-rest-normalize-spec shared)))
        (when (and (fboundp 'majutsu-gerrit--remote-user)
                   (not (plist-get spec :user)))
          (setq spec (plist-put spec :user
                                (majutsu-gerrit--remote-user remote directory))))
        spec))))

(defun majutsu-gerrit-rest--require-spec (spec)
  "Return normalized SPEC or the current discovered spec, or signal."
  (or (majutsu-gerrit-rest-normalize-spec spec)
      (majutsu-gerrit-rest-current-spec)
      (error "majutsu-gerrit-rest: no Gerrit REST endpoint discovered")))

;;;; URL and request construction

(defun majutsu-gerrit-rest-encode-id (id)
  "URL-encode ID for use as a single Gerrit REST path segment."
  (url-hexify-string (format "%s" id)))

(defun majutsu-gerrit-rest--path (&rest segments)
  "Build a Gerrit REST path from SEGMENTS.
String SEGMENTS are URL-encoded; symbol SEGMENTS are inserted verbatim
as fixed path components (e.g. `comments')."
  (mapconcat (lambda (segment)
               (concat "/" (if (symbolp segment)
                               (symbol-name segment)
                             (majutsu-gerrit-rest-encode-id segment))))
             segments ""))

(defun majutsu-gerrit-rest--repeat-params (key values)
  "Return alist params repeating KEY for each of VALUES."
  (mapcar (lambda (value) (cons key value)) values))

(defun majutsu-gerrit-rest-params (params)
  "Encode PARAMS as a query string.

PARAMS is an alist.  Repeated keys are emitted repeatedly.  A value of t
is encoded as a bare boolean flag (\"raw\" rather than \"raw=t\")."
  (mapconcat
   (lambda (cell)
     (let ((key (url-hexify-string (format "%s" (car cell))))
           (value (cdr cell)))
       (if (eq value t)
           key
         (concat key "=" (url-hexify-string (format "%s" (or value "")))))))
   params
   "&"))

(defun majutsu-gerrit-rest--normalize-path-part (part)
  "Normalize URL path PART with one leading slash and no trailing slash."
  (let ((part (replace-regexp-in-string "\\`/+\\|/+\\'" "" (or part ""))))
    (if (string-empty-p part) "" (concat "/" part))))

(defun majutsu-gerrit-rest--build-url (spec endpoint &optional params)
  "Build a full URL for ENDPOINT under SPEC with query PARAMS."
  (unless (and (stringp endpoint) (string-prefix-p "/" endpoint))
    (error "majutsu-gerrit-rest: endpoint must start with /: %S" endpoint))
  (concat (plist-get spec :scheme)
          "://"
          (plist-get spec :host)
          (majutsu-gerrit-rest--normalize-path-part (plist-get spec :path))
          (majutsu-gerrit-rest--normalize-path-part (plist-get spec :prefix))
          endpoint
          (when params
            (concat "?" (majutsu-gerrit-rest-params params)))))

(defun majutsu-gerrit-rest--auth-credentials (spec)
  "Return (USER . SECRET) for SPEC, or nil if no auth-source entry exists."
  (let* ((host (plist-get spec :host))
         (user (plist-get spec :user))
         (args (append (list :host host
                             :require (if user '(:secret) '(:user :secret))
                             :create nil
                             :max 1)
                       (when user (list :user user))))
         (entry (car (apply #'auth-source-search args))))
    (when entry
      (let ((secret (plist-get entry :secret)))
        (cons (or (plist-get entry :user) user)
              (if (functionp secret) (funcall secret) secret))))))

(defun majutsu-gerrit-rest--header-string (header)
  "Return HEADER formatted for curl."
  (pcase header
    (`(,name . ,value) (format "%s: %s" name value))
    ((pred stringp) header)
    (_ (error "majutsu-gerrit-rest: invalid header %S" header))))

(defun majutsu-gerrit-rest--has-header-p (headers name)
  "Return non-nil if HEADERS contains NAME, case-insensitively."
  (seq-some
   (lambda (header)
     (let ((text (majutsu-gerrit-rest--header-string header)))
       (string-prefix-p (downcase (concat name ":")) (downcase text))))
   headers))

(defun majutsu-gerrit-rest--body-string (data)
  "Return DATA as a request body string, or nil."
  (cond
   ((null data) nil)
   ((stringp data) data)
   (t (json-encode data))))

(defun majutsu-gerrit-rest--curl-args (spec method url data headers)
  "Return curl args for SPEC METHOD URL DATA and HEADERS."
  (let* ((auth (majutsu-gerrit-rest--auth-credentials spec))
         (body (majutsu-gerrit-rest--body-string data))
         (headers (copy-sequence headers))
         (args (append (list "--silent" "--show-error" "--location" "--compressed"
                             "--request" method
                             "--write-out"
                             (concat "\n" majutsu-gerrit-rest--status-marker "%{http_code}\n"))
                       majutsu-gerrit-rest-curl-extra-args)))
    (when auth
      (setq args (append args (list "--user" (format "%s:%s" (car auth) (cdr auth))))))
    (when (and body (not (majutsu-gerrit-rest--has-header-p headers "Content-Type")))
      (push '("Content-Type" . "application/json; charset=UTF-8") headers))
    (dolist (header (nreverse headers))
      (setq args (append args (list "--header" (majutsu-gerrit-rest--header-string header)))))
    (when body
      (setq args (append args (list "--data" body))))
    (append args (list url))))

(defun majutsu-gerrit-rest--safe-curl-args (args)
  "Return ARGS with --user credentials redacted."
  (let ((copy (copy-sequence args)))
    (cl-loop for tail on copy
             when (and (string= (car tail) "--user") (cdr tail))
             do (setcar (cdr tail) "<redacted>"))
    copy))

(defun majutsu-gerrit-rest--parse-raw-response ()
  "Parse current buffer into (:status STATUS :body BODY)."
  (goto-char (point-max))
  (unless (re-search-backward
           (concat "\n" (regexp-quote majutsu-gerrit-rest--status-marker)
                   "\\([0-9][0-9][0-9]\\)\n?\\'")
           nil t)
    (error "majutsu-gerrit-rest: curl response did not include HTTP status marker"))
  (let ((status (string-to-number (match-string 1)))
        (body (buffer-substring-no-properties (point-min) (match-beginning 0))))
    (list :status status :body body)))

(defun majutsu-gerrit-rest--ok-p (status)
  "Return non-nil if HTTP STATUS is a success."
  (and (<= 200 status) (< status 300)))

(defun majutsu-gerrit-rest--check-response (response)
  "Signal `majutsu-gerrit-rest-error' unless RESPONSE is successful."
  (unless (majutsu-gerrit-rest--ok-p (plist-get response :status))
    (signal 'majutsu-gerrit-rest-error
            (list (plist-get response :status)
                  (plist-get response :body)
                  (plist-get response :url))))
  response)

(defun majutsu-gerrit-rest--json-params (params)
  "Return PARAMS with pp=0 unless the caller already supplied pp."
  (if (assoc "pp" params)
      params
    (append params '(("pp" . "0")))))

(defun majutsu-gerrit-rest--strip-xssi (body)
  "Strip Gerrit's XSSI prefix from BODY if present."
  (if (string-prefix-p majutsu-gerrit-rest--xssi-prefix body)
      (substring body (length majutsu-gerrit-rest--xssi-prefix))
    body))

;;;; Public request API

(defun majutsu-gerrit-rest-request (method endpoint &optional data spec params headers)
  "Perform Gerrit REST METHOD request to ENDPOINT.

Return a plist (:status STATUS :body BODY :url URL).  DATA is encoded as
JSON unless it is already a string.  PARAMS is an alist query string and
HEADERS is a list of strings or (NAME . VALUE) cons cells."
  (let* ((spec (majutsu-gerrit-rest--require-spec spec))
         (method (upcase (format "%s" method)))
         (url (majutsu-gerrit-rest--build-url spec endpoint params))
         (args (majutsu-gerrit-rest--curl-args spec method url data headers)))
    (when majutsu-gerrit-rest-debug
      (message "majutsu-gerrit-rest: curl %S"
               (majutsu-gerrit-rest--safe-curl-args args)))
    (with-temp-buffer
      (let ((exit (apply #'majutsu-process-file "curl" nil t nil args)))
        (unless (integerp exit)
          (error "majutsu-gerrit-rest: curl did not return an exit status: %S" exit))
        (let ((response (majutsu-gerrit-rest--parse-raw-response)))
          (setq response (plist-put response :url url))
          ;; Curl can return non-zero for transport/TLS failures while still
          ;; not having an HTTP error status.  HTTP errors are represented by
          ;; the status code and handled by higher-level helpers.
          (when (and (not (zerop exit))
                     (majutsu-gerrit-rest--ok-p (plist-get response :status)))
            (error "majutsu-gerrit-rest: curl exited with status %s" exit))
          response)))))

(defun majutsu-gerrit-rest-json (method endpoint &optional data spec params headers)
  "Perform METHOD request to ENDPOINT and JSON-decode the response body."
  (let* ((headers (append headers '("Accept: application/json")))
         (params (majutsu-gerrit-rest--json-params params))
         (response (majutsu-gerrit-rest--check-response
                    (majutsu-gerrit-rest-request method endpoint data spec params headers)))
         (body (string-trim (majutsu-gerrit-rest--strip-xssi
                             (plist-get response :body)))))
    (if (string-empty-p body)
        nil
      (json-parse-string body
                         :object-type 'alist
                         :array-type 'list
                         :null-object :json-null
                         :false-object :json-false))))

(defun majutsu-gerrit-rest-text (method endpoint &optional data spec params headers)
  "Perform METHOD request to ENDPOINT and return the raw text body."
  (plist-get (majutsu-gerrit-rest--check-response
              (majutsu-gerrit-rest-request method endpoint data spec params headers))
             :body))

;;;; Endpoint helpers

(defun majutsu-gerrit-rest-config-version (&optional spec verbose)
  "Return Gerrit server version for SPEC.
If VERBOSE is non-nil, return the verbose VersionInfo object."
  (majutsu-gerrit-rest-json "GET" "/config/server/version" nil spec
                            (when verbose '(("verbose" . t)))))

(defun majutsu-gerrit-rest-config-info (&optional spec)
  "Return Gerrit ServerInfo for SPEC."
  (majutsu-gerrit-rest-json "GET" "/config/server/info" nil spec))

(defun majutsu-gerrit-rest-account-query (query &optional limit start options spec)
  "Query accounts matching QUERY.
LIMIT, START and OPTIONS map to Gerrit's n, S and repeated o params."
  (majutsu-gerrit-rest-json
   "GET" "/accounts/" nil spec
   (append `(("q" . ,query))
           (and limit `(("n" . ,limit)))
           (and start `(("S" . ,start)))
           (majutsu-gerrit-rest--repeat-params "o" options))))

(defun majutsu-gerrit-rest-account-query-with-details (query &optional limit spec)
  "Query accounts matching QUERY, asking for detailed account info."
  (majutsu-gerrit-rest-account-query query limit nil '(DETAILS) spec))

(defun majutsu-gerrit-rest-account-suggest (seed &optional limit spec)
  "Return account suggestions for SEED."
  (majutsu-gerrit-rest-json
   "GET" "/accounts/" nil spec
   (append `(("suggest" . t) ("q" . ,seed))
           (and limit `(("n" . ,limit))))))

(defun majutsu-gerrit-rest-account-self (&optional spec)
  "Return AccountInfo for the calling user."
  (majutsu-gerrit-rest-json "GET" "/accounts/self" nil spec))

(defun majutsu-gerrit-rest--change-query-params (queries limit start options)
  "Build repeated query params for change QUERIES."
  (append (majutsu-gerrit-rest--repeat-params
           "q" (if (listp queries) queries (list queries)))
          (and limit `(("n" . ,limit)))
          (and start `(("S" . ,start)))
          (majutsu-gerrit-rest--repeat-params "o" options)))

(defun majutsu-gerrit-rest-change-query (queries &optional limit start options spec)
  "Query changes using QUERIES.
QUERIES may be a string or a list of strings."
  (majutsu-gerrit-rest-json
   "GET" "/changes/" nil spec
   (majutsu-gerrit-rest--change-query-params queries limit start options)))

(defun majutsu-gerrit-rest-change-get (change-id &optional options spec)
  "Return ChangeInfo for CHANGE-ID."
  (majutsu-gerrit-rest-json
   "GET" (majutsu-gerrit-rest--path 'changes change-id) nil spec
   (majutsu-gerrit-rest--repeat-params "o" options)))

(defun majutsu-gerrit-rest-change-detail (change-id &optional options spec)
  "Return detailed ChangeInfo for CHANGE-ID."
  (majutsu-gerrit-rest-json
   "GET" (majutsu-gerrit-rest--path 'changes change-id 'detail) nil spec
   (majutsu-gerrit-rest--repeat-params "o" options)))

(defun majutsu-gerrit-rest--context-params (context-padding)
  "Return comment context params for CONTEXT-PADDING, or nil."
  (when context-padding
    `(("enable-context" . "true") ("context-padding" . ,context-padding))))

(defun majutsu-gerrit-rest-change-comments (change-id &optional spec context-padding)
  "Return published comments for CHANGE-ID."
  (majutsu-gerrit-rest-json
   "GET" (majutsu-gerrit-rest--path 'changes change-id 'comments) nil spec
   (majutsu-gerrit-rest--context-params context-padding)))

(defun majutsu-gerrit-rest-change-drafts (change-id &optional spec context-padding)
  "Return draft comments for CHANGE-ID belonging to the caller."
  (majutsu-gerrit-rest-json
   "GET" (majutsu-gerrit-rest--path 'changes change-id 'drafts) nil spec
   (majutsu-gerrit-rest--context-params context-padding)))

(defun majutsu-gerrit-rest-change-messages (change-id &optional spec)
  "Return messages for CHANGE-ID."
  (majutsu-gerrit-rest-json
   "GET" (majutsu-gerrit-rest--path 'changes change-id 'messages) nil spec))

(defun majutsu-gerrit-rest-revision-files (change-id revision-id &optional spec)
  "Return files for REVISION-ID of CHANGE-ID."
  (majutsu-gerrit-rest-json
   "GET" (majutsu-gerrit-rest--path 'changes change-id 'revisions revision-id 'files)
   nil spec))

(defun majutsu-gerrit-rest-revision-patch (change-id revision-id &optional spec params)
  "Return raw text patch for REVISION-ID of CHANGE-ID."
  (majutsu-gerrit-rest-text
   "GET" (majutsu-gerrit-rest--path 'changes change-id 'revisions revision-id 'patch)
   nil spec (append params '(("raw" . t)))))

(defun majutsu-gerrit-rest-revision-file-diff (change-id revision-id file-id
                                                         &optional spec params)
  "Return DiffInfo for FILE-ID in REVISION-ID of CHANGE-ID."
  (majutsu-gerrit-rest-json
   "GET" (majutsu-gerrit-rest--path 'changes change-id 'revisions revision-id
                                    'files file-id 'diff)
   nil spec params))

(defun majutsu-gerrit-rest-revision-ported-comments (change-id revision-id &optional spec)
  "Return comments ported to REVISION-ID of CHANGE-ID."
  (majutsu-gerrit-rest-json
   "GET" (majutsu-gerrit-rest--path 'changes change-id 'revisions revision-id
                                    'ported_comments)
   nil spec))

(defun majutsu-gerrit-rest-revision-ported-drafts (change-id revision-id &optional spec)
  "Return drafts ported to REVISION-ID of CHANGE-ID."
  (majutsu-gerrit-rest-json
   "GET" (majutsu-gerrit-rest--path 'changes change-id 'revisions revision-id
                                    'ported_drafts)
   nil spec))

(defun majutsu-gerrit-rest-review-post (change-id revision-id input &optional spec)
  "Post ReviewInput INPUT for REVISION-ID of CHANGE-ID."
  (majutsu-gerrit-rest-json
   "POST" (majutsu-gerrit-rest--path 'changes change-id 'revisions revision-id 'review)
   input spec))

(defun majutsu-gerrit-rest-review-publish-drafts (change-id revision-id &optional spec all-revisions)
  "Publish drafts for REVISION-ID of CHANGE-ID.
When ALL-REVISIONS is non-nil, publish all drafts on all revisions."
  (majutsu-gerrit-rest-review-post
   change-id revision-id
   `(("drafts" . ,(if all-revisions "PUBLISH_ALL_REVISIONS" "PUBLISH")))
   spec))

(provide 'majutsu-gerrit-rest)

;;; majutsu-gerrit-rest.el ends here
