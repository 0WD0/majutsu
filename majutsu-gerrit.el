;;; majutsu-gerrit.el --- Gerrit integration commands for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides shared helpers for Majutsu's native Gerrit
;; integration, including URL/spec discovery and account, topic, and
;; remote-branch completion.  Upload commands live in the dedicated
;; `majutsu-gerrit-upload' module.

;;; Code:

(require 'majutsu)
(require 'majutsu-completion)
(require 'majutsu-config)
(require 'majutsu-gerrit-rest)
(require 'majutsu-remote)

(require 'cl-lib)
(require 'consult)
(require 'json)
(require 'subr-x)
(require 'url-parse)
(require 'svg)
(require 'plz)

(declare-function majutsu-jj-lines "majutsu-jj")

;;; Customization

(defgroup majutsu-gerrit nil
  "Gerrit integration for Majutsu."
  :group 'majutsu)

;;; State

(defvar majutsu-gerrit-remote-branch-history nil
  "Minibuffer history for Gerrit remote branch names.")

(defvar majutsu-gerrit-reviewer-history nil
  "Minibuffer history for Gerrit reviewer values.")

(defvar majutsu-gerrit-cc-history nil
  "Minibuffer history for Gerrit CC values.")

(defvar majutsu-gerrit-topic-history nil
  "Minibuffer history for Gerrit topics.")

(defvar majutsu-gerrit-hashtag-history nil
  "Minibuffer history for Gerrit hashtags.")

(defvar majutsu-gerrit-label-history nil
  "Minibuffer history for Gerrit labels.")

(defcustom majutsu-gerrit-account-suggestion-limit 15
  "Maximum number of Gerrit account suggestions requested for completion."
  :type 'integer
  :group 'majutsu-gerrit)

(defcustom majutsu-gerrit-topic-suggestion-limit 100
  "Maximum number of open changes scanned for Gerrit topic completion."
  :type 'integer
  :group 'majutsu-gerrit)

(defcustom majutsu-gerrit-account-completion-strategy 'suggest
  "Strategy for Gerrit account completion.
The value `suggest' uses the optimized `/accounts/?suggest' endpoint,
which is intended for UI autocompletion and may be faster.  The value
`query' uses the regular `/accounts/?q=' endpoint with `DETAILS',
which supports account search operators and returns candidates even
for very short seeds."
  :type '(choice (const :tag "Use suggest endpoint" suggest)
          (const :tag "Use query endpoint" query))
  :group 'majutsu-gerrit)

(defcustom majutsu-gerrit-account-show-avatars t
  "Non-nil to show avatar images in Gerrit account completion."
  :type 'boolean
  :group 'majutsu-gerrit)

(defcustom majutsu-gerrit-account-avatar-size 16
  "Size in pixels for Gerrit account avatars in completion."
  :type 'integer
  :group 'majutsu-gerrit)

;;; Core command

(defun majutsu-gerrit--start (args &optional success-msg finish-callback)
  "Start `jj gerrit ARGS' asynchronously, for side-effects."
  (majutsu-start-jj (append '("gerrit") args) success-msg finish-callback))

(defun majutsu-gerrit--config-get (key)
  "Return jj config KEY, or nil if it is unset or unreadable."
  (condition-case nil
      (when-let* ((value (majutsu-get key))
                  ((not (string-empty-p value))))
        value)
    (error nil)))

(defun majutsu-gerrit--selected-remote (&optional args directory)
  "Return the currently selected Gerrit remote name.
ARGS defaults to current transient arguments.  DIRECTORY controls repo lookup."
  (let* ((default-directory (or directory default-directory))
         (args (or args (ignore-errors (transient-get-value))))
         (configured (majutsu-gerrit--config-get "gerrit.default-remote"))
         (remotes (majutsu-remote-names default-directory)))
    (or (transient-arg-value "--remote=" args)
        (and configured (member configured remotes) configured)
        (and (member "gerrit" remotes) "gerrit")
        (and (= (length remotes) 1) (car remotes)))))

(defun majutsu-gerrit--remote-entry (&optional remote directory)
  "Return plist metadata for Gerrit REMOTE in DIRECTORY."
  (let* ((default-directory (or directory default-directory))
         (payload (majutsu-remote-candidate-data default-directory))
         (entries (plist-get payload :entries))
         (remote (or remote (majutsu-gerrit--selected-remote nil default-directory))))
    (and entries remote (gethash remote entries))))

(defun majutsu-gerrit--remote-url (&optional remote directory)
  "Return push or fetch URL for Gerrit REMOTE in DIRECTORY."
  (when-let* ((entry (majutsu-gerrit--remote-entry remote directory)))
    (or (plist-get entry :push-url)
        (plist-get entry :fetch-url))))

(defun majutsu-gerrit--trim-trailing-slashes (string)
  "Return STRING without trailing slashes."
  (replace-regexp-in-string "/+\\'" "" (or string "")))

(defun majutsu-gerrit--url-default-port (scheme)
  "Return default port for URL SCHEME, or nil when unknown."
  (pcase scheme
    ("https" 443)
    ("http" 80)
    (_ nil)))

(defun majutsu-gerrit--host-with-port (host port scheme)
  "Return HOST optionally suffixed with PORT for URL SCHEME."
  (if (and port (not (= port (or (majutsu-gerrit--url-default-port scheme) -1))))
      (format "%s:%s" host port)
    host))

(defun majutsu-gerrit--remote-user (&optional remote directory)
  "Return the user component from Gerrit REMOTE in DIRECTORY, if any."
  (let ((remote-url (majutsu-gerrit--remote-url remote directory)))
    (cond
     ((not (stringp remote-url))
      nil)
     ((let* ((parsed (url-generic-parse-url remote-url))
             (scheme (url-type parsed)))
        (and scheme (not (string-empty-p scheme))))
      (url-user (url-generic-parse-url remote-url)))
     ((string-match "^\\([^@]+\\)@[^:]+:" remote-url)
      (match-string 1 remote-url)))))

(defun majutsu-gerrit--project-from-remote-url (remote-url)
  "Extract Gerrit project path from REMOTE-URL."
  (cond
   ((not (stringp remote-url))
    nil)
   ((let* ((parsed (url-generic-parse-url remote-url))
           (scheme (url-type parsed)))
      (and scheme (not (string-empty-p scheme))))
    (replace-regexp-in-string
     "^/?\\(.*?\\)\\(?:\\.git/?\\)?$" "\\1"
     (or (url-filename (url-generic-parse-url remote-url)) "")))
   ((string-match "^\\(?:[^@]+@\\)?[^:]+:\\(.*?\\)\\(?:\\.git/?\\)?$" remote-url)
    (match-string 1 remote-url))))

(defun majutsu-gerrit--base-url-from-review-url (review-url)
  "Normalize REVIEW-URL as a Gerrit web/API base URL."
  (when (and (stringp review-url) (not (string-empty-p review-url)))
    (let* ((parsed (url-generic-parse-url review-url))
           (scheme (url-type parsed))
           (host (url-host parsed))
           (port (url-port parsed))
           (path (majutsu-gerrit--trim-trailing-slashes
                  (or (url-filename parsed) ""))))
      (when (and scheme host)
        (concat scheme "://"
                (majutsu-gerrit--host-with-port host port scheme)
                path)))))

(defun majutsu-gerrit--base-url-from-remote-url (remote-url)
  "Derive Gerrit's web/API base URL from REMOTE-URL."
  (cond
   ((not (stringp remote-url))
    nil)
   ((let* ((parsed (url-generic-parse-url remote-url))
           (scheme (url-type parsed)))
      (and scheme (not (string-empty-p scheme))))
    (let* ((parsed (url-generic-parse-url remote-url))
           (scheme (url-type parsed))
           (display-scheme (if (member scheme '("http" "https")) scheme "https"))
           (host (url-host parsed))
           (port (and (member scheme '("http" "https"))
                      (url-port parsed))))
      (when host
        (concat display-scheme "://"
                (majutsu-gerrit--host-with-port host port display-scheme)))))
   ((string-match "^\\(?:[^@]+@\\)?\\([^:]+\\):" remote-url)
    (format "https://%s" (match-string 1 remote-url)))))

(defun majutsu-gerrit--shared-spec (&optional remote directory)
  "Return a reliable Gerrit web/API spec for REMOTE in DIRECTORY.
Only explicit web origins are accepted: `gerrit.review-url' or an HTTP(S)
remote.  SSH remotes are intentionally ignored because their ports are upload
ports, not Gerrit web/API ports."
  (when-let* ((base-url
               (or (when-let* ((review-url (majutsu-gerrit--config-get
                                            "gerrit.review-url")))
                     (majutsu-gerrit--base-url-from-review-url review-url))
                   (when-let* ((remote-url (majutsu-gerrit--remote-url remote directory))
                               (parsed (url-generic-parse-url remote-url))
                               (scheme (url-type parsed))
                               ((member scheme '("http" "https"))))
                     (majutsu-gerrit--base-url-from-remote-url remote-url))))
              (parsed (url-generic-parse-url base-url))
              (scheme (url-type parsed))
              (host (url-host parsed)))
    (let* ((port (url-port parsed))
           (host-with-port (majutsu-gerrit--host-with-port host port scheme))
           (path (majutsu-gerrit--trim-trailing-slashes
                  (or (url-filename parsed) ""))))
      (list :gerrit-host host-with-port
            :gerrit-prefix (concat (if (string-empty-p path) "" path) "/a")
            :ssl (equal scheme "https")))))

;;; Account helpers

(defun majutsu-gerrit--account-get (account key)
  "Return ACCOUNT field KEY, accepting symbol and string alist keys."
  (or (alist-get key account)
      (and (symbolp key) (alist-get (symbol-name key) account nil nil #'equal))))

(defun majutsu-gerrit--account-candidate (account)
  "Return the completion candidate string for ACCOUNT."
  (or (majutsu-gerrit--account-get account 'email)
      (majutsu-gerrit--account-get account 'username)
      (when-let* ((id (majutsu-gerrit--account-get account '_account_id)))
        (format "%s" id))))

(defun majutsu-gerrit--account-annotation (account candidate)
  "Return completion annotation body for ACCOUNT candidate CANDIDATE."
  (let ((body (string-join
               (delq nil
                     (list (majutsu-gerrit--account-get account 'name)
                           (majutsu-gerrit--account-get account 'display_name)
                           (when-let* ((username (majutsu-gerrit--account-get account 'username))
                                       ((not (equal username candidate))))
                             (format "@%s" username))
                           (when-let* ((email (majutsu-gerrit--account-get account 'email))
                                       ((not (equal email candidate))))
                             (format "<%s>" email))
                           (when-let* ((id (majutsu-gerrit--account-get account '_account_id)))
                             (format "#%s" id))))
               " ")))
    (unless (string-empty-p body)
      (propertize (concat " " body) 'face 'majutsu-completion-documentation))))

(defun majutsu-gerrit--account-avatar-url (account)
  "Return the first avatar URL for ACCOUNT, or nil."
  (when-let* ((avatars (majutsu-gerrit--account-get account 'avatars))
              ((consp avatars)))
    (or (majutsu-gerrit--account-get (car avatars) 'url)
        (cdar avatars))))

(defun majutsu-gerrit--account-initials (account)
  "Return initials for ACCOUNT, or a generic placeholder."
  (let ((name (or (majutsu-gerrit--account-get account 'name)
                  (majutsu-gerrit--account-get account 'username)
                  "?")))
    (if (string-empty-p name)
        "?"
      (upcase (substring name 0 1)))))

(defconst majutsu-gerrit--account-avatar-colors
  ["#1f77b4" "#ff7f0e" "#2ca02c" "#d62728" "#9467bd"
   "#8c564b" "#e377c2" "#7f7f7f" "#bcbd22" "#17becf"]
  "Palette used for generated account avatars.")

(defvar majutsu-gerrit--account-avatar-image-cache (make-hash-table :test #'equal)
  "Cache mapping avatar URL to a `create-image' image object.
The keyword `:missing' means the URL could not be turned into an image.")

(defvar majutsu-gerrit--account-avatar-fetching (make-hash-table :test #'equal)
  "Set of avatar URLs currently being fetched asynchronously.")

(defun majutsu-gerrit--account-avatar-color (account)
  "Return a stable background color for ACCOUNT."
  (let* ((seed (or (majutsu-gerrit--account-get account 'email)
                   (majutsu-gerrit--account-get account 'username)
                   (majutsu-gerrit--account-get account '_account_id)
                   ""))
         (hash (abs (sxhash (format "%s" seed)))))
    (aref majutsu-gerrit--account-avatar-colors
          (mod hash (length majutsu-gerrit--account-avatar-colors)))))

(defun majutsu-gerrit--account-avatar-svg (account)
  "Return an SVG image object for ACCOUNT."
  (let* ((size majutsu-gerrit-account-avatar-size)
         (initials (majutsu-gerrit--account-initials account))
         (color (majutsu-gerrit--account-avatar-color account))
         (svg (svg-create size size)))
    (svg-circle svg (/ size 2) (/ size 2) (/ size 2) :fill-color color)
    (svg-text svg initials
              :font-size (/ size 2)
              :font-weight "bold"
              :fill "white"
              :x "50%"
              :y "55%"
              :text-anchor "middle"
              :dominant-baseline "middle")
    (create-image (with-temp-buffer
                    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
                    (svg-print svg)
                    (buffer-string))
                  'svg t
                  :scale 1
                  :ascent 'center)))

(defun majutsu-gerrit--image-size (image)
  "Return size of IMAGE, or nil if it cannot be measured."
  (ignore-errors (image-size image t)))

(defun majutsu-gerrit--account-avatar-image-valid-p (image)
  "Return non-nil when IMAGE object appears renderable."
  (and image (majutsu-gerrit--image-size image) t))

(defun majutsu-gerrit--account-avatar-redisplay ()
  "Redisplay the current frame so newly cached avatars appear."
  (redisplay t))

(defun majutsu-gerrit--account-start-avatar-fetch (url)
  "Start asynchronous fetch of avatar URL.
Fetched bytes are turned into an image and cached; failures are cached
as `:missing' so we do not retry indefinitely."
  (when (and (fboundp 'plz)
             (not (gethash url majutsu-gerrit--account-avatar-fetching))
             (not (gethash url majutsu-gerrit--account-avatar-image-cache)))
    (puthash url t majutsu-gerrit--account-avatar-fetching)
    (plz 'get url
      :as 'binary
      :then (lambda (bytes)
              (let ((image (ignore-errors
                             (create-image bytes nil t
                                           :width majutsu-gerrit-account-avatar-size
                                           :height majutsu-gerrit-account-avatar-size
                                           :ascent 'center))))
                (puthash url
                         (if (majutsu-gerrit--account-avatar-image-valid-p image)
                             image
                           :missing)
                         majutsu-gerrit--account-avatar-image-cache)
                (remhash url majutsu-gerrit--account-avatar-fetching)
                (majutsu-gerrit--account-avatar-redisplay)))
      :else (lambda (_err)
              (puthash url :missing majutsu-gerrit--account-avatar-image-cache)
              (remhash url majutsu-gerrit--account-avatar-fetching)))))

(defun majutsu-gerrit--account-avatar-image (account)
  "Return a cached remote avatar image for ACCOUNT, or nil.
When the avatar URL is known but not yet cached, start an asynchronous
fetch and return nil so the caller can fall back to a generated avatar."
  (when-let* ((url (majutsu-gerrit--account-avatar-url account))
              ((not (eq (gethash url majutsu-gerrit--account-avatar-image-cache)
                        :missing))))
    (let ((cached (gethash url majutsu-gerrit--account-avatar-image-cache)))
      (cond
       ((majutsu-gerrit--account-avatar-image-valid-p cached)
        cached)
       (t
        (majutsu-gerrit--account-start-avatar-fetch url)
        nil)))))

(defun majutsu-gerrit--account-avatar-prefix (account)
  "Return prefix string displaying ACCOUNT's avatar, or nil."
  (when (and majutsu-gerrit-account-show-avatars
             (display-images-p))
    (let ((image (or (majutsu-gerrit--account-avatar-image account)
                     (majutsu-gerrit--account-avatar-svg account))))
      (concat (propertize " " 'display image 'rear-nonsticky '(display))
              " "))))

(defun majutsu-gerrit--account-annotate-candidate (candidate)
  "Return aligned annotation for CANDIDATE using its account text property.
When `majutsu-gerrit-account-show-avatars' is non-nil and the display
supports images, return a (CANDIDATE PREFIX SUFFIX) list so Consult
can render an avatar prefix."
  (when-let* ((account (get-text-property 0 'majutsu-gerrit-account candidate))
              (annotation (majutsu-gerrit--account-annotation account candidate)))
    (let ((aligned (consult--annotate-align candidate annotation)))
      (if-let* ((prefix (majutsu-gerrit--account-avatar-prefix account)))
          (list candidate prefix aligned)
        aligned))))

(defun majutsu-gerrit--account-transform (outputs)
  "Parse Gerrit JSON OUTPUTS into account candidate strings.
OUTPUTS is a list of raw strings emitted by the curl process."
  (mapcan
   (lambda (output)
     (when (and (stringp output) (not (string-empty-p output))
                (or (string-prefix-p "[" output)
                    (string-prefix-p ")]}'" output)))
       (let* ((body (string-trim (majutsu-gerrit-rest--strip-xssi output)))
              (accounts (condition-case nil
                            (json-parse-string body
                                               :object-type 'alist
                                               :array-type 'list
                                               :null-object nil
                                               :false-object nil)
                          (error nil))))
         (delq nil
               (mapcar (lambda (account)
                         (when-let* ((candidate (majutsu-gerrit--account-candidate account)))
                           (put-text-property 0 (length candidate)
                                              'majutsu-gerrit-account account candidate)
                           candidate))
                       accounts)))))
   outputs))

(defun majutsu-gerrit--account-request (input remote)
  "Return (URL . HEADERS) for an account suggestion request, or nil.
HEADERS includes auth-source Basic credentials when available."
  (when-let* ((input (string-trim input))
              ((not (string-empty-p input)))
              (spec (condition-case nil
                        (majutsu-gerrit-rest-current-spec remote)
                      (error nil))))
    (let* ((params (pcase majutsu-gerrit-account-completion-strategy
                     ('query
                      `(("q" . ,input)
                        ("n" . ,majutsu-gerrit-account-suggestion-limit)
                        ("o" . "DETAILS")
                        ("pp" . "0")))
                     (_
                      `(("suggest" . t)
                        ("q" . ,input)
                        ("n" . ,majutsu-gerrit-account-suggestion-limit)
                        ("pp" . "0")))))
           (url (majutsu-gerrit-rest--build-url spec "/accounts/" params))
           (headers (majutsu-gerrit-rest--headers spec nil
                                                  '(("Accept" . "application/json")))))
      (cons url headers))))

(defun majutsu-gerrit--async-accounts (builder)
  "Consult async function for Gerrit account completion via `plz'.
BUILDER takes an input string and returns (URL . HEADERS), or nil.

This mirrors `consult--async-process' but drives `plz' instead of
`make-process': `plz' returns a curl process object for async
requests, which we cancel on new input.  Candidates are parsed with
`majutsu-gerrit--account-transform' and pushed through the sink."
  (lambda (sink)
    (let (proc last-input)
      (lambda (action)
        (pcase action
          ((pred stringp)
           (funcall sink action)
           (if (equal action last-input)
               nil
             (setq last-input action)
             (when proc
               (delete-process proc)
               (setq proc nil))
             (when-let* ((req (funcall builder action)))
               (let ((url (car req))
                     (headers (cdr req)))
                 (funcall sink [indicator running])
                 (setq proc
                       (plz 'get url
                         :headers headers
                         :as 'string
                         :then (lambda (body)
                                 (setq proc nil)
                                 (funcall sink 'flush)
                                 ;; Send the raw body as a single-element
                                 ;; list; the pipeline's transform stage
                                 ;; parses it with
                                 ;; `majutsu-gerrit--account-transform'.
                                 (funcall sink (list body))
                                 (funcall sink [indicator finished]))
                         :else (lambda (_err)
                                 (setq proc nil)
                                 (funcall sink [indicator failed]))))))))
          ((or 'cancel 'destroy)
           (when proc
             (delete-process proc)
             (setq proc nil))
           (setq last-input nil)
           (funcall sink action))
          (_ (funcall sink action)))))))

;;;###autoload
(defun majutsu-gerrit-read-account (prompt initial-input history &optional remote)
  "Read one Gerrit account using asynchronous REST-backed completion."
  (let ((consult-async-split-style 'none))
    (consult--read
     (consult--async-pipeline
      (consult--async-min-input 1)
      (consult--async-throttle 0.1 0.2)
      (majutsu-gerrit--async-accounts
       (lambda (input)
         (majutsu-gerrit--account-request input remote)))
      (consult--async-transform #'majutsu-gerrit--account-transform))
     :prompt (format-prompt prompt nil)
     :initial initial-input
     :history history
     :require-match nil
     :category 'majutsu-gerrit-account
     :sort nil
     :annotate #'majutsu-gerrit--account-annotate-candidate)))

;;;###autoload
(defun majutsu-gerrit-read-accounts (prompt initial-input history &optional remote)
  "Read Gerrit accounts using asynchronous REST-backed completion.
The user can select multiple accounts in a loop; an empty input finishes."
  (let ((selected nil))
    (cl-block done
      (while t
        (let ((input (majutsu-gerrit-read-account
                      (if selected
                          (format "%s (%s selected, RET to finish)"
                                  prompt (string-join selected ", "))
                        prompt)
                      (and (null selected) initial-input)
                      history remote)))
          (if (or (null input) (string-empty-p input))
              (cl-return-from done (nreverse selected))
            (push input selected)))))))

(defun majutsu-gerrit--make-counted-string-payload (items category label)
  "Build payload from ITEMS with count annotations.
CATEGORY names the completion category and LABEL is shown in annotations."
  (let ((entries (make-hash-table :test #'equal))
        (annotations (make-hash-table :test #'equal))
        (counts (make-hash-table :test #'equal))
        candidates)
    (dolist (item items)
      (when (and (stringp item) (not (string-empty-p item)))
        (puthash item (1+ (gethash item counts 0)) counts)
        (push item candidates)))
    (setq candidates (sort (delete-dups candidates) #'string<))
    (dolist (candidate candidates)
      (puthash candidate
               (list :value candidate :count (gethash candidate counts 0))
               entries)
      (puthash candidate
               (format "%s (%d open)" label (gethash candidate counts 0))
               annotations))
    (and candidates
         (list :category category
               :candidates candidates
               :entries entries
               :annotations annotations))))

;;;###autoload
(defun majutsu-gerrit-topic-candidate-data (&optional remote)
  "Return Gerrit topic completion payload for open changes."
  (let* ((project (majutsu-gerrit--project-from-remote-url
                   (majutsu-gerrit--remote-url remote)))
         (query (string-join (delq nil (list "is:open"
                                             (and project
                                                  (concat "project:" project))))
                             " ")))
    (condition-case nil
        (when-let* ((spec (majutsu-gerrit-rest-current-spec remote))
                    (changes (majutsu-gerrit-rest-change-query
                              query majutsu-gerrit-topic-suggestion-limit nil
                              nil spec)))
          (majutsu-gerrit--make-counted-string-payload
           (mapcar (lambda (change) (alist-get 'topic change)) changes)
           'majutsu-gerrit-topic "topic"))
      (error nil))))

;;; Remote branch helpers

(defconst majutsu-gerrit--remote-branch-template
  "if(remote, name ++ \"\\t\" ++ remote ++ \"\\n\", \"\")"
  "Template used to enumerate Gerrit remote branch candidates.")

(defun majutsu-gerrit--remote-branch-entry (name remotes)
  "Return structured remote branch entry for NAME and REMOTES."
  (list :name name :remotes (sort (delete-dups (copy-sequence remotes)) #'string<)))

(defun majutsu-gerrit--remote-branch-suffix (entry)
  "Return aligned completion suffix for remote branch ENTRY."
  (majutsu-completion-annotation
   (majutsu-completion-column "remote branch" 14 'majutsu-completion-key)
   (majutsu-completion-field
    (string-join (plist-get entry :remotes) ",")
    'majutsu-completion-type)))

;;;###autoload
(defun majutsu-gerrit-remote-branch-candidate-data (&optional remote)
  "Return completion payload for Gerrit remote branch names.
When REMOTE is non-nil, only branches from that remote are returned."
  (majutsu-with-toplevel
    (let ((sources (make-hash-table :test #'equal))
          (entries (make-hash-table :test #'equal))
          (annotations (make-hash-table :test #'equal)))
      (condition-case _
          (dolist (line (apply #'majutsu-jj-lines
                               (append (list "bookmark" "list" "--quiet")
                                       (if remote
                                           (list "--remote" remote)
                                         '("--all-remotes"))
                                       (list "-T" majutsu-gerrit--remote-branch-template))))
            (pcase-let ((`(,name ,source . ,_)
                         (split-string (substring-no-properties line) "\t")))
              (when (and name source
                         (not (string-empty-p name))
                         (not (string-empty-p source)))
                (puthash name
                         (majutsu--append-unique (gethash name sources) source)
                         sources))))
        (error nil))
      (let ((candidates (sort (hash-table-keys sources) #'string<)))
        (dolist (candidate candidates)
          (let ((entry (majutsu-gerrit--remote-branch-entry
                        candidate (gethash candidate sources))))
            (puthash candidate entry entries)
            (puthash candidate
                     (format "remote%s: %s"
                             (if (cdr (plist-get entry :remotes)) "s" "")
                             (string-join (plist-get entry :remotes) ","))
                     annotations)))
        (list :category 'majutsu-gerrit-remote-branch
              :candidates candidates
              :entries entries
              :annotations annotations
              :annotation-suffix-function
              (majutsu-completion-entry-suffix-function
               entries #'majutsu-gerrit--remote-branch-suffix))))))

(defun majutsu-gerrit--remote-branch-names (&optional remote)
  "Return a list of remote branch names for REMOTE.
If REMOTE is nil, return branches from all remotes."
  (plist-get (majutsu-gerrit-remote-branch-candidate-data remote)
             :candidates))

(provide 'majutsu-gerrit)

;;; majutsu-gerrit.el ends here
