;;; majutsu-gerrit.el --- Gerrit integration commands for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library wraps jj's Gerrit commands and exposes them through
;; Majutsu transients.
;;
;; When optional external Gerrit clients such as gerrit.el or egerrit are
;; already available, Majutsu borrows their completion helpers at runtime.
;; Those packages are not required dependencies.

;;; Code:

(require 'majutsu)
(require 'majutsu-completion)
(require 'majutsu-config)
(require 'majutsu-remote)
(require 'majutsu-selection)

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-parse)

(declare-function majutsu-jj-lines "majutsu-jj")
(declare-function majutsu-transient-read-remote-name "majutsu-remote")

(declare-function gerrit-get-accounts-alist "gerrit")
(declare-function gerrit-rest-open-reviews-for-project "gerrit-rest")
(declare-function gerrit-upload:--read-topic "gerrit")

(declare-function egerrit-users "egerrit")
(declare-function egerrit--read-changes "egerrit-core")
(declare-function egerrit--create-change "egerrit-core")
(declare-function egerrit-get-detailed-change "egerrit")
(declare-function egerrit--review-label-candidates "egerrit-review")

(defvar gerrit-host)
(defvar gerrit-use-ssl)
(defvar gerrit-rest-endpoint-prefix)
(defvar gerrit--accounts-alist)

(defvar egerrit-request-url)
(defvar egerrit-authenticate)
(defvar egerrit-user-name)

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
            :egerrit-url (concat host-with-port path)
            :ssl (equal scheme "https")))))

(defun majutsu-gerrit--gerrit-explicit-spec ()
  "Return explicit gerrit.el connection settings when configured by the user."
  (when-let* ((host (and (boundp 'gerrit-host) gerrit-host))
              (host (string-trim host))
              ((not (string-empty-p host))))
    (list :gerrit-host host
          :gerrit-prefix (if (and (boundp 'gerrit-rest-endpoint-prefix)
                                  (stringp gerrit-rest-endpoint-prefix)
                                  (not (string-empty-p gerrit-rest-endpoint-prefix)))
                             gerrit-rest-endpoint-prefix
                           "/a")
          :ssl (if (boundp 'gerrit-use-ssl) gerrit-use-ssl t))))

(defun majutsu-gerrit--egerrit-explicit-spec ()
  "Return explicit egerrit connection settings when configured by the user."
  (when-let* ((url (and (boundp 'egerrit-request-url) egerrit-request-url))
              (url (string-trim url))
              ((not (string-empty-p url)))
              ((not (equal url "www.gerrit.com"))))
    (list :egerrit-url url
          :ssl (if (boundp 'egerrit-authenticate) egerrit-authenticate t))))

(defun majutsu-gerrit--optional-require (feature)
  "Try to require optional FEATURE."
  (condition-case nil
      (require feature nil t)
    (error nil)))

(defun majutsu-gerrit--gerrit-available-p ()
  "Return non-nil when gerrit.el is available for borrowing."
  (majutsu-gerrit--optional-require 'gerrit))

(defun majutsu-gerrit--egerrit-available-p ()
  "Return non-nil when egerrit is available for borrowing."
  (majutsu-gerrit--optional-require 'egerrit))

(defun majutsu-gerrit--with-gerrit-context (fn &optional remote directory)
  "Call FN with borrowed gerrit.el context, or nil if unavailable."
  (when-let* ((_ (majutsu-gerrit--gerrit-available-p))
              (spec (or (majutsu-gerrit--gerrit-explicit-spec)
                        (majutsu-gerrit--shared-spec remote directory))))
    (condition-case nil
        (let ((gerrit-host (plist-get spec :gerrit-host))
              (gerrit-use-ssl (plist-get spec :ssl))
              (gerrit-rest-endpoint-prefix (plist-get spec :gerrit-prefix))
              (gerrit--accounts-alist nil))
          (funcall fn spec))
      (error nil))))

(defun majutsu-gerrit--with-egerrit-context (fn &optional remote directory)
  "Call FN with borrowed egerrit context, or nil if unavailable."
  (when-let* ((_ (majutsu-gerrit--egerrit-available-p))
              (spec (or (majutsu-gerrit--egerrit-explicit-spec)
                        (majutsu-gerrit--shared-spec remote directory))))
    (condition-case nil
        (let ((egerrit-request-url (plist-get spec :egerrit-url))
              (egerrit-authenticate (plist-get spec :ssl))
              (egerrit-user-name (majutsu-gerrit--remote-user remote directory))
              (orig-auth-source-search (symbol-function 'auth-source-search)))
          ;; egerrit uses :create t, which would prompt during completion.
          (cl-letf (((symbol-function 'auth-source-search)
                     (lambda (&rest args)
                       (apply orig-auth-source-search
                              (plist-put (copy-sequence args) :create nil)))))
            (funcall fn spec)))
      (error nil))))

;;; Borrowed completion providers

(defun majutsu-gerrit--account-matches-seed-p (account seed)
  "Return non-nil when ACCOUNT matches SEED.
ACCOUNT is an alist or plist-like entry from an external Gerrit client."
  (or (null seed)
      (string-empty-p seed)
      (let ((seed (downcase seed))
            (name (downcase (or (alist-get 'name account) "")))
            (username (downcase (or (alist-get 'username account) "")))
            (email (downcase (or (alist-get 'email account) ""))))
        (or (string-match-p (regexp-quote seed) name)
            (string-match-p (regexp-quote seed) username)
            (string-match-p (regexp-quote seed) email)))))

(defun majutsu-gerrit--make-account-payload (accounts &optional seed)
  "Build a structured completion payload from Gerrit ACCOUNTS.
SEED filters the candidates when non-nil."
  (let ((entries (make-hash-table :test #'equal))
        (annotations (make-hash-table :test #'equal))
        candidates)
    (dolist (account accounts)
      (let* ((account (if (and (consp account)
                               (not (consp (car account)))
                               (listp (cdr account)))
                          (cdr account)
                        account))
             (name (alist-get 'name account))
             (username (alist-get 'username account))
             (email (alist-get 'email account))
             (candidate (or email username))
             (parts (delq nil
                          (list name
                                (and username
                                     (not (equal username candidate))
                                     (format "@%s" username))
                                (and email
                                     (not (equal email candidate))
                                     (format "<%s>" email))))))
        (when (and candidate
                   (majutsu-gerrit--account-matches-seed-p account seed))
          (push candidate candidates)
          (puthash candidate account entries)
          (puthash candidate (string-join parts " ") annotations))))
    (setq candidates (sort (delete-dups candidates) #'string<))
    (and candidates
         (list :category 'majutsu-gerrit-account
               :candidates candidates
               :entries entries
               :annotations annotations))))

(defun majutsu-gerrit--gerrit-account-candidate-data (&optional remote directory seed)
  "Borrow reviewer/account candidates from gerrit.el."
  (majutsu-gerrit--with-gerrit-context
   (lambda (_spec)
     (majutsu-gerrit--make-account-payload (gerrit-get-accounts-alist) seed))
   remote directory))

(defun majutsu-gerrit--egerrit-account-candidate-data (&optional remote directory seed)
  "Borrow reviewer/account candidates from egerrit."
  (majutsu-gerrit--with-egerrit-context
   (lambda (_spec)
     (when-let* ((users (egerrit-users seed)))
       (let ((entries (make-hash-table :test #'equal))
             (annotations (make-hash-table :test #'equal))
             candidates)
         (dolist (user users)
           (let ((display (car user))
                 (candidate (cdr user)))
             (when (and (stringp candidate) (not (string-empty-p candidate)))
               (push candidate candidates)
               (puthash candidate user entries)
               (puthash candidate (string-trim display) annotations))))
         (setq candidates (sort (delete-dups candidates) #'string<))
         (and candidates
              (list :category 'majutsu-gerrit-account
                    :candidates candidates
                    :entries entries
                    :annotations annotations)))))
   remote directory))

(defun majutsu-gerrit-account-candidate-data (&optional remote directory seed)
  "Return borrowed reviewer/account completion payload.
Prefer gerrit.el and fall back to egerrit."
  (or (majutsu-gerrit--gerrit-account-candidate-data remote directory seed)
      (majutsu-gerrit--egerrit-account-candidate-data remote directory seed)))

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

(defun majutsu-gerrit--gerrit-topic-candidate-data (&optional remote directory)
  "Borrow topic candidates from gerrit.el."
  (let ((project (majutsu-gerrit--project-from-remote-url
                  (majutsu-gerrit--remote-url remote directory))))
    (when project
      (majutsu-gerrit--with-gerrit-context
       (lambda (_spec)
         (majutsu-gerrit--make-counted-string-payload
          (mapcar (lambda (change) (alist-get 'topic change))
                  (or (gerrit-rest-open-reviews-for-project project) '()))
          'majutsu-gerrit-topic
          "topic"))
       remote directory))))

(defun majutsu-gerrit--egerrit-topic-candidate-data (&optional remote directory)
  "Borrow topic candidates from egerrit."
  (let ((project (majutsu-gerrit--project-from-remote-url
                  (majutsu-gerrit--remote-url remote directory))))
    (when project
      (majutsu-gerrit--with-egerrit-context
       (lambda (_spec)
         (majutsu-gerrit--make-counted-string-payload
          (mapcar (lambda (change) (alist-get 'topic change))
                  (or (egerrit--read-changes
                       (format "status:open project:%s" project))
                      '()))
          'majutsu-gerrit-topic
          "topic"))
       remote directory))))

(defun majutsu-gerrit-topic-candidate-data (&optional remote directory)
  "Return borrowed topic completion payload.
Prefer gerrit.el and fall back to egerrit."
  (or (majutsu-gerrit--gerrit-topic-candidate-data remote directory)
      (majutsu-gerrit--egerrit-topic-candidate-data remote directory)))

(defun majutsu-gerrit--egerrit-label-candidate-data (&optional remote directory)
  "Borrow label candidates from egerrit's review label logic."
  (let* ((remote-url (majutsu-gerrit--remote-url remote directory))
         (project (majutsu-gerrit--project-from-remote-url remote-url)))
    (when project
      (majutsu-gerrit--with-egerrit-context
       (lambda (_spec)
         (when-let* ((json-changes (egerrit--read-changes
                                    (format "status:open project:%s" project)
                                    1))
                     (change (egerrit--create-change (car json-changes)))
                     (detailed-change (egerrit-get-detailed-change change))
                     (candidates (egerrit--review-label-candidates detailed-change)))
           (let ((entries (make-hash-table :test #'equal))
                 (annotations (make-hash-table :test #'equal))
                 values)
             (dolist (candidate candidates)
               (let* ((display (car candidate))
                      (label (car (cdr candidate)))
                      (vote (cdr (cdr candidate)))
                      (value (if (equal vote "+1")
                                 label
                               (concat label vote))))
                 (push value values)
                 (puthash value candidate entries)
                 (puthash value (string-trim display) annotations)))
             (setq values (sort (delete-dups values) #'string<))
             (and values
                  (list :category 'majutsu-gerrit-label
                        :candidates values
                        :entries entries
                        :annotations annotations)))))
       remote directory))))

(defun majutsu-gerrit-label-candidate-data (&optional remote directory)
  "Return borrowed label completion payload.
Label completion is borrowed from egerrit only."
  (majutsu-gerrit--egerrit-label-candidate-data remote directory))

;;; Native Majutsu readers

(defconst majutsu-gerrit--remote-branch-template
  "concat(if(self.primary().remote(), self.primary().name() ++ \"\t\" ++ self.primary().remote() ++ \"\n\", \"\"), self.tracked_refs().map(|ref| if(ref.remote(), ref.name() ++ \"\t\" ++ ref.remote() ++ \"\n\", \"\")).join(\"\"))"
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

(defun majutsu-gerrit--remote-branch-candidate-data (&optional remote)
  "Return completion payload for Gerrit remote branch names.
When REMOTE is non-nil, only branches from that remote are returned."
  (let ((default-directory (ignore-errors (majutsu--toplevel-safe)))
        (sources (make-hash-table :test #'equal))
        (entries (make-hash-table :test #'equal))
        (annotations (make-hash-table :test #'equal)))
    (when default-directory
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
        (error nil)))
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
             entries #'majutsu-gerrit--remote-branch-suffix)))))

(defun majutsu-gerrit--remote-branch-names (&optional remote)
  "Return a list of remote branch names for REMOTE.
If REMOTE is nil, return branches from all remotes."
  (plist-get (majutsu-gerrit--remote-branch-candidate-data remote) :candidates))

(defun majutsu-gerrit--account-query-seed (initial-input)
  "Return INITIAL-INPUT when it is suitable as an account query seed."
  (when-let* ((seed (and initial-input (string-trim initial-input))))
    (unless (or (string-empty-p seed)
                (string-match-p "[,[:space:]]" seed))
      seed)))

(defun majutsu-gerrit-upload--read-revset (prompt initial-input _history)
  "Read revset for `jj gerrit upload --revision='."
  (majutsu-read-revset prompt initial-input '("gerrit" "upload" "-r")))

(defun majutsu-gerrit-upload--read-remote-branch (prompt initial-input _history)
  "Read target remote branch for `jj gerrit upload' with completion."
  (let* ((args (transient-get-value))
         (remote (transient-arg-value "--remote=" args)))
    (majutsu-completing-read-payload
     prompt (majutsu-gerrit--remote-branch-candidate-data remote)
     nil nil initial-input 'majutsu-gerrit-remote-branch-history
     nil 'majutsu-gerrit-remote-branch)))

(defun majutsu-gerrit-upload--read-reviewer (prompt initial-input history)
  "Read one or more Gerrit reviewers.
Prefer optional external Gerrit integrations when available."
  (let* ((root (or (ignore-errors (majutsu--toplevel-safe)) default-directory))
         (payload (majutsu-gerrit-account-candidate-data
                   nil root (majutsu-gerrit--account-query-seed initial-input))))
    (if payload
        (majutsu-completing-read-multiple-payload
         prompt payload nil nil initial-input
         (or history 'majutsu-gerrit-reviewer-history)
         nil 'majutsu-gerrit-account nil root)
      (majutsu-completing-read-multiple
       prompt '() nil nil initial-input
       (or history 'majutsu-gerrit-reviewer-history)
       nil 'majutsu-gerrit-account))))

(defun majutsu-gerrit-upload--read-cc (prompt initial-input history)
  "Read one or more Gerrit CC values.
Prefer optional external Gerrit integrations when available."
  (let* ((root (or (ignore-errors (majutsu--toplevel-safe)) default-directory))
         (payload (majutsu-gerrit-account-candidate-data
                   nil root (majutsu-gerrit--account-query-seed initial-input))))
    (if payload
        (majutsu-completing-read-multiple-payload
         prompt payload nil nil initial-input
         (or history 'majutsu-gerrit-cc-history)
         nil 'majutsu-gerrit-account nil root)
      (majutsu-completing-read-multiple
       prompt '() nil nil initial-input
       (or history 'majutsu-gerrit-cc-history)
       nil 'majutsu-gerrit-account))))

(defun majutsu-gerrit-upload--read-topic (prompt initial-input history)
  "Read a Gerrit topic.
Prefer optional external Gerrit integrations when available."
  (let* ((root (or (ignore-errors (majutsu--toplevel-safe)) default-directory))
         (payload (majutsu-gerrit-topic-candidate-data nil root)))
    (cond
     (payload
      (majutsu-completing-read-payload
       prompt payload nil nil initial-input
       (or history 'majutsu-gerrit-topic-history)
       nil 'majutsu-gerrit-topic nil root))
     ((and (majutsu-gerrit--gerrit-available-p)
           (fboundp 'gerrit-upload:--read-topic))
      (majutsu-gerrit--with-gerrit-context
       (lambda (_spec)
         (gerrit-upload:--read-topic prompt initial-input
                                     (or history 'majutsu-gerrit-topic-history)))
       nil root))
     (t
      (majutsu-read-string prompt initial-input
                           (or history 'majutsu-gerrit-topic-history))))))

(defun majutsu-gerrit-upload--read-hashtag (prompt initial-input history)
  "Read one or more Gerrit hashtags.
This remains free-form because the optional providers do not expose a
stable hashtag completion helper."
  (majutsu-completing-read-multiple
   prompt '() nil nil initial-input
   (or history 'majutsu-gerrit-hashtag-history)
   nil 'majutsu-gerrit-hashtag))

(defun majutsu-gerrit-upload--read-label (prompt initial-input history)
  "Read one or more Gerrit labels.
Prefer egerrit's label candidate logic when available."
  (let* ((root (or (ignore-errors (majutsu--toplevel-safe)) default-directory))
         (payload (majutsu-gerrit-label-candidate-data nil root)))
    (if payload
        (majutsu-completing-read-multiple-payload
         prompt payload nil nil initial-input
         (or history 'majutsu-gerrit-label-history)
         nil 'majutsu-gerrit-label nil root)
      (majutsu-completing-read-multiple
       prompt '() nil nil initial-input
       (or history 'majutsu-gerrit-label-history)
       nil 'majutsu-gerrit-label))))

(defun majutsu-gerrit-upload-arguments ()
  "Return current Gerrit upload arguments.

When called from `majutsu-gerrit-upload-transient', use the transient's
arguments.  Otherwise leave the revision unset so `jj gerrit upload' can use
its own @/@- default."
  (if (eq transient-current-command 'majutsu-gerrit-upload-transient)
      (transient-args 'majutsu-gerrit-upload-transient)
    '()))

;;;###autoload
(defun majutsu-gerrit-upload (args)
  "Upload changes to Gerrit with ARGS."
  (interactive (list (majutsu-gerrit-upload-arguments)))
  (majutsu--message-with-log "Uploading to Gerrit...")
  (majutsu-gerrit--start
   (append '("upload") args)
   (if (member "--dry-run" args)
       "Gerrit upload dry run completed"
     "Uploaded to Gerrit")))

(defun majutsu-gerrit-upload--repo-args (args)
  "Keep only stable `jj gerrit upload' ARGS for repository defaults."
  (seq-filter (lambda (arg)
                (or (transient-arg-value "--remote=" (list arg))
                    (transient-arg-value "--remote-branch=" (list arg))
                    (transient-arg-value "--notify=" (list arg))
                    (member arg '("--ignore-attention-set"))))
              args))

(defclass majutsu-gerrit-upload-option (majutsu-selection-option) ())

(transient-define-argument majutsu-gerrit-upload:--revision ()
  :description "Revision"
  :class 'majutsu-gerrit-upload-option
  :selection-label "[REV]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :locate-fn (##majutsu-selection-find-section % 'jj-commit)
  :shortarg "-r"
  :argument "--revision="
  :multi-value 'repeat
  :prompt "Revision"
  :reader #'majutsu-gerrit-upload--read-revset)

(transient-define-argument majutsu-gerrit-upload:revision ()
  :description "Revision (toggle at point)"
  :class 'majutsu-selection-toggle-option
  :key "r"
  :argument "--revision="
  :multi-value 'repeat)

(transient-define-argument majutsu-gerrit-upload:--remote-branch ()
  :description "Remote branch"
  :class 'transient-option
  :shortarg "-b"
  :argument "--remote-branch="
  :prompt "Remote branch"
  :reader #'majutsu-gerrit-upload--read-remote-branch)

(transient-define-argument majutsu-gerrit-upload:--remote ()
  :description "Remote"
  :class 'transient-option
  :key "-R"
  :argument "--remote="
  :prompt "Remote"
  :reader #'majutsu-transient-read-remote-name)

(transient-define-argument majutsu-gerrit-upload:--reviewer ()
  :description "Reviewer"
  :class 'transient-option
  :key "-v"
  :argument "--reviewer="
  :multi-value 'repeat
  :prompt "Reviewer"
  :reader #'majutsu-gerrit-upload--read-reviewer)

(transient-define-argument majutsu-gerrit-upload:--cc ()
  :description "CC"
  :class 'transient-option
  :key "-C"
  :argument "--cc="
  :multi-value 'repeat
  :prompt "CC"
  :reader #'majutsu-gerrit-upload--read-cc)

(transient-define-argument majutsu-gerrit-upload:--label ()
  :description "Label"
  :class 'transient-option
  :shortarg "-l"
  :argument "--label="
  :multi-value 'repeat
  :prompt "Label"
  :reader #'majutsu-gerrit-upload--read-label)

(transient-define-argument majutsu-gerrit-upload:--topic ()
  :description "Topic"
  :class 'transient-option
  :key "-T"
  :argument "--topic="
  :prompt "Topic"
  :reader #'majutsu-gerrit-upload--read-topic)

(transient-define-argument majutsu-gerrit-upload:--hashtag ()
  :description "Hashtag"
  :class 'transient-option
  :key "-H"
  :argument "--hashtag="
  :multi-value 'repeat
  :prompt "Hashtag"
  :reader #'majutsu-gerrit-upload--read-hashtag)

(transient-define-argument majutsu-gerrit-upload:--message ()
  :description "Patch set message"
  :class 'transient-option
  :shortarg "-m"
  :argument "--message="
  :prompt "Patch set message: ")

(transient-define-argument majutsu-gerrit-upload:--notify ()
  :description "Notify"
  :class 'transient-option
  :key "-N"
  :argument "--notify="
  :choices '("none" "owner" "owner-reviewers" "all")
  :prompt "Notify: ")

(transient-define-argument majutsu-gerrit-upload:--deadline ()
  :description "Deadline"
  :class 'transient-option
  :key "-D"
  :argument "--deadline="
  :prompt "Deadline: ")

(transient-define-argument majutsu-gerrit-upload:--custom ()
  :description "Custom key=value"
  :class 'transient-option
  :key "-K"
  :argument "--custom="
  :multi-value 'repeat
  :prompt "Custom key=value: ")

(transient-define-argument majutsu-gerrit-upload:--trace ()
  :description "Trace"
  :class 'transient-option
  :key "-X"
  :argument "--trace="
  :prompt "Trace: ")

;;;###autoload(autoload 'majutsu-gerrit-upload-transient "majutsu-gerrit" nil t)
(transient-define-prefix majutsu-gerrit-upload-transient ()
  "Transient for jj gerrit upload."
  :man-page "jj-gerrit-upload"
  :class 'majutsu-repository-transient-prefix
  :repo-namespace 'majutsu-gerrit
  :repo-key 'majutsu-gerrit-upload
  :repo-filter #'majutsu-gerrit-upload--repo-args
  :incompatible '(("--wip" "--ready")
                  ("--private" "--remove-private")
                  ("--publish-comments" "--no-publish-comments"))
  :transient-non-suffix t
  [:description
   "JJ Gerrit Upload"
   ["Selection"
    (majutsu-gerrit-upload:--revision)
    (majutsu-gerrit-upload:revision)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Target"
    (majutsu-gerrit-upload:--remote-branch)
    (majutsu-gerrit-upload:--remote)
    ("-n" "Dry run" ("-n" "--dry-run"))]
   ["Review"
    (majutsu-gerrit-upload:--reviewer)
    (majutsu-gerrit-upload:--cc)
    (majutsu-gerrit-upload:--label)
    (majutsu-gerrit-upload:--topic)
    (majutsu-gerrit-upload:--hashtag)
    (majutsu-gerrit-upload:--message)]
   ["State"
    ("-e" "Change edit" "--edit")
    ("-w" "WIP" "--wip")
    ("-a" "Ready" "--ready")
    ("-p" "Private" "--private")
    ("-P" "Remove private" "--remove-private")]
   ["Notify"
    (majutsu-gerrit-upload:--notify)
    ("-u" "Publish comments" "--publish-comments")
    ("-U" "No publish comments" "--no-publish-comments")
    ("-A" "Ignore attention" "--ignore-attention-set")]
   ["Advanced"
    ("-s" "Submit" "--submit")
    ("-S" "Skip validation" "--skip-validation")
    ("-M" "Merged" "--merged")
    (majutsu-gerrit-upload:--deadline)
    (majutsu-gerrit-upload:--custom)
    (majutsu-gerrit-upload:--trace)]
   ["Actions"
    ("u" "Upload" majutsu-gerrit-upload)
    ("RET" "Upload" majutsu-gerrit-upload)
    ("W" "Save repo defaults" majutsu-transient-save-repository-defaults
     :transient t)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (transient-setup
   'majutsu-gerrit-upload-transient nil nil
   :scope (majutsu-selection-session-begin)))

;;; _
(provide 'majutsu-gerrit)
;;; majutsu-gerrit.el ends here
