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

;;; Code:

(require 'majutsu)
(require 'majutsu-completion)
(require 'majutsu-config)
(require 'majutsu-gerrit-rest)
(require 'majutsu-remote)
(require 'majutsu-selection)

(require 'cl-lib)
(require 'consult)
(require 'json)
(require 'rx)
(require 'seq)
(require 'subr-x)
(require 'url-parse)

(declare-function majutsu-jj-lines "majutsu-jj")
(declare-function majutsu-transient-read-remote-name "majutsu-remote")
(autoload 'majutsu-gerrit-dashboard "majutsu-gerrit-dashboard" nil t)
(autoload 'majutsu-gerrit-dashboard-transient "majutsu-gerrit-dashboard" nil t)

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
      (propertize body 'face 'majutsu-completion-documentation))))

(defun majutsu-gerrit--account-annotate-candidate (candidate)
  "Return aligned annotation for CANDIDATE using its account text property."
  (when-let* ((account (get-text-property 0 'majutsu-gerrit-account candidate))
              (annotation (majutsu-gerrit--account-annotation account candidate)))
    (consult--annotate-align candidate annotation)))

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

(defun majutsu-gerrit--account-builder (remote)
  "Return a consult async builder for account completion under REMOTE."
  (let ((default-directory default-directory))
    (lambda (input)
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
               (args (cons "curl" (majutsu-gerrit-rest--curl-args
                                   spec "GET" url nil '("Accept: application/json")))))
          args)))))

(defun majutsu-gerrit-read-account (prompt initial-input history &optional remote)
  "Read one Gerrit account using asynchronous REST-backed completion."
  (let ((consult-async-split-style 'none))
    (consult--read
     (consult--process-collection
         (majutsu-gerrit--account-builder remote)
       :min-input 1
       :debounce 0.2
       :throttle 0.1
       :transform (consult--async-transform #'majutsu-gerrit--account-transform))
     :prompt prompt
     :initial initial-input
     :history history
     :require-match nil
     :category 'majutsu-gerrit-account
     :sort nil
     :annotate #'majutsu-gerrit--account-annotate-candidate)))

(defun majutsu-gerrit-read-accounts (prompt initial-input history &optional remote)
  "Read Gerrit accounts using asynchronous REST-backed completion.
The user can select multiple accounts in a loop; an empty input finishes."
  (let ((selected nil))
    (cl-block done
      (while t
        (let ((input (majutsu-gerrit-read-account
                      (if selected
                          (format "%s (%s selected, RET to finish): "
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
  (plist-get (majutsu-gerrit--remote-branch-candidate-data remote)
             :candidates))

(defun majutsu-gerrit-upload--read-revset (prompt initial-input _history)
  "Read revset for `jj gerrit upload --revision='."
  (majutsu-read-revset prompt initial-input '("gerrit" "upload" "-r")))

(defun majutsu-gerrit-upload--read-remote-branch (prompt initial-input _history)
  "Read target remote branch for `jj gerrit upload' with completion."
  (majutsu-with-toplevel
    (let ((remote (majutsu-gerrit-upload--transient-remote)))
      (majutsu-completing-read-payload
       prompt (majutsu-gerrit--remote-branch-candidate-data remote)
       nil nil initial-input 'majutsu-gerrit-remote-branch-history
       nil 'majutsu-gerrit-remote-branch))))

(defun majutsu-gerrit-upload--transient-remote ()
  "Return the Gerrit upload transient's selected remote, if any."
  (when-let* ((args (ignore-errors (transient-get-value))))
    (transient-arg-value "--remote=" args)))

(defun majutsu-gerrit-upload--read-reviewer (prompt initial-input history)
  "Read one or more Gerrit reviewers."
  (majutsu-with-toplevel
    (majutsu-gerrit-read-accounts
     prompt initial-input (or history 'majutsu-gerrit-reviewer-history)
     (majutsu-gerrit-upload--transient-remote))))

(defun majutsu-gerrit-upload--read-cc (prompt initial-input history)
  "Read one or more Gerrit CC values."
  (majutsu-with-toplevel
    (majutsu-gerrit-read-accounts
     prompt initial-input (or history 'majutsu-gerrit-cc-history)
     (majutsu-gerrit-upload--transient-remote))))

(defun majutsu-gerrit-upload--read-topic (prompt initial-input history)
  "Read a Gerrit topic."
  (majutsu-with-toplevel
    (let* ((remote (majutsu-gerrit-upload--transient-remote))
           (payload (majutsu-gerrit-topic-candidate-data remote)))
      (if payload
          (majutsu-completing-read-payload
           prompt payload nil nil initial-input
           (or history 'majutsu-gerrit-topic-history)
           nil 'majutsu-gerrit-topic nil default-directory)
        (majutsu-read-string prompt initial-input
                             (or history 'majutsu-gerrit-topic-history))))))

(defun majutsu-gerrit-upload--read-hashtag (prompt initial-input history)
  "Read one or more Gerrit hashtags."
  (majutsu-completing-read-multiple
   prompt '() nil nil initial-input
   (or history 'majutsu-gerrit-hashtag-history)
   nil 'majutsu-gerrit-hashtag))

(defun majutsu-gerrit-upload--read-label (prompt initial-input history)
  "Read one or more Gerrit labels."
  (majutsu-completing-read-multiple
   prompt '() nil nil initial-input
   (or history 'majutsu-gerrit-label-history)
   nil 'majutsu-gerrit-label))

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
