;;; majutsu-gerrit-dashboard.el --- Gerrit dashboard for Majutsu -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Section-based Gerrit dashboard backed by Majutsu's native REST/data layers.

;;; Code:

(require 'majutsu-base)
(require 'majutsu-gerrit)
(require 'majutsu-gerrit-data)
(require 'majutsu-gerrit-rest)
(require 'majutsu-mode)

(require 'cl-lib)
(require 'magit-section)
(require 'seq)
(require 'transient)
(require 'subr-x)
(require 'url-util)

(autoload 'majutsu-gerrit-download-change "majutsu-gerrit-download" nil t)

(declare-function majutsu-completing-read-payload "majutsu-base")
(declare-function majutsu-gerrit--project-from-remote-url "majutsu-gerrit")
(declare-function majutsu-gerrit--remote-url "majutsu-gerrit")
(declare-function majutsu-gerrit--selected-remote "majutsu-gerrit")
(declare-function majutsu-gerrit-read-account "majutsu-gerrit")
(declare-function majutsu-gerrit-topic-candidate-data "majutsu-gerrit")
(declare-function majutsu-gerrit--remote-branch-candidate-data "majutsu-gerrit")
(declare-function majutsu-transient-read-remote-name "majutsu-remote")

(defgroup majutsu-gerrit-dashboard nil
  "Gerrit dashboard for Majutsu."
  :group 'majutsu-gerrit)

(defcustom majutsu-gerrit-dashboard-queries
  '(("Outgoing reviews" . "is:open owner:self")
    ("Incoming reviews" . "is:open reviewer:self -owner:self"))
  "Named Gerrit queries shown by `majutsu-gerrit-dashboard'."
  :type '(repeat (cons (string :tag "Title") (string :tag "Query")))
  :group 'majutsu-gerrit-dashboard)

(defcustom majutsu-gerrit-dashboard-limit 50
  "Maximum number of changes fetched for each dashboard query."
  :type 'integer
  :group 'majutsu-gerrit-dashboard)

(defcustom majutsu-gerrit-dashboard-options
  '("LABELS" "DETAILED_ACCOUNTS")
  "ListChangesOption values requested for dashboard queries."
  :type '(repeat string)
  :group 'majutsu-gerrit-dashboard)

(defclass majutsu-gerrit-dashboard-section (magit-section) ())

(defclass majutsu-gerrit-query-section (magit-section) ())

(defclass majutsu-gerrit-change-section (magit-section)
  ((keymap :initform 'majutsu-gerrit-change-section-map)))

(setf (alist-get 'majutsu-gerrit-dashboard magit--section-type-alist)
      'majutsu-gerrit-dashboard-section)
(setf (alist-get 'majutsu-gerrit-query magit--section-type-alist)
      'majutsu-gerrit-query-section)
(setf (alist-get 'majutsu-gerrit-change magit--section-type-alist)
      'majutsu-gerrit-change-section)

(defvar majutsu-gerrit-dashboard-query-history nil
  "Minibuffer history for ad-hoc Gerrit dashboard queries.")

(defvar majutsu-gerrit-dashboard-section-history nil
  "Minibuffer history for custom Gerrit dashboard sections.")

(defvar majutsu-gerrit-dashboard-foreach-history nil
  "Minibuffer history for dashboard-wide Gerrit query suffixes.")

(defvar majutsu-gerrit-dashboard-project-history nil
  "Minibuffer history for Gerrit dashboard project filters.")

(defvar majutsu-gerrit-dashboard-branch-history nil
  "Minibuffer history for Gerrit dashboard branch filters.")

(defvar majutsu-gerrit-dashboard-owner-history nil
  "Minibuffer history for Gerrit dashboard owner filters.")

(defvar majutsu-gerrit-dashboard-reviewer-history nil
  "Minibuffer history for Gerrit dashboard reviewer filters.")

(defvar majutsu-gerrit-dashboard-cc-history nil
  "Minibuffer history for Gerrit dashboard CC filters.")

(defvar majutsu-gerrit-dashboard-topic-history nil
  "Minibuffer history for Gerrit dashboard topic filters.")

(defvar majutsu-gerrit-dashboard-title-history nil
  "Minibuffer history for Gerrit dashboard titles.")

(defconst majutsu-gerrit-dashboard--default-preset
  '(("Has draft comments" . "has:draft")
    ("Your turn" . "attention:self")
    ("Work in progress" . "is:open owner:self is:wip")
    ("Outgoing reviews" . "is:open owner:self -is:wip")
    ("Incoming reviews" . "is:open -owner:self -is:wip reviewer:self")
    ("CCed on" . "is:open -is:wip cc:self")
    ("Recently closed" . "is:closed (-is:wip OR owner:self) (owner:self OR reviewer:self OR cc:self) -age:4w"))
  "Polygerrit-style default user dashboard queries.")

(defconst majutsu-gerrit-dashboard--preset-alist
  `(("minimal" . ,majutsu-gerrit-dashboard-queries)
    ("default" . ,majutsu-gerrit-dashboard--default-preset)
    ("outgoing" . (("Outgoing reviews" . "is:open owner:self -is:wip")))
    ("incoming" . (("Incoming reviews" . "is:open -owner:self -is:wip reviewer:self")))
    ("your-turn" . (("Your turn" . "attention:self")))
    ("wip" . (("Work in progress" . "is:open owner:self is:wip")))
    ("cced" . (("CCed on" . "is:open -is:wip cc:self")))
    ("closed" . (("Recently closed" . "is:closed (-is:wip OR owner:self) (owner:self OR reviewer:self OR cc:self) -age:4w"))))
  "Named dashboard query presets.")

(defun majutsu-gerrit-dashboard--query-specs (queries)
  "Normalize dashboard QUERIES to a list of (TITLE . QUERY) cells."
  (mapcar (lambda (query)
            (cond
             ((and (consp query) (stringp (car query)) (stringp (cdr query)))
              query)
             ((stringp query)
              (cons query query))
             (t
              (error "Invalid Gerrit dashboard query: %S" query))))
          (or queries majutsu-gerrit-dashboard-queries)))

(defun majutsu-gerrit-dashboard--arg-values (prefix args)
  "Return every value in ARGS whose transient argument starts with PREFIX."
  (seq-keep (lambda (arg) (transient-arg-value prefix (list arg))) args))

(defun majutsu-gerrit-dashboard--number-arg (prefix args default)
  "Return numeric transient value for PREFIX in ARGS, falling back to DEFAULT."
  (if-let* ((value (transient-arg-value prefix args)))
      (string-to-number value)
    default))

(defun majutsu-gerrit-dashboard--normalize-token (token)
  "Return Gerrit query TOKEN quoted when it contains whitespace."
  (if (string-match-p "[[:space:]]" token)
      (format "\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" token))
    token))

(defun majutsu-gerrit-dashboard--join-query (&rest parts)
  "Join non-empty query PARTS with spaces."
  (string-join (seq-filter (lambda (part)
                             (and (stringp part)
                                  (not (string-empty-p part))))
                           parts)
               " "))

(defun majutsu-gerrit-dashboard--query-filter (name value)
  "Return Gerrit query filter NAME:VALUE."
  (when (and (stringp value) (not (string-empty-p value)))
    (format "%s:%s" name (majutsu-gerrit-dashboard--normalize-token value))))

(defun majutsu-gerrit-dashboard--wip-filter (value)
  "Return Gerrit WIP filter for transient VALUE."
  (pcase value
    ("only" "is:wip")
    ("exclude" "-is:wip")
    (_ nil)))

(defun majutsu-gerrit-dashboard--global-filter (args)
  "Return the dashboard-wide Gerrit filter represented by ARGS."
  (majutsu-gerrit-dashboard--join-query
   (transient-arg-value "--foreach=" args)
   (majutsu-gerrit-dashboard--query-filter
    "project" (transient-arg-value "--project=" args))
   (majutsu-gerrit-dashboard--query-filter
    "branch" (transient-arg-value "--branch=" args))
   (majutsu-gerrit-dashboard--query-filter
    "owner" (transient-arg-value "--owner=" args))
   (majutsu-gerrit-dashboard--query-filter
    "reviewer" (transient-arg-value "--reviewer=" args))
   (majutsu-gerrit-dashboard--query-filter
    "cc" (transient-arg-value "--cc=" args))
   (majutsu-gerrit-dashboard--query-filter
    "topic" (transient-arg-value "--topic=" args))
   (majutsu-gerrit-dashboard--wip-filter
    (transient-arg-value "--wip=" args))))

(defun majutsu-gerrit-dashboard--append-filter (query filter)
  "Return QUERY with dashboard-wide FILTER appended."
  (majutsu-gerrit-dashboard--join-query query filter))

(defun majutsu-gerrit-dashboard--parse-section (section)
  "Parse SECTION in the form TITLE:QUERY into a query spec."
  (unless (and (stringp section)
               (string-match "\\`\\([^:]+\\):\\(.+\\)\\'" section))
    (user-error "Section must have form TITLE:QUERY"))
  (cons (string-trim (match-string 1 section))
        (string-trim (match-string 2 section))))

(defun majutsu-gerrit-dashboard--preset-query-specs (preset)
  "Return dashboard query specs for PRESET."
  (if (or (null preset) (equal preset "minimal"))
      (copy-tree majutsu-gerrit-dashboard-queries)
    (copy-tree (or (cdr (assoc preset majutsu-gerrit-dashboard--preset-alist))
                   majutsu-gerrit-dashboard-queries))))

(defun majutsu-gerrit-dashboard--queries-from-args (args)
  "Build dashboard query specs from transient ARGS."
  (let* ((filter (majutsu-gerrit-dashboard--global-filter args))
         (custom (transient-arg-value "--custom-query=" args))
         (sections (majutsu-gerrit-dashboard--arg-values "--section=" args))
         (queries (cond
                   (custom (list (cons "Query" custom)))
                   (sections (mapcar #'majutsu-gerrit-dashboard--parse-section
                                     sections))
                   (t (majutsu-gerrit-dashboard--preset-query-specs
                       (transient-arg-value "--preset=" args))))))
    (mapcar (lambda (query-spec)
              (cons (car query-spec)
                    (majutsu-gerrit-dashboard--append-filter
                     (cdr query-spec) filter)))
            queries)))

(defun majutsu-gerrit-dashboard--repo-args (args)
  "Keep stable dashboard ARGS for repository-local defaults."
  (seq-filter (lambda (arg)
                (or (transient-arg-value "--remote=" (list arg))
                    (transient-arg-value "--preset=" (list arg))
                    (transient-arg-value "--project=" (list arg))
                    (transient-arg-value "--branch=" (list arg))
                    (transient-arg-value "--limit=" (list arg))
                    (transient-arg-value "--option=" (list arg))
                    (member arg '("--hide-empty"))))
              args))

(defun majutsu-gerrit-dashboard--default-value ()
  "Return dashboard transient default arguments."
  (majutsu-transient-default-value
   'majutsu-gerrit 'majutsu-gerrit-dashboard
   'majutsu-gerrit-dashboard-current-arguments
   'majutsu-gerrit-dashboard-default-arguments))

(defun majutsu-gerrit-dashboard--set-value (args &optional save)
  "Set current dashboard ARGS.
When SAVE is non-nil, also persist them globally."
  (setq args (seq-remove #'null (copy-sequence args)))
  (if-let* ((config-id (majutsu-repository-config-id)))
      (majutsu-transient-put-repository-current-value
       'majutsu-gerrit 'majutsu-gerrit-dashboard args config-id)
    (put 'majutsu-gerrit-dashboard
         'majutsu-gerrit-dashboard-current-arguments args))
  (when save
    (setf (alist-get
           (majutsu-transient-global-default-key
            'majutsu-gerrit 'majutsu-gerrit-dashboard)
           transient-values)
          args)
    (transient-save-values))
  nil)

(defun majutsu-gerrit-dashboard--normalize-query-response (queries response)
  "Return RESPONSE as one result list per query in QUERIES."
  (if (= (length queries) 1)
      (list response)
    response))

(defun majutsu-gerrit-dashboard--fetch
    (queries &optional remote directory limit start options)
  "Fetch dashboard QUERIES for REMOTE in DIRECTORY.
LIMIT, START and OPTIONS are passed to Gerrit's change query endpoint.
Return a list of (QUERY-SPEC . CHANGES), where CHANGES are
`majutsu-gerrit-change' objects."
  (let* ((query-specs (majutsu-gerrit-dashboard--query-specs queries))
         (query-strings (mapcar #'cdr query-specs))
         (spec (majutsu-gerrit-rest-current-spec remote directory))
         (raw (majutsu-gerrit-rest-change-query
               query-strings
               (or limit majutsu-gerrit-dashboard-limit)
               start
               (or options majutsu-gerrit-dashboard-options)
               spec))
         (groups (majutsu-gerrit-dashboard--normalize-query-response
                  query-specs raw)))
    (cl-mapcar (lambda (query-spec changes)
                 (cons query-spec
                       (mapcar #'majutsu-gerrit-change-from-alist changes)))
               query-specs groups)))

(defun majutsu-gerrit-dashboard--status-face (status)
  "Return a face for Gerrit change STATUS."
  (pcase status
    ("NEW" 'success)
    ("MERGED" 'magit-dimmed)
    ("ABANDONED" 'error)
    (_ 'shadow)))

(defun majutsu-gerrit-dashboard--account-display (account)
  "Return a short display string for ACCOUNT."
  (or (and account (majutsu-gerrit-account-name account))
      (and account (majutsu-gerrit-account-username account))
      (and account (majutsu-gerrit-account-email account))
      "?"))

(defun majutsu-gerrit-dashboard--comment-summary (change)
  "Return compact comment summary for CHANGE."
  (let ((total (majutsu-gerrit-change-total-comment-count change))
        (unresolved (majutsu-gerrit-change-unresolved-comment-count change)))
    (cond
     ((and unresolved (> unresolved 0))
      (format " %d unresolved" unresolved))
     ((and total (> total 0))
      (format " %d comments" total))
     (t ""))))

(defun majutsu-gerrit-dashboard--topic-summary (change)
  "Return compact topic summary for CHANGE."
  (if-let* ((topic (majutsu-gerrit-change-topic change)))
      (format " #%s" topic)
    ""))

(defun majutsu-gerrit-dashboard--change-web-url (change &optional spec)
  "Return a Polygerrit web URL for CHANGE using SPEC."
  (let* ((spec (majutsu-gerrit-rest-normalize-spec spec))
         (scheme (plist-get spec :scheme))
         (host (plist-get spec :host))
         (path (replace-regexp-in-string
                "/+\\'" "" (or (plist-get spec :path) "")))
         (project (majutsu-gerrit-change-project change))
         (number (majutsu-gerrit-change-number change)))
    (when (and scheme host project number)
      (format "%s://%s%s/c/%s/+/%s"
              scheme host path
              (majutsu-gerrit-rest-encode-id project)
              number))))

(defun majutsu-gerrit-dashboard--insert-change (change)
  "Insert one dashboard row for CHANGE."
  (magit-insert-section (majutsu-gerrit-change change)
    (let ((number (or (majutsu-gerrit-change-number change) "?"))
          (status (or (majutsu-gerrit-change-status change) "?"))
          (owner (majutsu-gerrit-dashboard--account-display
                  (majutsu-gerrit-change-owner change))))
      (insert (propertize (format "%-6s" number) 'face 'magit-hash))
      (insert " ")
      (insert (propertize (format "%-9s" status)
                          'face (majutsu-gerrit-dashboard--status-face status)))
      (insert " ")
      (insert (or (majutsu-gerrit-change-subject change) ""))
      (insert (propertize (format " [%s]" owner) 'face 'shadow))
      (insert (propertize (majutsu-gerrit-dashboard--topic-summary change)
                          'face 'font-lock-keyword-face))
      (insert (propertize (majutsu-gerrit-dashboard--comment-summary change)
                          'face 'warning))
      (insert "\n"))))

(defun majutsu-gerrit-dashboard--more-changes-p (changes)
  "Return non-nil if CHANGES were truncated by Gerrit."
  (and changes
       (majutsu-gerrit-change-more-changes-p (car (last changes)))))

(defun majutsu-gerrit-dashboard--insert-query (query-spec changes)
  "Insert one QUERY-SPEC section containing CHANGES."
  (magit-insert-section (majutsu-gerrit-query query-spec)
    (magit-insert-heading
      (format "%s (%d%s)\n" (car query-spec) (length changes)
              (if (majutsu-gerrit-dashboard--more-changes-p changes) "+" "")))
    (if changes
        (dolist (change changes)
          (majutsu-gerrit-dashboard--insert-change change))
      (insert (propertize "No changes.\n" 'face 'shadow)))
    (insert "\n")))

(defun majutsu-gerrit-dashboard--state-from-args (args root)
  "Return dashboard state plist from transient ARGS in ROOT."
  (let ((remote (majutsu-gerrit-dashboard--current-remote args root)))
    (list :args args
          :remote remote
          :queries (majutsu-gerrit-dashboard--queries-from-args args)
          :limit (majutsu-gerrit-dashboard--number-arg
                  "--limit=" args majutsu-gerrit-dashboard-limit)
          :start (majutsu-gerrit-dashboard--number-arg "--start=" args nil)
          :options (or (majutsu-gerrit-dashboard--arg-values "--option=" args)
                       majutsu-gerrit-dashboard-options)
          :hide-empty (member "--hide-empty" args)
          :title (transient-arg-value "--title=" args))))

(defun majutsu-gerrit-dashboard--open (&optional directory)
  "Open a Gerrit dashboard in DIRECTORY using current transient value."
  (let ((default-directory (or directory default-directory)))
    (majutsu-with-toplevel
      (majutsu-setup-buffer #'majutsu-gerrit-dashboard-mode nil
        :buffer "*Majutsu Gerrit*"
        :directory default-directory))))

(defun majutsu-gerrit-dashboard--open-with-args (args &optional directory)
  "Open a Gerrit dashboard configured by transient ARGS."
  (majutsu-gerrit-dashboard--set-value args)
  (majutsu-gerrit-dashboard--open directory))

;;;###autoload
(defun majutsu-gerrit-dashboard (&optional query)
  "Show a Gerrit dashboard for the current repository.
With prefix argument, prompt for a single ad-hoc QUERY."
  (interactive
   (list (when current-prefix-arg
           (read-string "Gerrit query: " nil
                        'majutsu-gerrit-dashboard-query-history))))
  (if query
      (majutsu-gerrit-dashboard--open-with-args
       (list (concat "--custom-query=" query)))
    (majutsu-gerrit-dashboard--open)))

(defun majutsu-gerrit-dashboard--current-remote (args root)
  "Return the dashboard remote selected by ARGS in ROOT."
  (or (transient-arg-value "--remote=" args)
      (majutsu-gerrit--selected-remote args root)))

(defun majutsu-gerrit-dashboard-arguments ()
  "Return current dashboard transient arguments."
  (transient-args 'majutsu-gerrit-dashboard-transient))

;;;###autoload(autoload 'majutsu-gerrit-dashboard-open "majutsu-gerrit-dashboard" nil t)
(transient-define-suffix majutsu-gerrit-dashboard-open (args)
  "Open a Gerrit dashboard configured by transient ARGS."
  :class 'majutsu-transient-default-action-suffix
  (interactive (list (majutsu-gerrit-dashboard-arguments)))
  (majutsu-gerrit-dashboard--open-with-args args))

(defun majutsu-gerrit-dashboard-browse-change ()
  "Open the Gerrit change at point in a browser."
  (interactive)
  (let* ((change (magit-section-value-if 'majutsu-gerrit-change))
         (state (majutsu-gerrit-dashboard--state-from-args
                 (majutsu-gerrit-dashboard-arguments)
                 majutsu--default-directory))
         (spec (majutsu-gerrit-rest-current-spec
                (plist-get state :remote) majutsu--default-directory))
         (url (and change (majutsu-gerrit-dashboard--change-web-url change spec))))
    (unless url
      (user-error "No Gerrit change at point"))
    (browse-url url)))

;;; Transient

(defclass majutsu-gerrit-dashboard-prefix (transient-prefix)
  ((history-key :initform 'majutsu-gerrit-dashboard))
  "Prefix class for Gerrit dashboard refresh options.")

(cl-defmethod transient-init-value ((obj majutsu-gerrit-dashboard-prefix))
  (oset obj value (copy-sequence (majutsu-gerrit-dashboard--default-value))))

(cl-defmethod transient-set-value ((obj majutsu-gerrit-dashboard-prefix))
  (let* ((obj (oref obj prototype))
         (args (transient-args (oref obj command))))
    (majutsu-gerrit-dashboard--set-value args)
    (transient--history-push obj)
    (majutsu-gerrit-dashboard--open)))

(cl-defmethod transient-save-value ((obj majutsu-gerrit-dashboard-prefix))
  (let* ((obj (oref obj prototype))
         (args (transient-args (oref obj command))))
    (majutsu-gerrit-dashboard--set-value args t)
    (transient--history-push obj)
    (majutsu-gerrit-dashboard--open)))

(cl-defmethod majutsu-transient--save-repository-defaults
  ((obj majutsu-gerrit-dashboard-prefix))
  (let* ((obj (oref obj prototype))
         (args (transient-args (oref obj command))))
    (majutsu-transient-save-repository-value
     'majutsu-gerrit 'majutsu-gerrit-dashboard
     (majutsu-gerrit-dashboard--repo-args args))
    (transient--history-push obj)
    (majutsu-gerrit-dashboard--open)
    (message "Saved dashboard arguments as repository defaults")))

(defun majutsu-gerrit-dashboard--read-preset (prompt initial-input history)
  "Read a Gerrit dashboard preset with PROMPT."
  (completing-read prompt (mapcar #'car majutsu-gerrit-dashboard--preset-alist)
                   nil t initial-input history "minimal"))

(defun majutsu-gerrit-dashboard--read-section (prompt initial-input history)
  "Read a dashboard section as TITLE:QUERY."
  (majutsu-read-string prompt initial-input
                       (or history 'majutsu-gerrit-dashboard-section-history)))

(defun majutsu-gerrit-dashboard--read-project (prompt initial-input history)
  "Read a Gerrit project filter."
  (majutsu-with-toplevel
    (let* ((args (ignore-errors (transient-get-value)))
           (remote (or (and args (transient-arg-value "--remote=" args))
                       (majutsu-gerrit--selected-remote
                        args default-directory)))
           (project (majutsu-gerrit--project-from-remote-url
                     (majutsu-gerrit--remote-url remote default-directory))))
      (majutsu-read-string prompt initial-input
                           (or history 'majutsu-gerrit-dashboard-project-history)
                           project))))

(defun majutsu-gerrit-dashboard--read-branch (prompt initial-input history)
  "Read a Gerrit branch filter."
  (majutsu-with-toplevel
    (let* ((args (ignore-errors (transient-get-value)))
           (remote (and args (transient-arg-value "--remote=" args)))
           (payload (majutsu-gerrit--remote-branch-candidate-data remote)))
      (if payload
          (majutsu-completing-read-payload
           prompt payload nil nil initial-input
           (or history 'majutsu-gerrit-dashboard-branch-history)
           nil 'majutsu-gerrit-remote-branch)
        (majutsu-read-string
         prompt initial-input
         (or history 'majutsu-gerrit-dashboard-branch-history))))))

(defun majutsu-gerrit-dashboard--read-account (prompt initial-input history)
  "Read a Gerrit account filter."
  (majutsu-with-toplevel
    (let* ((args (ignore-errors (transient-get-value)))
           (remote (and args (transient-arg-value "--remote=" args))))
      (majutsu-gerrit-read-account prompt initial-input history remote))))

(defun majutsu-gerrit-dashboard--read-owner (prompt initial-input history)
  "Read a Gerrit owner filter."
  (majutsu-gerrit-dashboard--read-account
   prompt initial-input (or history 'majutsu-gerrit-dashboard-owner-history)))

(defun majutsu-gerrit-dashboard--read-reviewer (prompt initial-input history)
  "Read a Gerrit reviewer filter."
  (majutsu-gerrit-dashboard--read-account
   prompt initial-input (or history 'majutsu-gerrit-dashboard-reviewer-history)))

(defun majutsu-gerrit-dashboard--read-cc (prompt initial-input history)
  "Read a Gerrit CC filter."
  (majutsu-gerrit-dashboard--read-account
   prompt initial-input (or history 'majutsu-gerrit-dashboard-cc-history)))

(defun majutsu-gerrit-dashboard--read-topic (prompt initial-input history)
  "Read a Gerrit topic filter."
  (majutsu-with-toplevel
    (let ((payload (majutsu-gerrit-topic-candidate-data)))
      (if payload
          (majutsu-completing-read-payload
           prompt payload nil nil initial-input
           (or history 'majutsu-gerrit-dashboard-topic-history)
           nil 'majutsu-gerrit-topic)
        (majutsu-read-string
         prompt initial-input
         (or history 'majutsu-gerrit-dashboard-topic-history))))))

(transient-define-argument majutsu-gerrit-dashboard:--preset ()
  :description "Preset"
  :class 'transient-option
  :shortarg "-p"
  :argument "--preset="
  :reader #'majutsu-gerrit-dashboard--read-preset)

(transient-define-argument majutsu-gerrit-dashboard:--custom-query ()
  :description "Custom query"
  :class 'transient-option
  :shortarg "-q"
  :argument "--custom-query="
  :prompt "Gerrit query"
  :reader (lambda (prompt initial-input history)
            (majutsu-read-string
             prompt initial-input
             (or history 'majutsu-gerrit-dashboard-query-history))))

(transient-define-argument majutsu-gerrit-dashboard:--section ()
  :description "Section"
  :class 'transient-option
  :shortarg "-s"
  :argument "--section="
  :multi-value 'repeat
  :prompt "Section TITLE:QUERY"
  :reader #'majutsu-gerrit-dashboard--read-section)

(transient-define-argument majutsu-gerrit-dashboard:--foreach ()
  :description "Foreach"
  :class 'transient-option
  :shortarg "-F"
  :argument "--foreach="
  :prompt "Append to every query"
  :reader (lambda (prompt initial-input history)
            (majutsu-read-string
             prompt initial-input
             (or history 'majutsu-gerrit-dashboard-foreach-history))))

(transient-define-argument majutsu-gerrit-dashboard:--remote ()
  :description "Remote"
  :class 'transient-option
  :shortarg "-R"
  :argument "--remote="
  :prompt "Remote"
  :reader #'majutsu-transient-read-remote-name)

(transient-define-argument majutsu-gerrit-dashboard:--project ()
  :description "Project"
  :class 'transient-option
  :shortarg "-P"
  :argument "--project="
  :reader #'majutsu-gerrit-dashboard--read-project)

(transient-define-argument majutsu-gerrit-dashboard:--branch ()
  :description "Branch"
  :class 'transient-option
  :shortarg "-b"
  :argument "--branch="
  :reader #'majutsu-gerrit-dashboard--read-branch)

(transient-define-argument majutsu-gerrit-dashboard:--owner ()
  :description "Owner"
  :class 'transient-option
  :shortarg "-o"
  :argument "--owner="
  :reader #'majutsu-gerrit-dashboard--read-owner)

(transient-define-argument majutsu-gerrit-dashboard:--reviewer ()
  :description "Reviewer"
  :class 'transient-option
  :shortarg "-r"
  :argument "--reviewer="
  :reader #'majutsu-gerrit-dashboard--read-reviewer)

(transient-define-argument majutsu-gerrit-dashboard:--cc ()
  :description "CC"
  :class 'transient-option
  :shortarg "-c"
  :argument "--cc="
  :reader #'majutsu-gerrit-dashboard--read-cc)

(transient-define-argument majutsu-gerrit-dashboard:--topic ()
  :description "Topic"
  :class 'transient-option
  :shortarg "-t"
  :argument "--topic="
  :reader #'majutsu-gerrit-dashboard--read-topic)

(transient-define-argument majutsu-gerrit-dashboard:--wip ()
  :description "WIP"
  :class 'transient-option
  :shortarg "-w"
  :argument "--wip="
  :choices '("include" "exclude" "only")
  :prompt "WIP mode")

(transient-define-argument majutsu-gerrit-dashboard:--limit ()
  :description "Limit"
  :class 'transient-option
  :shortarg "-n"
  :argument "--limit="
  :reader #'transient-read-number-N+)

(transient-define-argument majutsu-gerrit-dashboard:--start ()
  :description "Start"
  :class 'transient-option
  :shortarg "-S"
  :argument "--start="
  :reader #'transient-read-number-N0)

(transient-define-argument majutsu-gerrit-dashboard:--option ()
  :description "REST option"
  :class 'transient-option
  :shortarg "-O"
  :argument "--option="
  :multi-value 'repeat
  :choices '("LABELS" "DETAILED_LABELS" "SUBMIT_REQUIREMENTS"
             "CURRENT_REVISION" "CURRENT_COMMIT" "CURRENT_FILES"
             "DETAILED_ACCOUNTS")
  :prompt "ListChangesOption")

(transient-define-argument majutsu-gerrit-dashboard:--title ()
  :description "Title"
  :class 'transient-option
  :shortarg "-T"
  :argument "--title="
  :reader (lambda (prompt initial-input history)
            (majutsu-read-string
             prompt initial-input
             (or history 'majutsu-gerrit-dashboard-title-history))))

(defun majutsu-gerrit-dashboard--transient-description ()
  "Return dashboard transient heading."
  (let ((args (transient-get-value)))
    (if args
        (format "Gerrit Dashboard (%s)" (string-join args " "))
      "Gerrit Dashboard")))

;;;###autoload(autoload 'majutsu-gerrit-dashboard-transient "majutsu-gerrit-dashboard" nil t)
(transient-define-prefix majutsu-gerrit-dashboard-transient ()
  "Transient interface for Gerrit dashboard options."
  :class 'majutsu-gerrit-dashboard-prefix
  :transient-non-suffix t
  [:description
   majutsu-gerrit-dashboard--transient-description
   ["Queries"
    (majutsu-gerrit-dashboard:--preset)
    (majutsu-gerrit-dashboard:--custom-query)
    (majutsu-gerrit-dashboard:--section)
    (majutsu-gerrit-dashboard:--foreach)]
   ["Filters"
    (majutsu-gerrit-dashboard:--remote)
    (majutsu-gerrit-dashboard:--project)
    (majutsu-gerrit-dashboard:--branch)
    (majutsu-gerrit-dashboard:--owner)
    (majutsu-gerrit-dashboard:--reviewer)
    (majutsu-gerrit-dashboard:--cc)
    (majutsu-gerrit-dashboard:--topic)
    (majutsu-gerrit-dashboard:--wip)]
   ["Fetch"
    (majutsu-gerrit-dashboard:--limit)
    (majutsu-gerrit-dashboard:--start)
    (majutsu-gerrit-dashboard:--option)
    (majutsu-gerrit-dashboard:--title)
    ("-H" "Hide empty" "--hide-empty")]
   ["Actions"
    ("g" "Dashboard" majutsu-gerrit-dashboard-open)
    ("s" "Dashboard and set defaults" transient-set-and-exit)
    ("w" "Dashboard and save defaults" transient-save-and-exit)
    ("W" "Dashboard and save repo defaults" majutsu-transient-save-repository-defaults
     :transient t)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (if (not (eq transient-current-command 'majutsu-gerrit-dashboard-transient))
      (transient-setup 'majutsu-gerrit-dashboard-transient)
    (let ((args (transient-args 'majutsu-gerrit-dashboard-transient)))
      (majutsu-gerrit-dashboard--set-value args)
      (majutsu-gerrit-dashboard--open))))

(defvar-keymap majutsu-gerrit-change-section-map
  :doc "Keymap for Gerrit change sections."
  "b"   #'majutsu-gerrit-dashboard-browse-change
  "d"   #'majutsu-gerrit-download-change
  "<remap> <majutsu-visit-thing>" #'majutsu-gerrit-dashboard-browse-change)

(defvar-keymap majutsu-gerrit-dashboard-mode-map
  :doc "Keymap for `majutsu-gerrit-dashboard-mode'."
  :parent majutsu-mode-map
  "b" #'majutsu-gerrit-dashboard-browse-change
  "d" #'majutsu-gerrit-download-change
  "R" #'majutsu-gerrit-dashboard-transient)

(define-derived-mode majutsu-gerrit-dashboard-mode majutsu-mode "Majutsu Gerrit"
  "Major mode for Gerrit dashboards."
  :group 'majutsu-gerrit-dashboard
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

(cl-defmethod majutsu-buffer-value (&context (major-mode majutsu-gerrit-dashboard-mode))
  "Return the dashboard query identity for the current buffer."
  (majutsu-gerrit-dashboard-arguments))

(defun majutsu-gerrit-dashboard-refresh-buffer ()
  "Refresh the Gerrit dashboard buffer."
  (majutsu--assert-mode 'majutsu-gerrit-dashboard-mode)
  (let* ((state (majutsu-gerrit-dashboard--state-from-args
                 (majutsu-gerrit-dashboard-arguments)
                 majutsu--default-directory))
         (groups (majutsu-gerrit-dashboard--fetch
                  (plist-get state :queries)
                  (plist-get state :remote)
                  majutsu--default-directory
                  (plist-get state :limit)
                  (plist-get state :start)
                  (plist-get state :options))))
    (magit-insert-section (majutsu-gerrit-dashboard)
      (magit-insert-heading
        (format "%s\n" (or (plist-get state :title) "Gerrit Dashboard")))
      (let ((inserted 0))
        (dolist (group groups)
          (unless (and (plist-get state :hide-empty)
                       (null (cdr group)))
            (cl-incf inserted)
            (majutsu-gerrit-dashboard--insert-query (car group) (cdr group))))
        (when (zerop inserted)
          (insert (propertize "No matching dashboard sections.\n" 'face 'shadow)))))))

(provide 'majutsu-gerrit-dashboard)

;;; majutsu-gerrit-dashboard.el ends here
