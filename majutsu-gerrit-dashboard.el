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

(autoload 'majutsu-gerrit-download-change "majutsu-gerrit-download" nil t)

(declare-function majutsu-gerrit--project-from-remote-url "majutsu-gerrit")
(declare-function majutsu-gerrit--remote-url "majutsu-gerrit")
(declare-function majutsu-gerrit--selected-remote "majutsu-gerrit")
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

(defvar majutsu-gerrit-dashboard-query-history nil
  "Minibuffer history for ad-hoc Gerrit dashboard queries.")

(defvar majutsu-gerrit-dashboard-section-history nil
  "Minibuffer history for custom Gerrit dashboard sections.")

(defvar majutsu-gerrit-dashboard-foreach-history nil
  "Minibuffer history for dashboard-wide Gerrit query suffixes.")

(defvar majutsu-gerrit-dashboard-title-history nil
  "Minibuffer history for Gerrit dashboard titles.")

(defvar majutsu-gerrit-query-history nil
  "Minibuffer history for Gerrit query strings.")

(put 'majutsu-gerrit-dashboard
     'majutsu-gerrit-dashboard-default-arguments
     '("--preset=default"))

(cl-defstruct (majutsu-gerrit-dashboard-state
               (:constructor majutsu-gerrit-dashboard-state-create))
  root remote title sections limit start options hide-empty dirty-p next-id)

(cl-defstruct (majutsu-gerrit-dashboard-query
               (:constructor majutsu-gerrit-dashboard-query-create))
  id title query)

(defvar-local majutsu-gerrit-dashboard--state nil
  "State object for the current Gerrit dashboard buffer.")

(defvar majutsu-gerrit-query--remote nil
  "Dynamic Gerrit remote used while completing a query.")

(defconst majutsu-gerrit-query--operators
  '("age:" "attention:" "branch:" "cc:" "change:" "comment:"
    "file:" "has:" "is:" "label:" "limit:" "message:" "owner:"
    "project:" "reviewer:" "status:" "topic:" "tr:" "before:"
    "after:")
  "Gerrit query operators offered by query completion.")

(defconst majutsu-gerrit-query--value-alist
  '(("has" . ("draft" "edit" "hashtag" "star" "unresolved"))
    ("is" . ("abandoned" "closed" "ignored" "merged" "open" "owner"
             "reviewer" "starred" "watched" "wip"))
    ("status" . ("abandoned" "closed" "merged" "new" "open"))
    ("label" . ("Code-Review" "Verified")))
  "Static Gerrit query operator value completions.")

(defun majutsu-gerrit-query--token-bounds ()
  "Return bounds of the Gerrit query token at point."
  (let ((end (point)))
    (save-excursion
      (skip-chars-backward "^ \t\n\r()")
      (cons (point) end))))

(defun majutsu-gerrit-query--operator-at (token)
  "Return Gerrit query operator in TOKEN, or nil."
  (when (string-match "\\`-?\\([^:[:space:]]+\\):" token)
    (match-string 1 token)))

(defun majutsu-gerrit-query--account-candidates (prefix)
  "Return Gerrit account candidates for PREFIX."
  (let ((self (and (string-prefix-p prefix "self") (list "self"))))
    (if (string-empty-p prefix)
        (or self '("self"))
      (append
       self
       (condition-case nil
           (let* ((spec (majutsu-gerrit-rest-current-spec
                         majutsu-gerrit-query--remote))
                  (accounts
                   (pcase majutsu-gerrit-account-completion-strategy
                     ('query
                      (majutsu-gerrit-rest-account-query
                       prefix majutsu-gerrit-account-suggestion-limit
                       nil '(DETAILS) spec))
                     (_
                      (majutsu-gerrit-rest-account-suggest
                       prefix majutsu-gerrit-account-suggestion-limit spec)))))
             (delq nil
                   (mapcar (lambda (account)
                             (or (alist-get 'email account)
                                 (alist-get 'username account)
                                 (when-let* ((id (alist-get '_account_id account)))
                                   (format "%s" id))))
                           accounts)))
         (error nil))))))

(defun majutsu-gerrit-query--project-candidates ()
  "Return project candidates for Gerrit query completion."
  (condition-case nil
      (let* ((remote (or majutsu-gerrit-query--remote
                         (majutsu-gerrit--selected-remote)))
             (project (majutsu-gerrit--project-from-remote-url
                       (majutsu-gerrit--remote-url remote))))
        (and project (list project)))
    (error nil)))

(defun majutsu-gerrit-query--payload-candidates (payload)
  "Return completion candidates from PAYLOAD."
  (plist-get payload :candidates))

(defun majutsu-gerrit-query--dynamic-table (function)
  "Return a dynamic completion table backed by FUNCTION."
  (completion-table-dynamic
   (lambda (string)
     (or (while-no-input (funcall function string))
         nil))))

(defun majutsu-gerrit-query--value-table (operator)
  "Return completion table for OPERATOR's value."
  (pcase operator
    ((or "attention" "cc" "owner" "reviewer")
     (majutsu-gerrit-query--dynamic-table
      #'majutsu-gerrit-query--account-candidates))
    ("branch"
     (condition-case nil
         (majutsu-gerrit-query--payload-candidates
          (majutsu-gerrit--remote-branch-candidate-data
           majutsu-gerrit-query--remote))
       (error nil)))
    ("project" (majutsu-gerrit-query--project-candidates))
    ("topic"
     (condition-case nil
         (majutsu-gerrit-query--payload-candidates
          (majutsu-gerrit-topic-candidate-data majutsu-gerrit-query--remote))
       (error nil)))
    (_ (cdr (assoc operator majutsu-gerrit-query--value-alist)))))

(defun majutsu-gerrit-query-completion-at-point ()
  "Return completion data for Gerrit query text at point."
  (pcase-let* ((`(,start . ,end) (majutsu-gerrit-query--token-bounds))
               (token (buffer-substring-no-properties start end))
               (operator (majutsu-gerrit-query--operator-at token)))
    (if operator
        (let* ((colon (string-search ":" token))
               (value-start (+ start colon 1))
               (table (majutsu-gerrit-query--value-table operator)))
          (and table
               (list value-start end table
                     :exclusive 'no
                     :category 'majutsu-gerrit-query)))
      (let ((operator-start (if (string-prefix-p "-" token) (1+ start) start)))
        (list operator-start end majutsu-gerrit-query--operators
              :exclusive 'no
              :category 'majutsu-gerrit-query)))))

(defun majutsu-gerrit-query-complete ()
  "Complete the Gerrit query at point."
  (interactive)
  (unless (completion-at-point)
    (minibuffer-message "No Gerrit query completions")))

(defvar majutsu-gerrit-query-read-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'majutsu-gerrit-query-complete)
    (define-key map (kbd "<tab>") #'majutsu-gerrit-query-complete)
    (define-key map [remap completion-at-point]
                #'majutsu-gerrit-query-complete)
    (make-composed-keymap map minibuffer-local-map))
  "Keymap used while reading Gerrit query strings.")

(defun majutsu-gerrit-query--minibuffer-setup ()
  "Install Gerrit query completion in the current minibuffer."
  (setq-local completion-at-point-functions
              '(majutsu-gerrit-query-completion-at-point)))

(defun majutsu-gerrit-read-query (prompt &optional initial-input history default remote)
  "Read a Gerrit query string with completion."
  (majutsu-with-toplevel
    (let ((majutsu-gerrit-query--remote remote))
      (let ((value (minibuffer-with-setup-hook
                       #'majutsu-gerrit-query--minibuffer-setup
                     (read-from-minibuffer
                      (format-prompt prompt default)
                      initial-input
                      majutsu-gerrit-query-read-map
                      nil
                      (or history 'majutsu-gerrit-query-history)
                      default))))
        (if (string-empty-p value)
            (or default (user-error "Need non-empty Gerrit query"))
          value)))))

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

(defun majutsu-gerrit-dashboard--query-title (query)
  "Return dashboard QUERY's title."
  (if (majutsu-gerrit-dashboard-query-p query)
      (majutsu-gerrit-dashboard-query-title query)
    (car query)))

(defun majutsu-gerrit-dashboard--query-string (query)
  "Return dashboard QUERY's Gerrit query string."
  (if (majutsu-gerrit-dashboard-query-p query)
      (majutsu-gerrit-dashboard-query-query query)
    (cdr query)))

(defun majutsu-gerrit-dashboard--query-specs (queries)
  "Normalize dashboard QUERIES to a list of query objects."
  (mapcar (lambda (query)
            (cond
             ((majutsu-gerrit-dashboard-query-p query)
              query)
             ((and (consp query) (stringp (car query)) (stringp (cdr query)))
              (majutsu-gerrit-dashboard-query-create
               :title (car query)
               :query (cdr query)))
             ((stringp query)
              (majutsu-gerrit-dashboard-query-create
               :title query
               :query query))
             (t
              (error "Invalid Gerrit dashboard query: %S" query))))
          (or queries majutsu-gerrit-dashboard-queries)))

(defun majutsu-gerrit-dashboard--assign-query-ids (queries &optional next-id)
  "Return (QUERIES . NEXT-ID) with stable ids assigned to QUERIES."
  (let ((next-id (or next-id 1))
        result)
    (dolist (query (majutsu-gerrit-dashboard--query-specs queries))
      (unless (majutsu-gerrit-dashboard-query-id query)
        (setf (majutsu-gerrit-dashboard-query-id query) next-id)
        (cl-incf next-id))
      (push query result))
    (cons (nreverse result) next-id)))

(defun majutsu-gerrit-dashboard--arg-values (prefix args)
  "Return every value in ARGS whose transient argument starts with PREFIX."
  (seq-keep (lambda (arg) (transient-arg-value prefix (list arg))) args))

(defun majutsu-gerrit-dashboard--number-arg (prefix args default)
  "Return numeric transient value for PREFIX in ARGS, falling back to DEFAULT."
  (if-let* ((value (transient-arg-value prefix args)))
      (string-to-number value)
    default))

(defun majutsu-gerrit-dashboard--join-query (&rest parts)
  "Join non-empty query PARTS with spaces."
  (string-join (seq-filter (lambda (part)
                             (and (stringp part)
                                  (not (string-empty-p part))))
                           parts)
               " "))

(defun majutsu-gerrit-dashboard--global-filter (args)
  "Return the dashboard-wide Gerrit filter represented by ARGS."
  (transient-arg-value "--foreach=" args))

(defun majutsu-gerrit-dashboard--append-filter (query filter)
  "Return QUERY with dashboard-wide FILTER appended."
  (majutsu-gerrit-dashboard--join-query query filter))

(defun majutsu-gerrit-dashboard--preset-query-specs (preset)
  "Return dashboard query specs for PRESET."
  (cond ((equal preset "minimal")
         (copy-tree majutsu-gerrit-dashboard-queries))
        ((null preset)
         (copy-tree majutsu-gerrit-dashboard--default-preset))
        (t
         (copy-tree (or (cdr (assoc preset majutsu-gerrit-dashboard--preset-alist))
                        majutsu-gerrit-dashboard--default-preset)))))

(defun majutsu-gerrit-dashboard--queries-from-args (args)
  "Build dashboard query specs from transient ARGS."
  (let* ((filter (majutsu-gerrit-dashboard--global-filter args))
         (custom (transient-arg-value "--custom-query=" args))
         (queries (if custom
                      (list (cons "Query" custom))
                    (majutsu-gerrit-dashboard--preset-query-specs
                     (transient-arg-value "--preset=" args)))))
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
                    (transient-arg-value "--custom-query=" (list arg))
                    (transient-arg-value "--foreach=" (list arg))
                    (transient-arg-value "--limit=" (list arg))
                    (transient-arg-value "--start=" (list arg))
                    (transient-arg-value "--option=" (list arg))
                    (transient-arg-value "--title=" (list arg))
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
         (query-strings (mapcar #'majutsu-gerrit-dashboard--query-string query-specs))
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

(defun majutsu-gerrit-dashboard--face (text face)
  "Return TEXT propertized with FACE for Magit section overlays."
  (propertize text 'face face 'font-lock-face face))

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
  (magit-insert-section (gerrit-change change)
    (let ((number (or (majutsu-gerrit-change-number change) "?"))
          (status (or (majutsu-gerrit-change-status change) "?"))
          (owner (majutsu-gerrit-dashboard--account-display
                  (majutsu-gerrit-change-owner change))))
      (insert (majutsu-gerrit-dashboard--face
               (format "%-6s" number) 'magit-hash))
      (insert " ")
      (insert (majutsu-gerrit-dashboard--face
               (format "%-9s" status)
               (majutsu-gerrit-dashboard--status-face status)))
      (insert " ")
      (insert (or (majutsu-gerrit-change-subject change) ""))
      (insert (majutsu-gerrit-dashboard--face (format " [%s]" owner) 'shadow))
      (insert (majutsu-gerrit-dashboard--face
               (majutsu-gerrit-dashboard--topic-summary change)
               'font-lock-keyword-face))
      (insert (majutsu-gerrit-dashboard--face
               (majutsu-gerrit-dashboard--comment-summary change)
               'warning))
      (insert "\n"))))

(defun majutsu-gerrit-dashboard--more-changes-p (changes)
  "Return non-nil if CHANGES were truncated by Gerrit."
  (and changes
       (majutsu-gerrit-change-more-changes-p (car (last changes)))))

(defun majutsu-gerrit-dashboard--insert-query (query changes)
  "Insert one QUERY section containing CHANGES."
  (magit-insert-section (gerrit-query
                         (majutsu-gerrit-dashboard-query-id query))
    (magit-insert-heading
      (insert (format "%s (%d%s)"
                      (majutsu-gerrit-dashboard--query-title query)
                      (length changes)
                      (if (majutsu-gerrit-dashboard--more-changes-p changes) "+" "")))
      (insert (majutsu-gerrit-dashboard--face
               (format "  %s" (majutsu-gerrit-dashboard--query-string query))
               'shadow))
      (insert "\n"))
    (if changes
        (dolist (change changes)
          (majutsu-gerrit-dashboard--insert-change change))
      (insert (majutsu-gerrit-dashboard--face "No changes.\n" 'shadow)))
    (insert "\n")))

(defun majutsu-gerrit-dashboard--state-create
    (root remote title sections limit start options hide-empty)
  "Create a dashboard state from ROOT, REMOTE, TITLE and SECTIONS."
  (pcase-let ((`(,queries . ,next-id)
               (majutsu-gerrit-dashboard--assign-query-ids sections)))
    (majutsu-gerrit-dashboard-state-create
     :root root
     :remote remote
     :title title
     :sections queries
     :limit limit
     :start start
     :options options
     :hide-empty hide-empty
     :dirty-p nil
     :next-id next-id)))

(defun majutsu-gerrit-dashboard--state-from-args (args root)
  "Return dashboard state object from transient ARGS in ROOT."
  (majutsu-gerrit-dashboard--state-create
   root
   (majutsu-gerrit-dashboard--current-remote args root)
   (transient-arg-value "--title=" args)
   (majutsu-gerrit-dashboard--queries-from-args args)
   (majutsu-gerrit-dashboard--number-arg
    "--limit=" args majutsu-gerrit-dashboard-limit)
   (majutsu-gerrit-dashboard--number-arg "--start=" args nil)
   (or (majutsu-gerrit-dashboard--arg-values "--option=" args)
       majutsu-gerrit-dashboard-options)
   (member "--hide-empty" args)))

(defun majutsu-gerrit-dashboard--state ()
  "Return the current dashboard state or signal an error."
  (or majutsu-gerrit-dashboard--state
      (user-error "No Gerrit dashboard state in this buffer")))

(defun majutsu-gerrit-dashboard--open-state (state &optional directory)
  "Open a Gerrit dashboard configured by STATE."
  (let ((default-directory (or directory
                               (majutsu-gerrit-dashboard-state-root state)
                               default-directory)))
    (majutsu-with-toplevel
      (majutsu-setup-buffer #'majutsu-gerrit-dashboard-mode nil
        :buffer "*Majutsu Gerrit*"
        :directory default-directory
        (majutsu-gerrit-dashboard--state state)))))

(defun majutsu-gerrit-dashboard--open-with-sections (sections &optional directory)
  "Open a Gerrit dashboard showing SECTIONS."
  (let ((root (majutsu--toplevel-safe directory)))
    (majutsu-gerrit-dashboard--open-state
     (majutsu-gerrit-dashboard--state-create
      root
      (majutsu-gerrit-dashboard--current-remote nil root)
      nil
      sections
      majutsu-gerrit-dashboard-limit
      nil
      majutsu-gerrit-dashboard-options
      nil)
     root)))

(defun majutsu-gerrit-dashboard--open-with-args (args &optional directory)
  "Open a Gerrit dashboard configured by transient ARGS."
  (let ((root (majutsu--toplevel-safe directory)))
    (majutsu-gerrit-dashboard--set-value args)
    (majutsu-gerrit-dashboard--open-state
     (majutsu-gerrit-dashboard--state-from-args args root)
     root)))

;;;###autoload
(defun majutsu-gerrit-dashboard (&optional query)
  "Show a Gerrit dashboard for the current repository.
With prefix argument, prompt for a single ad-hoc QUERY."
  (interactive
   (list (when current-prefix-arg
           (majutsu-gerrit-read-query
            "Gerrit query" nil 'majutsu-gerrit-dashboard-query-history))))
  (if query
      (majutsu-gerrit-dashboard--open-with-sections
       (list (cons "Query" query)))
    (majutsu-gerrit-dashboard--open-with-sections
     (copy-tree majutsu-gerrit-dashboard-queries))))

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
  (let* ((change (magit-section-value-if 'gerrit-change))
         (state (majutsu-gerrit-dashboard--state))
         (spec (majutsu-gerrit-rest-current-spec
                (majutsu-gerrit-dashboard-state-remote state)
                majutsu--default-directory))
         (url (and change (majutsu-gerrit-dashboard--change-web-url change spec))))
    (unless url
      (user-error "No Gerrit change at point"))
    (browse-url url)))

(defun majutsu-gerrit-dashboard--query-id-at-point ()
  "Return query section id at point, or nil."
  (let ((section (magit-current-section)))
    (while (and section
                (not (magit-section-match 'gerrit-query section)))
      (setq section (oref section parent)))
    (and section (oref section value))))

(defun majutsu-gerrit-dashboard--query-by-id (state id)
  "Return dashboard query with ID in STATE."
  (seq-find (lambda (query)
              (equal (majutsu-gerrit-dashboard-query-id query) id))
            (majutsu-gerrit-dashboard-state-sections state)))

(defun majutsu-gerrit-dashboard--current-query ()
  "Return dashboard query at point, or signal an error."
  (let* ((state (majutsu-gerrit-dashboard--state))
         (id (majutsu-gerrit-dashboard--query-id-at-point))
         (query (and id (majutsu-gerrit-dashboard--query-by-id state id))))
    (or query (user-error "No dashboard query section at point"))))

(defun majutsu-gerrit-dashboard--mark-dirty (state)
  "Mark dashboard STATE as modified."
  (setf (majutsu-gerrit-dashboard-state-dirty-p state) t))

(defun majutsu-gerrit-dashboard--append-query (state query)
  "Append QUERY to dashboard STATE."
  (unless (majutsu-gerrit-dashboard-query-id query)
    (setf (majutsu-gerrit-dashboard-query-id query)
          (majutsu-gerrit-dashboard-state-next-id state))
    (cl-incf (majutsu-gerrit-dashboard-state-next-id state)))
  (setf (majutsu-gerrit-dashboard-state-sections state)
        (append (majutsu-gerrit-dashboard-state-sections state)
                (list query)))
  (majutsu-gerrit-dashboard--mark-dirty state))

(defun majutsu-gerrit-dashboard-add-section ()
  "Add a query section to the current dashboard."
  (interactive)
  (let ((state (majutsu-gerrit-dashboard--state)))
    (majutsu-gerrit-dashboard--append-query
     state (majutsu-gerrit-dashboard-read-section))
    (majutsu-refresh-buffer)))

(defun majutsu-gerrit-dashboard-edit-section-query ()
  "Edit the Gerrit query for the section at point."
  (interactive)
  (let* ((state (majutsu-gerrit-dashboard--state))
         (query (majutsu-gerrit-dashboard--current-query))
         (new-query (majutsu-gerrit-read-query
                     "Gerrit query"
                     (majutsu-gerrit-dashboard-query-query query)
                     'majutsu-gerrit-dashboard-query-history
                     (majutsu-gerrit-dashboard-query-query query)
                     (majutsu-gerrit-dashboard-state-remote state))))
    (setf (majutsu-gerrit-dashboard-query-query query) new-query)
    (majutsu-gerrit-dashboard--mark-dirty state)
    (majutsu-refresh-buffer)))

(defun majutsu-gerrit-dashboard-edit-section-title ()
  "Edit the title for the section at point."
  (interactive)
  (let* ((state (majutsu-gerrit-dashboard--state))
         (query (majutsu-gerrit-dashboard--current-query))
         (title (majutsu-read-string
                 "Section title"
                 (majutsu-gerrit-dashboard-query-title query)
                 'majutsu-gerrit-dashboard-section-history
                 (majutsu-gerrit-dashboard-query-title query))))
    (setf (majutsu-gerrit-dashboard-query-title query) title)
    (majutsu-gerrit-dashboard--mark-dirty state)
    (majutsu-refresh-buffer)))

(defun majutsu-gerrit-dashboard-remove-section ()
  "Remove the section at point from the current dashboard."
  (interactive)
  (let* ((state (majutsu-gerrit-dashboard--state))
         (query (majutsu-gerrit-dashboard--current-query))
         (title (majutsu-gerrit-dashboard-query-title query)))
    (unless (y-or-n-p (format "Remove query section %S? " title))
      (user-error "Remove canceled"))
    (setf (majutsu-gerrit-dashboard-state-sections state)
          (delq query (majutsu-gerrit-dashboard-state-sections state)))
    (majutsu-gerrit-dashboard--mark-dirty state)
    (majutsu-refresh-buffer)))

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
    (majutsu-gerrit-dashboard--open-with-args args)))

(cl-defmethod transient-save-value ((obj majutsu-gerrit-dashboard-prefix))
  (let* ((obj (oref obj prototype))
         (args (transient-args (oref obj command))))
    (majutsu-gerrit-dashboard--set-value args t)
    (transient--history-push obj)
    (majutsu-gerrit-dashboard--open-with-args args)))

(cl-defmethod majutsu-transient--save-repository-defaults
  ((obj majutsu-gerrit-dashboard-prefix))
  (let* ((obj (oref obj prototype))
         (args (transient-args (oref obj command))))
    (majutsu-transient-save-repository-value
     'majutsu-gerrit 'majutsu-gerrit-dashboard
     (majutsu-gerrit-dashboard--repo-args args))
    (transient--history-push obj)
    (majutsu-gerrit-dashboard--open-with-args args)
    (message "Saved dashboard arguments as repository defaults")))

(defun majutsu-gerrit-dashboard--read-preset (prompt initial-input history)
  "Read a Gerrit dashboard preset with PROMPT."
  (completing-read prompt (mapcar #'car majutsu-gerrit-dashboard--preset-alist)
                   nil t initial-input history "default"))

(defun majutsu-gerrit-dashboard--transient-remote ()
  "Return explicit remote selected by the active dashboard transient."
  (transient-arg-value "--remote=" (transient-get-value)))

(defun majutsu-gerrit-dashboard--read-query (prompt initial-input history)
  "Read a Gerrit query for dashboard prompts."
  (majutsu-gerrit-read-query
   prompt initial-input
   (or history 'majutsu-gerrit-dashboard-query-history)
   nil
   (majutsu-gerrit-dashboard--transient-remote)))

(defun majutsu-gerrit-dashboard-read-section (&optional section)
  "Read a dashboard section, defaulting from SECTION."
  (let* ((title (majutsu-read-string
                 "Section title"
                 (and section (majutsu-gerrit-dashboard-query-title section))
                 'majutsu-gerrit-dashboard-section-history
                 (and section (majutsu-gerrit-dashboard-query-title section))))
         (query (majutsu-gerrit-read-query
                 "Gerrit query"
                 (and section (majutsu-gerrit-dashboard-query-query section))
                 'majutsu-gerrit-dashboard-query-history
                 (and section (majutsu-gerrit-dashboard-query-query section))
                 (and (boundp 'majutsu-gerrit-dashboard--state)
                      majutsu-gerrit-dashboard--state
                      (majutsu-gerrit-dashboard-state-remote
                       majutsu-gerrit-dashboard--state)))))
    (majutsu-gerrit-dashboard-query-create :title title :query query)))

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
  :reader #'majutsu-gerrit-dashboard--read-query)

(transient-define-argument majutsu-gerrit-dashboard:--foreach ()
  :description "Foreach"
  :class 'transient-option
  :shortarg "-F"
  :argument "--foreach="
  :prompt "Append to every query"
  :reader (lambda (prompt initial-input history)
            (majutsu-gerrit-read-query
             prompt initial-input
             (or history 'majutsu-gerrit-dashboard-foreach-history)
             nil
             (majutsu-gerrit-dashboard--transient-remote))))

(transient-define-argument majutsu-gerrit-dashboard:--remote ()
  :description "Remote"
  :class 'transient-option
  :shortarg "-R"
  :argument "--remote="
  :prompt "Remote"
  :reader #'majutsu-transient-read-remote-name)

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
    (majutsu-gerrit-dashboard:--foreach)]
   ["Context"
    (majutsu-gerrit-dashboard:--remote)]
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
     :transient t)]
   ["Sections"
    :if-derived majutsu-gerrit-dashboard-mode
    ("a" "Add query section" majutsu-gerrit-dashboard-add-section)
    ("e" "Edit query" majutsu-gerrit-dashboard-edit-section-query)
    ("E" "Edit title" majutsu-gerrit-dashboard-edit-section-title)
    ("k" "Remove section" majutsu-gerrit-dashboard-remove-section)]]
  (interactive)
  (if (not (eq transient-current-command 'majutsu-gerrit-dashboard-transient))
      (transient-setup 'majutsu-gerrit-dashboard-transient)
    (let ((args (transient-args 'majutsu-gerrit-dashboard-transient)))
      (majutsu-gerrit-dashboard--open-with-args args))))

(defvar-keymap majutsu-gerrit-change-section-map
  :doc "Keymap for Gerrit change sections."
  "b"   #'majutsu-gerrit-dashboard-browse-change
  "d"   #'majutsu-gerrit-download-change
  "<remap> <majutsu-visit-thing>" #'majutsu-gerrit-dashboard-browse-change)

(defvar-keymap majutsu-gerrit-dashboard-mode-map
  :doc "Keymap for `majutsu-gerrit-dashboard-mode'."
  :parent magit-section-mode-map
  "RET" #'majutsu-gerrit-dashboard-browse-change
  "g" #'majutsu-refresh
  "q" #'majutsu-mode-bury-buffer
  "a" #'majutsu-gerrit-dashboard-add-section
  "e" #'majutsu-gerrit-dashboard-edit-section-query
  "E" #'majutsu-gerrit-dashboard-edit-section-title
  "<remap> <majutsu-delete-thing>" #'majutsu-gerrit-dashboard-remove-section
  "b" #'majutsu-gerrit-dashboard-browse-change
  "d" #'majutsu-gerrit-download-change
  "R" #'majutsu-gerrit-dashboard-transient)

(defun majutsu-gerrit-dashboard--kill-buffer-query ()
  "Ask before killing a modified dashboard buffer."
  (or (not (and majutsu-gerrit-dashboard--state
                (majutsu-gerrit-dashboard-state-dirty-p
                 majutsu-gerrit-dashboard--state)))
      (y-or-n-p "Kill buffer and discard dashboard changes? ")))

(define-derived-mode majutsu-gerrit-dashboard-mode majutsu-mode "Majutsu Gerrit"
  "Major mode for Gerrit dashboards."
  :group 'majutsu-gerrit-dashboard
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer)
  (add-hook 'kill-buffer-query-functions
            #'majutsu-gerrit-dashboard--kill-buffer-query nil t))

(cl-defmethod majutsu-buffer-value (&context (major-mode majutsu-gerrit-dashboard-mode))
  "Return the dashboard query identity for the current buffer."
  (when majutsu-gerrit-dashboard--state
    (mapcar #'majutsu-gerrit-dashboard-query-id
            (majutsu-gerrit-dashboard-state-sections
             majutsu-gerrit-dashboard--state))))

(defun majutsu-gerrit-dashboard-refresh-buffer ()
  "Refresh the Gerrit dashboard buffer."
  (majutsu--assert-mode 'majutsu-gerrit-dashboard-mode)
  (let* ((state (majutsu-gerrit-dashboard--state))
         (sections (majutsu-gerrit-dashboard-state-sections state))
         (groups (and sections
                      (majutsu-gerrit-dashboard--fetch
                       sections
                       (majutsu-gerrit-dashboard-state-remote state)
                       majutsu--default-directory
                       (majutsu-gerrit-dashboard-state-limit state)
                       (majutsu-gerrit-dashboard-state-start state)
                       (majutsu-gerrit-dashboard-state-options state)))))
    (magit-insert-section (gerrit-dashboard state)
      (magit-insert-heading
        (format "%s\n" (or (majutsu-gerrit-dashboard-state-title state)
                           "Gerrit Dashboard")))
      (let ((inserted 0))
        (dolist (group groups)
          (unless (and (majutsu-gerrit-dashboard-state-hide-empty state)
                       (null (cdr group)))
            (cl-incf inserted)
            (majutsu-gerrit-dashboard--insert-query (car group) (cdr group))))
        (when (zerop inserted)
          (insert (majutsu-gerrit-dashboard--face
                   "No matching dashboard sections.\n" 'shadow)))))))

(provide 'majutsu-gerrit-dashboard)

;;; majutsu-gerrit-dashboard.el ends here
