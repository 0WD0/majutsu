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
(require 'subr-x)
(require 'url-util)

(autoload 'majutsu-gerrit-download-change "majutsu-gerrit-download" nil t)

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

(defvar-local majutsu-gerrit-dashboard--remote nil
  "Gerrit remote selected for this dashboard buffer.")

(defvar-local majutsu-gerrit-dashboard--queries nil
  "Named Gerrit queries shown in this dashboard buffer.")

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

(defun majutsu-gerrit-dashboard--normalize-query-response (queries response)
  "Return RESPONSE as one result list per query in QUERIES."
  (if (= (length queries) 1)
      (list response)
    response))

(defun majutsu-gerrit-dashboard--fetch (queries &optional remote directory)
  "Fetch dashboard QUERIES for REMOTE in DIRECTORY.
Return a list of (QUERY-SPEC . CHANGES), where CHANGES are
`majutsu-gerrit-change' objects."
  (let* ((query-specs (majutsu-gerrit-dashboard--query-specs queries))
         (query-strings (mapcar #'cdr query-specs))
         (spec (majutsu-gerrit-rest-current-spec remote directory))
         (raw (majutsu-gerrit-rest-change-query
               query-strings majutsu-gerrit-dashboard-limit nil
               majutsu-gerrit-dashboard-options spec))
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

;;;###autoload
(defun majutsu-gerrit-dashboard (&optional query)
  "Show a Gerrit dashboard for the current repository.
With prefix argument, prompt for a single ad-hoc QUERY."
  (interactive
   (list (when current-prefix-arg
           (read-string "Gerrit query: " nil
                        'majutsu-gerrit-dashboard-query-history))))
  (let* ((root (majutsu--toplevel-safe))
         (remote (when (fboundp 'majutsu-gerrit--selected-remote)
                   (majutsu-gerrit--selected-remote nil root)))
         (queries (if query
                      (list (cons "Query" query))
                    majutsu-gerrit-dashboard-queries)))
    (majutsu-setup-buffer #'majutsu-gerrit-dashboard-mode nil
      :buffer "*Majutsu Gerrit*"
      :directory root
      (majutsu-gerrit-dashboard--remote remote)
      (majutsu-gerrit-dashboard--queries queries))))

(defun majutsu-gerrit-dashboard-browse-change ()
  "Open the Gerrit change at point in a browser."
  (interactive)
  (let* ((change (magit-section-value-if 'majutsu-gerrit-change))
         (spec (majutsu-gerrit-rest-current-spec
                majutsu-gerrit-dashboard--remote majutsu--default-directory))
         (url (and change (majutsu-gerrit-dashboard--change-web-url change spec))))
    (unless url
      (user-error "No Gerrit change at point"))
    (browse-url url)))

(defvar-keymap majutsu-gerrit-change-section-map
  :doc "Keymap for Gerrit change sections."
  "b"   #'majutsu-gerrit-dashboard-browse-change
  "d"   #'majutsu-gerrit-download-change
  "<remap> <majutsu-visit-thing>" #'majutsu-gerrit-dashboard-browse-change)

(defvar-keymap majutsu-gerrit-dashboard-mode-map
  :doc "Keymap for `majutsu-gerrit-dashboard-mode'."
  :parent majutsu-mode-map
  "b" #'majutsu-gerrit-dashboard-browse-change
  "d" #'majutsu-gerrit-download-change)

(define-derived-mode majutsu-gerrit-dashboard-mode majutsu-mode "Majutsu Gerrit"
  "Major mode for Gerrit dashboards."
  :group 'majutsu-gerrit-dashboard
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

(cl-defmethod majutsu-buffer-value (&context (major-mode majutsu-gerrit-dashboard-mode))
  "Return the dashboard query identity for the current buffer."
  (list majutsu-gerrit-dashboard--remote majutsu-gerrit-dashboard--queries))

(defun majutsu-gerrit-dashboard-refresh-buffer ()
  "Refresh the Gerrit dashboard buffer."
  (majutsu--assert-mode 'majutsu-gerrit-dashboard-mode)
  (let ((groups (majutsu-gerrit-dashboard--fetch
                 majutsu-gerrit-dashboard--queries
                 majutsu-gerrit-dashboard--remote
                 majutsu--default-directory)))
    (magit-insert-section (majutsu-gerrit-dashboard)
      (magit-insert-heading "Gerrit Dashboard\n")
      (dolist (group groups)
        (majutsu-gerrit-dashboard--insert-query (car group) (cdr group))))))

(provide 'majutsu-gerrit-dashboard)

;;; majutsu-gerrit-dashboard.el ends here
