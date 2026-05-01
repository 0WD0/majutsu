;;; majutsu-op.el --- JJ Operation view for majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides helpers and transients for jj op workflows.

;;; Code:

(require 'cl-lib)
(require 'majutsu)
(require 'subr-x)
(require 'transient)

(declare-function majutsu-jj-apply-ansi "majutsu-jj" (string))
(declare-function majutsu-jj-colored-string "majutsu-jj" (&rest args))
(declare-function majutsu-jj-strip-ansi "majutsu-jj" (string))

;;; majutsu-undo

;;;###autoload
(defun majutsu-undo ()
  "Undo the last change."
  (interactive)
  (if (not (majutsu-confirm 'undo "Undo the most recent change? "))
      (message "Undo canceled")
    (let ((revset (magit-section-value-if 'jj-commit)))
      (when (zerop (majutsu-run-jj "undo"))
        (when revset
          (majutsu-goto-commit revset))))))

;;; majutsu-redo

;;;###autoload
(defun majutsu-redo ()
  "Redo the last undone change."
  (interactive)
  (if (not (majutsu-confirm 'redo "Redo the previously undone change? "))
      (message "Redo canceled")
    (let ((revset (magit-section-value-if 'jj-commit)))
      (when (zerop (majutsu-run-jj "redo"))
        (when revset
          (majutsu-goto-commit revset))))))

;;; shared templates

(defconst majutsu-op--field-separator "\x1e"
  "Field separator used by Majutsu operation templates.")

(defconst majutsu-op--log-template
  (majutsu-tpl
   [:concat
    [:separate "\x1e"
               [:id]
               [:label "id short" [:id :short]]
               [:label "user" [:user]]
               [:label "workspace_name" [:workspace_name]]
               [:label "time end ago" [:method [:time :end] :ago]]
               [:if [:snapshot]
                   [:label "snapshot" "snapshot"]
                 "op"]
               [:label "description first_line"
                       [:method [:description] :first_line]]]
    "\n"]
   'Operation)
  "Template used by `majutsu-op-log'.")

(defconst majutsu-op--show-template
  (majutsu-tpl
   [:concat
    [:separate "\x1e"
               [:id]
               [:label "id short" [:id :short]]
               [:label "user" [:user]]
               [:label "workspace_name" [:workspace_name]]
               [:label "time start" [:method [:time :start] :format "%Y-%m-%d %H:%M:%S"]]
               [:label "time end" [:method [:time :end] :format "%Y-%m-%d %H:%M:%S"]]
               [:label "time duration" [:method [:time] :duration]]
               [:if [:snapshot]
                   [:label "snapshot" "snapshot"]
                 "op"]
               [:label "description first_line"
                       [:description :first_line]]]
    "\n"]
   'Operation)
  "Template used for operation metadata in `majutsu-op-show'.")

(defconst majutsu-op--commit-summary-template
  (majutsu-tpl
   [:concat
    [:separate "\x1e"
               [:change_id]
               [:change_id :short]
               [:commit_id]
               [:commit_id :short]
               [:if [:hidden] "hidden" ""]
               [:if [:conflict] "conflict" ""]
               [:if [:empty] "empty" ""]
               [:coalesce
                [:if [:description]
                     [:method [:description] :first_line]]
                [:if [:empty] "(empty)"]
                "(no description set)"]]]
   'Commit)
  "Template injected as `templates.commit_summary' for operation diffs.")

(defun majutsu-op--machine-field (field)
  "Return FIELD without ANSI escapes or surrounding whitespace."
  (string-trim (majutsu-jj-strip-ansi (or field ""))))

(defun majutsu-op--display-field (field)
  "Return FIELD with ANSI escapes converted to text properties."
  (majutsu-jj-apply-ansi (or field "")))

(defun majutsu-op--split-record (line expected)
  "Split LINE into EXPECTED fields, or return nil when malformed."
  (let ((fields (split-string line majutsu-op--field-separator)))
    (and (= (length fields) expected) fields)))

(defun majutsu-op--parse-log-line (line)
  "Parse one operation log LINE into a plist."
  (when-let* ((fields (majutsu-op--split-record line 7)))
    (pcase-let ((`(,op-id ,op-id-short ,user ,workspace ,time ,kind ,description)
                 fields))
      (list :op-id (majutsu-op--machine-field op-id)
            :op-id-short (majutsu-op--display-field op-id-short)
            :user (majutsu-op--display-field user)
            :workspace (majutsu-op--display-field workspace)
            :time (majutsu-op--display-field time)
            :kind (majutsu-op--machine-field kind)
            :desc (majutsu-op--display-field description)))))

(defun majutsu-op--parse-show-line (line)
  "Parse one operation metadata LINE into a plist."
  (when-let* ((fields (majutsu-op--split-record line 9)))
    (pcase-let ((`(,op-id ,op-id-short ,user ,workspace
                   ,start-time ,end-time ,duration ,kind ,description)
                 fields))
      (list :op-id (majutsu-op--machine-field op-id)
            :op-id-short (majutsu-op--display-field op-id-short)
            :user (majutsu-op--display-field user)
            :workspace (majutsu-op--display-field workspace)
            :start-time (majutsu-op--display-field start-time)
            :end-time (majutsu-op--display-field end-time)
            :duration (majutsu-op--display-field duration)
            :kind (majutsu-op--machine-field kind)
            :desc (majutsu-op--display-field description)))))

(defun majutsu-op--parse-log-output (output)
  "Parse operation log OUTPUT into entry plists."
  (delq nil
        (mapcar #'majutsu-op--parse-log-line
                (split-string (or output "") "\n" t))))

(defun majutsu-op--commit-summary-config-arg ()
  "Return a jj --config argument for operation commit summaries."
  (concat "templates.commit_summary=" majutsu-op--commit-summary-template))

(defun majutsu-op--operation-at-point ()
  "Return the operation id at point, or nil."
  (or (magit-section-value-if 'jj-op)
      (magit-section-value-if 'jj-op-show)))

(defun majutsu-op--read-operation (prompt)
  "Read an operation id with PROMPT, defaulting to point or @."
  (let* ((default (or (majutsu-op--operation-at-point) "@"))
         (value (read-string (format "%s (default %s): " prompt default)
                             nil nil default)))
    (if (string-empty-p value) default value)))

(defun majutsu-op--shorten-id (id)
  "Return a short display prefix for operation ID."
  (if (> (length id) 12)
      (substring id 0 12)
    id))

;;; op transient

;;;###autoload(autoload 'majutsu-op-transient "majutsu-op" nil t)
(transient-define-prefix majutsu-op-transient ()
  "Transient for jj operation commands."
  [["History"
    ("l" "Log" majutsu-op-log)
    ("s" "Show" majutsu-op-show)]
   ["State"
    ("u" "Undo" majutsu-undo)
    ("r" "Redo" majutsu-redo)]
   [("q" "Quit" transient-quit-one)]])

;;; op log

(defvar-local majutsu-op-log--cached-entries nil
  "Cached operation log entries.")

(defun majutsu-parse-op-log-entries (&optional buf log-output)
  "Parse jj operation log output.

When LOG-OUTPUT is nil, run jj in BUF or the current buffer and cache the
result.  Colored output is preserved in display fields, while machine ids are
stored without ANSI escapes."
  (with-current-buffer (or buf (current-buffer))
    (if (and majutsu-op-log--cached-entries (not log-output))
        majutsu-op-log--cached-entries
      (let ((entries (majutsu-op--parse-log-output
                      (or log-output
                          (majutsu-jj-colored-string
                           "op" "log" "--no-graph" "-T" majutsu-op--log-template)))))
        (unless log-output
          (setq majutsu-op-log--cached-entries entries))
        entries))))

(defun majutsu-op-log-insert-entries ()
  "Insert operation log entries."
  (magit-insert-section (jj-op-log)
    (magit-insert-heading "Operation Log")
    (dolist (entry (majutsu-parse-op-log-entries))
      (let ((op-id (plist-get entry :op-id)))
        (magit-insert-section (jj-op op-id t)
          (magit-insert-heading
            (format "%-12s %-18s %-14s %-12s %s"
                    (plist-get entry :op-id-short)
                    (plist-get entry :user)
                    (plist-get entry :workspace)
                    (plist-get entry :time)
                    (plist-get entry :desc)))
          (insert "\n"))))))

(defun majutsu-op-log-render ()
  "Render the op log buffer."
  (magit-insert-section (oplog)
    (majutsu-op-log-insert-entries)))

(defun majutsu-op-log-refresh-buffer ()
  "Refresh the op log buffer."
  (interactive)
  (majutsu--assert-mode 'majutsu-op-log-mode)
  (setq majutsu-op-log--cached-entries nil)
  (majutsu-op-log-render))

(defun majutsu-op-log-show-at-point ()
  "Show the operation at point."
  (interactive)
  (if-let* ((op-id (majutsu-op--operation-at-point)))
      (majutsu-op-show op-id)
    (user-error "No operation at point")))

(defvar-keymap majutsu-op-log-mode-map
  :doc "Keymap for `majutsu-op-log-mode'."
  :parent majutsu-mode-map
  "RET" 'majutsu-op-log-show-at-point
  "d" 'majutsu-op-log-show-at-point)

(define-derived-mode majutsu-op-log-mode majutsu-mode "Majutsu Op Log"
  "Major mode for viewing jj operation log."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

;;;###autoload
(defun majutsu-op-log ()
  "Open the Majutsu operation log."
  (interactive)
  (let* ((root (majutsu--toplevel-safe))
         (repo (file-name-nondirectory (directory-file-name root))))
    (majutsu-setup-buffer #'majutsu-op-log-mode nil
      :buffer (format "*majutsu-op: %s*" repo)
      :directory root)))

;;; op show

(defvar-local majutsu-op-show--operation nil
  "Operation id shown in the current operation show buffer.")

(defun majutsu-op--show-output (operation template)
  "Return colored `jj op show' output for OPERATION rendered with TEMPLATE."
  (majutsu-jj-colored-string
   "op" "show" operation "--no-op-diff" "-T" template))

(defun majutsu-op--show-metadata (operation)
  "Return metadata plist for OPERATION."
  (or (majutsu-op--parse-show-line
       (car (split-string (majutsu-op--show-output operation majutsu-op--show-template)
                          "\n" t)))
      (user-error "Failed to parse operation metadata for %s" operation)))

(defun majutsu-op--operation-body (operation template)
  "Return colored body output for OPERATION rendered with TEMPLATE."
  (majutsu-op--show-output operation template))

(defun majutsu-op--operation-diff-output (operation)
  "Return colored operation diff output for OPERATION."
  (majutsu-jj-colored-string
   "--config" (majutsu-op--commit-summary-config-arg)
   "op" "diff" "--operation" operation "--no-graph"))

(defun majutsu-op--insert-field (label value)
  "Insert metadata LABEL and VALUE."
  (insert (propertize label 'face 'font-lock-keyword-face) ": " value "\n"))

(defun majutsu-op--insert-colored-block (string)
  "Insert STRING after applying ANSI colors."
  (let ((text (majutsu-jj-apply-ansi (or string ""))))
    (unless (string-empty-p (string-trim text))
      (insert text)
      (unless (string-suffix-p "\n" text)
        (insert "\n")))))

(defun majutsu-op-show-refresh-buffer ()
  "Refresh the current operation show buffer."
  (interactive)
  (majutsu--assert-mode 'majutsu-op-show-mode)
  (let* ((operation (or majutsu-op-show--operation "@"))
         (metadata (majutsu-op--show-metadata operation))
         (op-id (plist-get metadata :op-id)))
    (setq majutsu-op-show--operation op-id)
    (magit-insert-section (jj-op-show op-id t)
      (magit-insert-heading
        (format "Operation %s" (plist-get metadata :op-id-short)))
      (magit-insert-section (jj-op-show-metadata op-id t)
        (magit-insert-heading "Metadata")
        (majutsu-op--insert-field "User" (plist-get metadata :user))
        (majutsu-op--insert-field "Workspace" (plist-get metadata :workspace))
        (majutsu-op--insert-field "Started" (plist-get metadata :start-time))
        (majutsu-op--insert-field "Ended" (plist-get metadata :end-time))
        (majutsu-op--insert-field "Duration" (plist-get metadata :duration))
        (majutsu-op--insert-field "Kind" (plist-get metadata :kind)))
      (let ((description (majutsu-op--operation-body op-id "self.description()")))
        (unless (string-empty-p (string-trim (majutsu-jj-strip-ansi description)))
          (magit-insert-section (jj-op-show-description op-id t)
            (magit-insert-heading "Description")
            (majutsu-op--insert-colored-block description))))
      (let ((tags (majutsu-op--operation-body op-id "self.tags()")))
        (unless (string-empty-p (string-trim (majutsu-jj-strip-ansi tags)))
          (magit-insert-section (jj-op-show-tags op-id t)
            (magit-insert-heading "Tags")
            (majutsu-op--insert-colored-block tags))))
      (magit-insert-section (jj-op-diff op-id t)
        (magit-insert-heading "Repository Changes")
        (majutsu-op--insert-colored-block
         (majutsu-op--operation-diff-output op-id))))))

(defvar-keymap majutsu-op-show-mode-map
  :doc "Keymap for `majutsu-op-show-mode'."
  :parent majutsu-mode-map)

(define-derived-mode majutsu-op-show-mode majutsu-mode "Majutsu Op Show"
  "Major mode for viewing one jj operation."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

(defun majutsu-op-show--buffer-name (operation)
  "Return buffer name for OPERATION."
  (let* ((root (majutsu--toplevel-safe))
         (repo (file-name-nondirectory (directory-file-name root))))
    (format "*majutsu-op-show: %s:%s*" repo (majutsu-op--shorten-id operation))))

;;;###autoload
(defun majutsu-op-show (operation)
  "Show repository changes in OPERATION."
  (interactive (list (majutsu-op--read-operation "Show operation")))
  (let ((root (majutsu--toplevel-safe)))
    (majutsu-setup-buffer #'majutsu-op-show-mode nil
      :buffer (majutsu-op-show--buffer-name operation)
      :directory root
      (majutsu-op-show--operation operation))))

;;; _
(provide 'majutsu-op)
;;; majutsu-op.el ends here
