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
(require 'seq)
(require 'subr-x)
(require 'transient)

(declare-function majutsu-diff-revset "majutsu-diff" (revset &optional args range filesets))
(declare-function majutsu-evolog "majutsu-evolog" (revset))
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
    [:id]
    "\x1e"
    [:label "id short" [:id :short]]
    "\x1e"
    [:if [:current_operation]
        [:label "current_operation" "@"]
      ""]
    "\x1e"
    [:label "user" [:user]]
    "\x1e"
    [:label "workspace_name" [:workspace_name]]
    "\x1e"
    [:label "time end" [:method [:time :end] :format "%Y-%m-%d %H:%M:%S"]]
    "\x1e"
    [:label "time end ago" [:method [:time :end] :ago]]
    "\x1e"
    [:label "time duration" [:method [:time] :duration]]
    "\x1e"
    [:if [:snapshot]
        [:label "snapshot" "snapshot"]
      "op"]
    "\x1e"
    [:label "description first_line"
            [:method [:description] :first_line]]
    "\x1e"
    [:label "tags first_line" [:method [:tags] :first_line]]
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
   [:join "\x1e"
          [:change_id]
          [:change_id :short]
          [:commit_id]
          [:commit_id :short]
          [:if [:hidden] "hidden"]
          [:if [:conflict] "conflict"]
          [:if [:empty] "empty"]
          [:coalesce
           [:if [:description]
               [:description :first_line]]
           "(no description set)"]]
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
  (when-let* ((fields (majutsu-op--split-record line 11)))
    (pcase-let ((`(,op-id ,op-id-short ,current ,user ,workspace
                  ,time ,time-ago ,duration ,kind ,description ,tags)
                 fields))
      (list :op-id (majutsu-op--machine-field op-id)
            :op-id-short (majutsu-op--display-field op-id-short)
            :current (majutsu-op--machine-field current)
            :user (majutsu-op--display-field user)
            :workspace (majutsu-op--display-field workspace)
            :time (majutsu-op--display-field time)
            :time-ago (majutsu-op--display-field time-ago)
            :duration (majutsu-op--display-field duration)
            :kind (majutsu-op--machine-field kind)
            :desc (majutsu-op--display-field description)
            :tags (majutsu-op--display-field tags)))))

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

(defun majutsu-op--nonempty-field-p (field)
  "Return non-nil when FIELD is present and non-empty."
  (and field (not (string-empty-p (string-trim field)))))

(defun majutsu-op--ansi-sequence-end (string start)
  "Return the index after the ANSI escape sequence at START in STRING."
  (let ((index (1+ start))
        (length (length string)))
    (if (and (< index length) (= (aref string index) ?\[))
        (progn
          (setq index (1+ index))
          (while (and (< index length)
                      (not (and (>= (aref string index) ?@)
                                (<= (aref string index) ?~))))
            (setq index (1+ index)))
          (min length (1+ index)))
      (min length (1+ index)))))

(defun majutsu-op--drop-visible-prefix (string count)
  "Drop COUNT visible characters from STRING, ignoring ANSI escapes."
  (let ((index 0)
        (visible 0)
        (length (length string)))
    (while (and (< index length) (< visible count))
      (if (= (aref string index) ?\e)
          (setq index (majutsu-op--ansi-sequence-end string index))
        (setq index (1+ index)
              visible (1+ visible))))
    (substring string index)))

(defun majutsu-op--parse-commit-summary (plain-summary &optional colored-summary)
  "Parse a structured commit PLAIN-SUMMARY and optional COLORED-SUMMARY."
  (when-let* ((fields (majutsu-op--split-record plain-summary 8)))
    (pcase-let* ((`(,change-id ,change-id-short ,commit-id ,commit-id-short
                    ,hidden ,conflict ,empty ,_description)
                  fields)
                 (display-fields (or (majutsu-op--split-record
                                      (or colored-summary plain-summary) 8)
                                     fields))
                 (`(,_ ,change-id-short-display ,_ ,commit-id-short-display
                    ,_ ,_ ,_ ,description-display)
                  display-fields))
      (list :change-id (majutsu-op--machine-field change-id)
            :change-id-short (majutsu-op--machine-field change-id-short)
            :change-id-short-display (majutsu-op--display-field change-id-short-display)
            :commit-id (majutsu-op--machine-field commit-id)
            :commit-id-short (majutsu-op--machine-field commit-id-short)
            :commit-id-short-display (majutsu-op--display-field commit-id-short-display)
            :hidden (majutsu-op--nonempty-field-p hidden)
            :conflict (majutsu-op--nonempty-field-p conflict)
            :empty (majutsu-op--nonempty-field-p empty)
            :description (majutsu-op--display-field description-display)))))

(defun majutsu-op--diff-strip-prefix (payload)
  "Return (PREFIX TARGET OFFSET) for a diff marker PAYLOAD."
  (if (string-match "\\`\\(tracked\\|untracked\\|(added)\\|(removed)\\) \\(.*\\)\\'" payload)
      (list (match-string 1 payload)
            (match-string 2 payload)
            (match-beginning 2))
    (list nil payload 0)))

(defun majutsu-op--diff-group-kind (plain-line)
  "Return the operation diff group kind for PLAIN-LINE, or nil."
  (let ((line (string-trim plain-line)))
    (cond
     ((string= line "Changed commits:") 'commits)
     ((string-match-p "\\`Changed working copy .+:\\'" line) 'working-copy)
     ((string= line "Changed local bookmarks:") 'local-bookmarks)
     ((string= line "Changed local tags:") 'local-tags)
     ((string= line "Changed remote bookmarks:") 'remote-bookmarks)
     ((string= line "Changed remote tags:") 'remote-tags)
     ((string-match-p "\\`Changed .+:\\'" line) 'changed))))

(defun majutsu-op--diff-ref-group-p (kind)
  "Return non-nil when KIND contains named ref entries."
  (memq kind '(local-bookmarks local-tags remote-bookmarks remote-tags)))

(defun majutsu-op--diff-operation-header-p (plain-line)
  "Return non-nil when PLAIN-LINE is an op-diff from/to header."
  (string-match-p "\\`\\(?:From\\|To\\) operation:" (string-trim-left plain-line)))

(defun majutsu-op--diff-elision-line-p (plain-line)
  "Return non-nil when PLAIN-LINE is an op-diff elision line."
  (string-match-p "\\`(Elided " (string-trim plain-line)))

(defun majutsu-op--diff-warning-line-p (plain-line)
  "Return non-nil when PLAIN-LINE is a jj warning."
  (string-prefix-p "Warning:" (string-trim-left plain-line)))

(defun majutsu-op--diff-ref-header-p (plain-line group-kind)
  "Return non-nil when PLAIN-LINE names a ref in GROUP-KIND."
  (and (majutsu-op--diff-ref-group-p group-kind)
       (not (string-match-p "[+-] " plain-line))
       (string-suffix-p ":" (string-trim plain-line))))

(defun majutsu-op--parse-diff-marker-line (plain-line colored-line group-kind ref-name)
  "Parse a marker PLAIN-LINE/COLORED-LINE in GROUP-KIND under REF-NAME."
  (when (and group-kind (string-match "\\([+-]\\) \\(.*\\)\\'" plain-line))
    (let* ((marker (match-string 1 plain-line))
           (payload (match-string 2 plain-line))
           (payload-start (match-beginning 2))
           (line-type (if (memq group-kind '(commits working-copy))
                          'commit-line
                        'ref-line)))
      (pcase-let* ((`(,prefix ,target ,target-offset)
                    (majutsu-op--diff-strip-prefix payload))
                   (summary-start (+ payload-start target-offset))
                   (colored-summary (majutsu-op--drop-visible-prefix
                                     colored-line summary-start))
                   (base (list :marker marker
                               :prefix prefix
                               :group-kind group-kind
                               :ref-name ref-name)))
        (if (string= target "(absent)")
            (list :type line-type
                  :text colored-line
                  :value (append base '(:absent t)))
          (if-let* ((summary (majutsu-op--parse-commit-summary
                              target colored-summary)))
              (list :type line-type
                    :text colored-line
                    :value (append base summary))
            (list :type 'raw-line :text colored-line)))))))

(defun majutsu-op--parse-diff-output (output)
  "Parse colored jj op diff OUTPUT into renderable nodes."
  (let (nodes group-title group-text group-kind group-children ref-name ref-text ref-children)
    (cl-labels
        ((finish-ref
           ()
           (when ref-name
             (push (list :type 'ref
                         :name ref-name
                         :text ref-text
                         :children (nreverse ref-children))
                   group-children)
             (setq ref-name nil
                   ref-text nil
                   ref-children nil)))
         (finish-group
           ()
           (finish-ref)
           (when group-title
             (push (list :type 'group
                         :title group-title
                         :text group-text
                         :kind group-kind
                         :children (nreverse group-children))
                   nodes)
             (setq group-title nil
                   group-text nil
                   group-kind nil
                   group-children nil)))
         (add-node
           (node)
           (cond
            (ref-name (push node ref-children))
            (group-title (push node group-children))
            (t (push node nodes)))))
      (dolist (colored-line (split-string (or output "") "\n"))
        (let* ((plain-line (majutsu-jj-strip-ansi colored-line))
               (trimmed (string-trim plain-line)))
          (cond
           ((string-empty-p trimmed))
           ((majutsu-op--diff-operation-header-p plain-line))
           ((majutsu-op--diff-group-kind plain-line)
            (finish-group)
            (setq group-title trimmed
                  group-text colored-line
                  group-kind (majutsu-op--diff-group-kind plain-line)))
           ((majutsu-op--diff-warning-line-p plain-line)
            (add-node (list :type 'warning :text colored-line)))
           ((majutsu-op--diff-elision-line-p plain-line)
            (add-node (list :type 'elision :text colored-line)))
           ((majutsu-op--diff-ref-header-p plain-line group-kind)
            (finish-ref)
            (setq ref-name (string-remove-suffix ":" trimmed)
                  ref-text colored-line))
           ((majutsu-op--parse-diff-marker-line
             plain-line colored-line group-kind ref-name)
            (add-node (majutsu-op--parse-diff-marker-line
                       plain-line colored-line group-kind ref-name)))
           (t
            (add-node (list :type 'raw-line :text colored-line))))))
      (finish-group)
      (nreverse nodes))))

(defun majutsu-op--commit-summary-config-arg ()
  "Return a jj --config argument for operation commit summaries."
  (concat "templates.commit_summary=" majutsu-op--commit-summary-template))

(defun majutsu-op--operation-at-point ()
  "Return the operation id at point, or nil."
  (or (magit-section-value-if 'jj-op)
      (magit-section-value-if 'jj-op-show)))

(defun majutsu-op--diff-line-at-point ()
  "Return the parsed operation diff line value at point, or nil."
  (or (magit-section-value-if 'jj-op-commit-line)
      (magit-section-value-if 'jj-op-ref-line)))

(defun majutsu-op--diff-line-commit-revset (line)
  "Return a commit-id revset for parsed operation diff LINE."
  (when-let* ((commit-id (and (not (plist-get line :absent))
                              (plist-get line :commit-id))))
    (format "commit_id(%s)" commit-id)))

(defun majutsu-op--diff-line-evolog-revset (line)
  "Return an evolog revset for parsed operation diff LINE."
  (when-let* ((change-id (and (not (plist-get line :absent))
                              (plist-get line :change-id)))
              (commit-id (plist-get line :commit-id)))
    (format "change_id(%s) | commit_id(%s)" change-id commit-id)))

(defun majutsu-op--read-operation (prompt)
  "Read an operation id with PROMPT, defaulting to point or @."
  (let* ((default (or (majutsu-op--operation-at-point) "@"))
         (value (read-string (format "%s (default %s): " prompt default)
                             nil nil default)))
    (if (string-empty-p value) default value)))

(defconst majutsu-op--read-only-global-args
  '("--at-op=@" "--ignore-working-copy")
  "Top-level jj arguments for read-only operation queries.")

(defun majutsu-op--shorten-id (id)
  "Return a short display prefix for operation ID."
  (if (> (length id) 12)
      (substring id 0 12)
    id))

(defun majutsu-op--transient-read-operation (prompt initial-input history)
  "Read an operation for a transient option.
PROMPT, INITIAL-INPUT, and HISTORY follow transient reader conventions."
  (read-string prompt initial-input history
               (or (majutsu-op--operation-at-point) "@")))

;;; op transient

;;;###autoload(autoload 'majutsu-op-transient "majutsu-op" nil t)
(transient-define-prefix majutsu-op-transient ()
  "Transient for jj operation commands."
  [["History"
    ("l" "Log..." majutsu-op-log-transient)
    ("s" "Show" majutsu-op-show)
    ("d" "Diff..." majutsu-op-diff-transient)]
   ["State"
    ("u" "Undo" majutsu-undo)
    ("r" "Redo" majutsu-redo)
    ("R" "Restore" majutsu-op-restore)
    ("V" "Revert" majutsu-op-revert)]])

;;; op log

(defvar-local majutsu-op-log--args nil
  "Arguments used for the current operation log buffer.")

(defvar-local majutsu-op-log--cached-entries nil
  "Cached operation log entries.")

(defun majutsu-op-log-arguments ()
  "Return operation log arguments from the active transient, if any."
  (if (eq transient-current-command 'majutsu-op-log-transient)
      (transient-args 'majutsu-op-log-transient)
    '()))

(defun majutsu-op--log-command-args (&optional args)
  "Return jj arguments for operation log ARGS."
  (append majutsu-op--read-only-global-args
          '("op" "log" "--no-graph")
          (or args majutsu-op-log--args)
          (list "-T" majutsu-op--log-template)))

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
                          (apply #'majutsu-jj-colored-string
                                 (majutsu-op--log-command-args))))))
        (unless log-output
          (setq majutsu-op-log--cached-entries entries))
        entries))))

(defun majutsu-op--log-current-marker (entry)
  "Return the current-operation marker for ENTRY."
  (if (string-empty-p (or (plist-get entry :current) ""))
      " "
    (propertize "@" 'face 'font-lock-warning-face)))

(defun majutsu-op--format-log-entry-heading (entry)
  "Return the heading line for one operation log ENTRY."
  (format "%s %-12s %-8s %s"
          (majutsu-op--log-current-marker entry)
          (plist-get entry :op-id-short)
          (plist-get entry :kind)
          (plist-get entry :desc)))

(defun majutsu-op--insert-log-entry-body (entry)
  "Insert multiline operation log body for ENTRY."
  (majutsu-op--insert-field "Id" (plist-get entry :op-id))
  (majutsu-op--insert-field "User" (plist-get entry :user))
  (majutsu-op--insert-field "Workspace" (plist-get entry :workspace))
  (majutsu-op--insert-field
   "Time"
   (format "%s (%s), lasted %s"
           (plist-get entry :time)
           (plist-get entry :time-ago)
           (plist-get entry :duration)))
  (when (majutsu-op--nonempty-field-p (plist-get entry :tags))
    (majutsu-op--insert-colored-block (plist-get entry :tags))))

(defun majutsu-op-log-insert-entries ()
  "Insert operation log entries."
  (magit-insert-section (jj-op-log)
    (magit-insert-heading "Operation Log")
    (dolist (entry (majutsu-parse-op-log-entries))
      (let ((op-id (plist-get entry :op-id)))
        (magit-insert-section (jj-op op-id)
          (magit-insert-heading (majutsu-op--format-log-entry-heading entry))
          (majutsu-op--insert-log-entry-body entry))))))

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

(transient-define-argument majutsu-op-log:--limit ()
  :description "Limit"
  :class 'transient-option
  :shortarg "-n"
  :argument "--limit="
  :reader #'transient-read-number-N+)

(transient-define-argument majutsu-op-log:--reversed ()
  :description "Reverse order"
  :class 'transient-switch
  :key "-r"
  :argument "--reversed")

;;;###autoload(autoload 'majutsu-op-log-transient "majutsu-op" nil t)
(transient-define-prefix majutsu-op-log-transient ()
  "Transient for jj operation log."
  [:description "JJ Operation Log"
   ["Options"
    (majutsu-op-log:--limit)
    (majutsu-op-log:--reversed)]
   ["Actions"
    ("l" "Open log" majutsu-op-log)
    ("RET" "Open log" majutsu-op-log)
    ]])

;;;###autoload
(defun majutsu-op-log (&optional args)
  "Open the Majutsu operation log with ARGS."
  (interactive (list (majutsu-op-log-arguments)))
  (let* ((root (majutsu--toplevel-safe))
         (repo (file-name-nondirectory (directory-file-name root))))
    (majutsu-setup-buffer #'majutsu-op-log-mode nil
      :buffer (format "*majutsu-op: %s*" repo)
      :directory root
      (majutsu-op-log--args args))))

;;; op restore/revert

(defun majutsu-op-restore (operation)
  "Restore repository state to OPERATION by running jj op restore."
  (interactive (list (majutsu-op--read-operation "Restore to operation")))
  (if (not (majutsu-confirm 'op-restore
                            (format "Restore repository state to operation %s? " operation)))
      (message "Operation restore canceled")
    (when (zerop (majutsu-run-jj "op" "restore" operation))
      (majutsu-refresh))))

(defun majutsu-op-revert (operation)
  "Revert OPERATION by running jj op revert."
  (interactive (list (majutsu-op--read-operation "Revert operation")))
  (if (not (majutsu-confirm 'op-revert
                            (format "Revert operation %s? " operation)))
      (message "Operation revert canceled")
    (when (zerop (majutsu-run-jj "op" "revert" operation))
      (majutsu-refresh))))

;;; op show

(defvar-local majutsu-op-show--operation nil
  "Operation id shown in the current operation show buffer.")

(defun majutsu-op--show-output (operation template)
  "Return colored `jj op show' output for OPERATION rendered with TEMPLATE."
  (apply #'majutsu-jj-colored-string
         (append majutsu-op--read-only-global-args
                 (list "op" "show" operation "--no-op-diff" "-T" template))))

(defun majutsu-op--show-metadata (operation)
  "Return metadata plist for OPERATION."
  (or (majutsu-op--parse-show-line
       (car (split-string (majutsu-op--show-output operation majutsu-op--show-template)
                          "\n" t)))
      (user-error "Failed to parse operation metadata for %s" operation)))

(defun majutsu-op--operation-body (operation template)
  "Return colored body output for OPERATION rendered with TEMPLATE."
  (majutsu-op--show-output operation template))

(defun majutsu-op--diff-command-args (args)
  "Return jj arguments for operation diff ARGS."
  (append majutsu-op--read-only-global-args
          (list "--config" (majutsu-op--commit-summary-config-arg)
                "op" "diff" "--no-graph")
          args))

(defun majutsu-op--operation-diff-output (operation)
  "Return colored operation diff output for OPERATION."
  (apply #'majutsu-jj-colored-string
         (majutsu-op--diff-command-args (list "--operation" operation))))

(defun majutsu-op--diff-output (args)
  "Return colored operation diff output for ARGS."
  (apply #'majutsu-jj-colored-string
         (majutsu-op--diff-command-args args)))

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

(defun majutsu-op--diff-line-marker (marker)
  "Return display text for diff line MARKER."
  (propertize marker 'face (if (string= marker "+") 'diff-added 'diff-removed)))

(defun majutsu-op--diff-line-flags (value)
  "Return display flag text for parsed diff line VALUE."
  (let ((flags (delq nil (list (and (plist-get value :hidden) "hidden")
                               (and (plist-get value :conflict) "conflict")
                               (and (plist-get value :empty) "empty")))))
    (unless (null flags)
      (concat " "
              (mapconcat (lambda (flag) (format "(%s)" flag)) flags " ")))))

(defun majutsu-op--format-diff-line (value)
  "Return display text for parsed operation diff line VALUE."
  (let ((prefix (plist-get value :prefix)))
    (concat (majutsu-op--diff-line-marker (plist-get value :marker))
            " "
            (when prefix
              (concat (propertize prefix 'face 'font-lock-keyword-face) " "))
            (if (plist-get value :absent)
                (propertize "(absent)" 'face 'shadow)
              (concat (plist-get value :change-id-short-display)
                      " "
                      (plist-get value :commit-id-short-display)
                      (or (majutsu-op--diff-line-flags value) "")
                      " "
                      (plist-get value :description))))))

(defun majutsu-op--node-display-text (node)
  "Return NODE text with ANSI colors applied."
  (majutsu-jj-apply-ansi (plist-get node :text)))

(defun majutsu-op--insert-diff-node (node)
  "Insert one parsed operation diff NODE."
  (pcase (plist-get node :type)
    ('group
     (magit-insert-section (jj-op-group (list :kind (plist-get node :kind)
                                              :title (plist-get node :title)))
       (magit-insert-heading (majutsu-op--node-display-text node))
       (dolist (child (plist-get node :children))
         (majutsu-op--insert-diff-node child))))
    ('ref
     (magit-insert-section (jj-op-ref (plist-get node :name))
       (magit-insert-heading (majutsu-op--node-display-text node))
       (dolist (child (plist-get node :children))
         (majutsu-op--insert-diff-node child))))
    ('commit-line
     (magit-insert-section (jj-op-commit-line (plist-get node :value))
       (magit-insert-heading (majutsu-op--format-diff-line
                              (plist-get node :value)))))
    ('ref-line
     (magit-insert-section (jj-op-ref-line (plist-get node :value))
       (magit-insert-heading (majutsu-op--format-diff-line
                              (plist-get node :value)))))
    ('warning
     (magit-insert-section (jj-op-warning nil)
       (magit-insert-heading (propertize (majutsu-op--node-display-text node)
                                         'face 'font-lock-warning-face))))
    ('elision
     (magit-insert-section (jj-op-elision nil)
       (magit-insert-heading (propertize (string-trim-left
                                          (majutsu-op--node-display-text node))
                                         'face 'shadow))))
    (_
     (magit-insert-section (jj-op-raw-line nil)
       (magit-insert-heading (majutsu-op--node-display-text node))))))

(defun majutsu-op--insert-operation-diff-output (output)
  "Insert parsed operation diff OUTPUT."
  (let ((nodes (majutsu-op--parse-diff-output output)))
    (if nodes
        (dolist (node nodes)
          (majutsu-op--insert-diff-node node))
      (insert (propertize "No repository changes\n" 'face 'shadow)))))

(defun majutsu-op-show-refresh-buffer ()
  "Refresh the current operation show buffer."
  (interactive)
  (majutsu--assert-mode 'majutsu-op-show-mode)
  (let* ((operation (or majutsu-op-show--operation "@"))
         (metadata (majutsu-op--show-metadata operation))
         (op-id (plist-get metadata :op-id)))
    (setq majutsu-op-show--operation op-id)
    (magit-insert-section (jj-op-show op-id)
      (magit-insert-heading
        (format "Operation %s" (plist-get metadata :op-id-short)))
      (magit-insert-section (jj-op-show-metadata op-id)
        (magit-insert-heading "Metadata")
        (majutsu-op--insert-field "User" (plist-get metadata :user))
        (majutsu-op--insert-field "Workspace" (plist-get metadata :workspace))
        (majutsu-op--insert-field "Started" (plist-get metadata :start-time))
        (majutsu-op--insert-field "Ended" (plist-get metadata :end-time))
        (majutsu-op--insert-field "Duration" (plist-get metadata :duration))
        (majutsu-op--insert-field "Kind" (plist-get metadata :kind)))
      (let ((description (majutsu-op--operation-body op-id "self.description()")))
        (unless (string-empty-p (string-trim (majutsu-jj-strip-ansi description)))
          (magit-insert-section (jj-op-show-description op-id)
            (magit-insert-heading "Description")
            (majutsu-op--insert-colored-block description))))
      (let ((tags (majutsu-op--operation-body op-id "self.tags()")))
        (unless (string-empty-p (string-trim (majutsu-jj-strip-ansi tags)))
          (magit-insert-section (jj-op-show-tags op-id)
            (magit-insert-heading "Tags")
            (majutsu-op--insert-colored-block tags))))
      (magit-insert-section (jj-op-diff op-id)
        (magit-insert-heading "Repository Changes")
        (majutsu-op--insert-operation-diff-output
         (majutsu-op--operation-diff-output op-id))))))

(defun majutsu-op-show-diff-at-point ()
  "Open the ordinary commit diff for the operation diff line at point."
  (interactive)
  (if-let* ((line (majutsu-op--diff-line-at-point))
            (revset (majutsu-op--diff-line-commit-revset line)))
      (majutsu-diff-revset revset)
    (user-error "No changed commit at point")))

(defun majutsu-op-show-default-action ()
  "Run the default action for the operation show section at point."
  (interactive)
  (majutsu-op-show-diff-at-point))

(defun majutsu-op-show-evolog-at-point ()
  "Open evolog for the operation diff line at point."
  (interactive)
  (if-let* ((line (majutsu-op--diff-line-at-point))
            (revset (majutsu-op--diff-line-evolog-revset line)))
      (if (fboundp 'majutsu-evolog)
          (funcall #'majutsu-evolog revset)
        (user-error "Evolog support is not implemented yet"))
    (user-error "No changed commit at point")))

(defvar-keymap majutsu-op-show-mode-map
  :doc "Keymap for `majutsu-op-show-mode'."
  :parent majutsu-mode-map
  "RET" 'majutsu-op-show-default-action
  "d" 'majutsu-op-show-diff-at-point
  "v" 'majutsu-op-show-evolog-at-point)

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

;;; op diff

(defvar-local majutsu-op-diff--args nil
  "Arguments used for the current operation diff buffer.")

(defun majutsu-op--diff-arg-value (prefix args)
  "Return the value for PREFIX in ARGS."
  (seq-some (lambda (arg)
              (and (string-prefix-p prefix arg)
                   (substring arg (length prefix))))
            args))

(defun majutsu-op--diff-buffer-heading (args)
  "Return a heading for operation diff ARGS."
  (cond
   ((majutsu-op--diff-arg-value "--operation=" args)
    (format "Operation Diff %s"
            (majutsu-op--diff-arg-value "--operation=" args)))
   ((or (majutsu-op--diff-arg-value "--from=" args)
        (majutsu-op--diff-arg-value "--to=" args))
    (format "Operation Diff %s..%s"
            (or (majutsu-op--diff-arg-value "--from=" args) "@-")
            (or (majutsu-op--diff-arg-value "--to=" args) "@")))
   (t "Operation Diff")))

(defun majutsu-op-diff-arguments ()
  "Return operation diff arguments from the active transient, if any."
  (if (eq transient-current-command 'majutsu-op-diff-transient)
      (transient-args 'majutsu-op-diff-transient)
    (list (concat "--operation="
                  (majutsu-op--read-operation "Diff operation")))))

(defun majutsu-op-diff-refresh-buffer ()
  "Refresh the current operation diff buffer."
  (interactive)
  (majutsu--assert-mode 'majutsu-op-diff-mode)
  (let ((args (or majutsu-op-diff--args '())))
    (magit-insert-section (jj-op-diff-buffer args)
      (magit-insert-heading (majutsu-op--diff-buffer-heading args))
      (majutsu-op--insert-operation-diff-output
       (majutsu-op--diff-output args)))))

(defvar-keymap majutsu-op-diff-mode-map
  :doc "Keymap for `majutsu-op-diff-mode'."
  :parent majutsu-op-show-mode-map)

(define-derived-mode majutsu-op-diff-mode majutsu-mode "Majutsu Op Diff"
  "Major mode for comparing jj operations."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

(defun majutsu-op-diff--buffer-name ()
  "Return buffer name for operation diff."
  (let* ((root (majutsu--toplevel-safe))
         (repo (file-name-nondirectory (directory-file-name root))))
    (format "*majutsu-op-diff: %s*" repo)))

;;;###autoload
(defun majutsu-op-diff (&optional args)
  "Open an operation diff buffer with ARGS."
  (interactive (list (majutsu-op-diff-arguments)))
  (let ((root (majutsu--toplevel-safe)))
    (majutsu-setup-buffer #'majutsu-op-diff-mode nil
      :buffer (majutsu-op-diff--buffer-name)
      :directory root
      (majutsu-op-diff--args args))))

(transient-define-argument majutsu-op-diff:--operation ()
  :description "Operation"
  :class 'transient-option
  :key "-o"
  :argument "--operation="
  :prompt "Operation: "
  :reader #'majutsu-op--transient-read-operation)

(transient-define-argument majutsu-op-diff:--from ()
  :description "From operation"
  :class 'transient-option
  :key "-f"
  :argument "--from="
  :prompt "From operation: "
  :reader #'majutsu-op--transient-read-operation)

(transient-define-argument majutsu-op-diff:--to ()
  :description "To operation"
  :class 'transient-option
  :key "-t"
  :argument "--to="
  :prompt "To operation: "
  :reader #'majutsu-op--transient-read-operation)

;;;###autoload(autoload 'majutsu-op-diff-transient "majutsu-op" nil t)
(transient-define-prefix majutsu-op-diff-transient ()
  "Transient for jj operation diff."
  :incompatible '(("--operation=" "--from=")
                  ("--operation=" "--to="))
  [:description "JJ Operation Diff"
   ["Selection"
    (majutsu-op-diff:--operation)
    (majutsu-op-diff:--from)
    (majutsu-op-diff:--to)]
   ["Actions"
    ("d" "Open diff" majutsu-op-diff)
    ("RET" "Open diff" majutsu-op-diff)
    ]])

;;; _
(provide 'majutsu-op)
;;; majutsu-op.el ends here
