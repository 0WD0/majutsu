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
(require 'majutsu-row)
(require 'majutsu-row)
(require 'majutsu-selection)
(require 'seq)
(require 'subr-x)
(require 'transient)

(declare-function majutsu-diff-revset "majutsu-diff" (revset &optional args range filesets))
(declare-function majutsu-edit-changeset "majutsu-edit" (&optional arg))
(declare-function majutsu-evolog "majutsu-evolog" (revset &optional args))
(declare-function majutsu-jj-buffer-string "majutsu-jj" (&rest args))
(declare-function majutsu-jj-wash "majutsu-jj" (washer keep-error &rest args))

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
  "Return FIELD without text properties or surrounding whitespace."
  (string-trim (substring-no-properties (or field ""))))

(defun majutsu-op--split-record (line expected)
  "Split LINE into EXPECTED fields, or return nil when malformed."
  (let ((fields (majutsu--split-fields line majutsu-op--field-separator)))
    (and (= (length fields) expected) fields)))

(defun majutsu-op--parse-show-line (line)
  "Parse one operation metadata LINE into a plist."
  (when-let* ((fields (majutsu-op--split-record line 9)))
    (pcase-let ((`(,op-id ,op-id-short ,user ,workspace
                   ,start-time ,end-time ,duration ,kind ,description)
                 fields))
      (list :op-id       op-id
            :op-id-short op-id-short
            :user        user
            :workspace   workspace
            :start-time  start-time
            :end-time    end-time
            :duration    duration
            :kind        kind
            :desc        description))))

(defun majutsu-op--nonempty-field-p (field)
  "Return non-nil when FIELD is present and non-empty."
  (and field (not (string-empty-p (string-trim (substring-no-properties field))))))

(defun majutsu-op--parse-commit-summary (plain-summary &optional display-summary)
  "Parse a structured commit PLAIN-SUMMARY and optional DISPLAY-SUMMARY."
  (when-let* ((fields (majutsu-op--split-record plain-summary 8)))
    (pcase-let* ((`(,change-id ,change-id-short ,commit-id ,commit-id-short
                    ,hidden ,conflict ,empty ,_description)
                  fields)
                 (display-fields (or (majutsu-op--split-record
                                      (or display-summary plain-summary) 8)
                                     fields))
                 (`(,_ ,change-id-short-display ,_ ,commit-id-short-display
                    ,_ ,_ ,_ ,description-display)
                  display-fields))
      (list :change-id               change-id
            :change-id-short         change-id-short
            :change-id-short-display change-id-short-display
            :commit-id               commit-id
            :commit-id-short         commit-id-short
            :commit-id-short-display commit-id-short-display
            :hidden                  (majutsu-op--nonempty-field-p hidden)
            :conflict                (majutsu-op--nonempty-field-p conflict)
            :empty                   (majutsu-op--nonempty-field-p empty)
            :description             description-display))))

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
                   (display-summary (substring colored-line summary-start))
                   (base (list :marker marker
                               :prefix prefix
                               :group-kind group-kind
                               :ref-name ref-name)))
        (if (string= target "(absent)")
            (list :type line-type
                  :text colored-line
                  :value (append base '(:absent t)))
          (if-let* ((summary (majutsu-op--parse-commit-summary
                              target display-summary)))
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
        (let* ((plain-line (substring-no-properties colored-line))
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

(defclass majutsu-op-option (majutsu-selection-option) ())

(defclass majutsu-op-toggle-option (majutsu-selection-toggle-option) ())

(defclass majutsu-op-target-prefix (transient-prefix) ())

(defun majutsu-op--section-in-lineage (types &optional section)
  "Return the first ancestor SECTION whose type is in TYPES."
  (let ((section (or section (magit-current-section)))
        (types (ensure-list types)))
    (while (and section
                (not (memq (oref section type) types)))
      (setq section (oref section parent)))
    section))

(defun majutsu-op--section-value-in-lineage (types &optional section)
  "Return the first ancestor value for section TYPES."
  (when-let* ((section (majutsu-op--section-in-lineage types section)))
    (oref section value)))

(defun majutsu-op--operation-at-point ()
  "Return the enclosing operation id at point, or nil."
  (majutsu-op--section-value-in-lineage '(jj-op jj-op-show)))

(defun majutsu-op--diff-line-at-point ()
  "Return the parsed operation diff line value at point, or nil."
  (majutsu-op--section-value-in-lineage '(jj-op-commit-line jj-op-ref-line)))

(defun majutsu-op--selection-locate (operation)
  "Locate OPERATION in the current operation buffer."
  (or (majutsu-selection-find-section operation 'jj-op)
      (majutsu-selection-find-section operation 'jj-op-show)))

(defun majutsu-op--selection-targets ()
  "Return operation ids selected from point or region."
  (or (magit-region-values 'jj-op t)
      (magit-region-values 'jj-op-show t)
      (when-let* ((operation (majutsu-op--operation-at-point)))
        (list operation))))

(cl-defmethod transient-init-value ((obj majutsu-op-target-prefix))
  (oset obj value
        (when-let* ((operation (majutsu-op--operation-at-point)))
          (list (concat "--operation=" operation)))))

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

(transient-define-argument majutsu-op-arg:--operation ()
  :description "Operation"
  :class 'majutsu-op-option
  :selection-label "[OP]"
  :selection-face '(:background "goldenrod" :foreground "black")
  :locate-fn #'majutsu-op--selection-locate
  :targets-fn #'majutsu-op--selection-targets
  :key "-o"
  :argument "--operation="
  :prompt "Operation: "
  :reader #'majutsu-op--transient-read-operation)

(transient-define-argument majutsu-op-arg:operation ()
  :description "Operation (toggle at point)"
  :class 'majutsu-op-toggle-option
  :key "o"
  :argument "--operation=")

(transient-define-argument majutsu-op-arg:--from ()
  :description "From operation"
  :class 'majutsu-op-option
  :selection-label "[FROM]"
  :selection-face '(:background "dark orange" :foreground "black")
  :locate-fn #'majutsu-op--selection-locate
  :targets-fn #'majutsu-op--selection-targets
  :shortarg "-f"
  :argument "--from="
  :prompt "From operation: "
  :reader #'majutsu-op--transient-read-operation)

(transient-define-argument majutsu-op-arg:from ()
  :description "From operation (toggle at point)"
  :class 'majutsu-op-toggle-option
  :key "f"
  :argument "--from=")

(transient-define-argument majutsu-op-arg:--to ()
  :description "To operation"
  :class 'majutsu-op-option
  :selection-label "[TO]"
  :selection-face '(:background "dark cyan" :foreground "white")
  :locate-fn #'majutsu-op--selection-locate
  :targets-fn #'majutsu-op--selection-targets
  :shortarg "-t"
  :argument "--to="
  :prompt "To operation: "
  :reader #'majutsu-op--transient-read-operation)

(transient-define-argument majutsu-op-arg:to ()
  :description "To operation (toggle at point)"
  :class 'majutsu-op-toggle-option
  :key "t"
  :argument "--to=")

;;; op transient

;;;###autoload(autoload 'majutsu-op-transient "majutsu-op" nil t)
(transient-define-prefix majutsu-op-transient ()
  "Transient for jj operation commands."
  :man-page "jj-operation"
  :transient-non-suffix t
  [["History"
    ("l" "Log..." majutsu-op-log-transient)
    ("s" "Show" majutsu-op-show)
    ("d" "Diff..." majutsu-op-diff-transient)]
   ["State"
    ("u" "Undo" majutsu-undo)
    ("r" "Redo" majutsu-redo)
    ("R" "Restore..." majutsu-op-restore-transient)
    ("V" "Revert..." majutsu-op-revert-transient)]])

;;; op log

(defvar-local majutsu-op-log--args nil
  "Arguments used for the current operation log buffer.")

(defvar-local majutsu-op-log--cached-entries nil
  "Cached operation log entries.")

(defconst majutsu-op-log--field-default-modules
  '((current-marker . heading)
    (op-id-short . heading)
    (kind . heading)
    (description . heading)
    (id-line . body)
    (user-line . body)
    (workspace-line . body)
    (time-line . body)
    (tags . body)
    (op-id . metadata)
    (user . metadata)
    (workspace . metadata)
    (time . metadata)
    (time-ago . metadata)
    (duration . metadata))
  "Default row module placement for operation log fields.")

(defcustom majutsu-op-log-columns
  '((:field current-marker :module heading :face t)
    (:field op-id-short :module heading :face t)
    (:field kind :module heading :face t)
    (:field description :module heading :face t)
    (:field id-line :module body :face t)
    (:field user-line :module body :face t)
    (:field workspace-line :module body :face t)
    (:field time-line :module body :face t)
    (:field tags :module body :face t)
    (:field op-id :module metadata :face nil)
    (:field user :module metadata :face nil)
    (:field workspace :module metadata :face nil)
    (:field time :module metadata :face nil)
    (:field time-ago :module metadata :face nil)
    (:field duration :module metadata :face nil))
  "Field specification controlling operation log template and rendering.

Each element is a plist with at least `:field'. Supported keys are the same as
other row views: `:field', `:module', `:face', and `:post'. Hidden
metadata fields should keep full operation ids available for commands and copy
operations."
  :type '(repeat (plist :options (:field :module :face :post)))
  :group 'majutsu)

(defvar majutsu-op-log--compiled-template-cache nil
  "Cached compiled operation log row template metadata.")

(defun majutsu-op-log--invalidate-template-cache (&rest _)
  "Invalidate cached operation log row template metadata."
  (setq majutsu-op-log--compiled-template-cache nil))

(when (fboundp 'add-variable-watcher)
  (add-variable-watcher 'majutsu-op-log-columns
                        #'majutsu-op-log--invalidate-template-cache))

(defun majutsu-op-log--column-template (field)
  "Return majutsu-template form for operation log FIELD."
  (pcase field
    ('current-marker
     '[:if [:current_operation]
          [:label "current_operation" "@"]
        ""])
    ('op-id-short
     '[:label "id short" [:id :short]])
    ('kind
     '[:if [:snapshot]
          [:label "snapshot" "snapshot"]
        "op"])
    ('description
     '[:label "description first_line"
       [:method [:description] :first_line]])
    ('id-line
     '[:concat "Id: " [:id]])
    ('user-line
     '[:concat "User: " [:label "user" [:user]]])
    ('workspace-line
     '[:concat "Workspace: " [:label "workspace_name" [:workspace_name]]])
    ('time-line
     '[:concat "Time: "
       [:label "time end"
               [:method [:time :end]
                :format "%Y-%m-%d %H:%M:%S"]]
       " ("
       [:label "time end ago" [:method [:time :end] :ago]]
       "), lasted "
       [:label "time duration" [:method [:time] :duration]]])
    ('tags
     '[:label "tags first_line" [:method [:tags] :first_line]])
    ('op-id '[:id])
    ('user '[:label "user" [:user]])
    ('workspace '[:label "workspace_name" [:workspace_name]])
    ('time '[:label "time end"
             [:method [:time :end] :format "%Y-%m-%d %H:%M:%S"]])
    ('time-ago '[:label "time end ago" [:method [:time :end] :ago]])
    ('duration '[:label "time duration" [:method [:time] :duration]])
    (_ (user-error "Unknown operation log field %S" field))))

(defun majutsu-op-log--record-field (entry field value)
  "Record canonical operation log FIELD VALUE onto ENTRY."
  (pcase field
    ('current-marker
     (setq entry (plist-put entry :current
                            (majutsu-op--machine-field value))))
    ('op-id-short
     (setq entry (plist-put entry :op-id-short value)))
    ('kind
     (setq entry (plist-put entry :kind
                            (majutsu-op--machine-field value))))
    ('description
     (setq entry (plist-put entry :desc value)))
    ('op-id
     (setq entry (plist-put entry :op-id
                            (majutsu-op--machine-field value))))
    ('user
     (setq entry (plist-put entry :user value)))
    ('workspace
     (setq entry (plist-put entry :workspace value)))
    ('time
     (setq entry (plist-put entry :time value)))
    ('time-ago
     (setq entry (plist-put entry :time-ago value)))
    ('duration
     (setq entry (plist-put entry :duration value)))
    ('tags
     (setq entry (plist-put entry :tags value))))
  (majutsu-row-record-canonical-field entry field value))

(defun majutsu-op-log--entry-id (entry)
  "Return stable section id string from operation log ENTRY."
  (or (let ((op-id (plist-get entry :op-id)))
        (and (stringp op-id)
             (not (string-empty-p (string-trim op-id)))
             (substring-no-properties op-id)))
      "unknown"))

(defun majutsu-op-log--row-profile ()
  "Return the row profile for operation log entries."
  (list :name 'op-log
        :self-type 'Operation
        :columns-var 'majutsu-op-log-columns
        :default-modules majutsu-op-log--field-default-modules
        :template-function 'majutsu-op-log--column-template
        :record-field-function 'majutsu-op-log--record-field
        :entry-id-function 'majutsu-op-log--entry-id
        :section-class 'jj-op
        :section-value-function 'majutsu-op-log--entry-id
        :section-hide nil
        :tail-align nil
        :compat-property-prefix 'majutsu-op-log))

(defun majutsu-op-log--compile-columns (&optional columns)
  "Compile operation log COLUMNS into row metadata."
  (majutsu-row-compile
   (majutsu-op-log--row-profile)
   (or columns majutsu-op-log-columns)))

(defun majutsu-op-log--ensure-template ()
  "Return cached compiled operation log template metadata."
  (or majutsu-op-log--compiled-template-cache
      (setq majutsu-op-log--compiled-template-cache
            (majutsu-op-log--compile-columns majutsu-op-log-columns))))

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
          (list "-T" (plist-get (majutsu-op-log--ensure-template)
                                :template))))

(defun majutsu-parse-op-log-entries (&optional buf log-output)
  "Parse jj operation log output.

When LOG-OUTPUT is nil, run jj in BUF or the current buffer and cache the
result."
  (with-current-buffer (or buf (current-buffer))
    (if (and majutsu-op-log--cached-entries (not log-output))
        majutsu-op-log--cached-entries
      (let* ((cmd-args (unless log-output (majutsu-op--log-command-args)))
             (entries
              (with-temp-buffer
                (insert (or log-output
                            (apply #'majutsu-jj-buffer-string cmd-args)))
                (goto-char (point-min))
                (majutsu-row-parse-buffer
                 (majutsu-op-log--ensure-template)))))
        (unless log-output
          (setq majutsu-op-log--cached-entries entries)
          (majutsu-row-set-buffer-data
           (majutsu-op-log--ensure-template)
           entries))
        entries))))

(defun majutsu-op--wash-log-output (_args)
  "Wash raw `jj op log` output in the current narrowed region."
  (let ((compiled (majutsu-op-log--ensure-template)))
    (setq majutsu-op-log--cached-entries
          (majutsu-row-wash-buffer compiled))
    (majutsu-row-set-buffer-data
     compiled
     majutsu-op-log--cached-entries)))

(defun majutsu-op-log-insert-entries ()
  "Insert operation log entries."
  (magit-insert-section (jj-op-log)
    (magit-insert-heading "Operation Log")
    (setq majutsu-op-log--cached-entries nil)
    (majutsu-row-clear-buffer-data)
    (apply #'majutsu-jj-wash
           #'majutsu-op--wash-log-output
           'wash-anyway
           (majutsu-op--log-command-args))))

(defun majutsu-op-log-render ()
  "Render the op log buffer."
  (magit-insert-section (oplog)
    (majutsu-op-log-insert-entries))
  (majutsu-selection-render))

(defun majutsu-op-log-refresh-buffer ()
  "Refresh the op log buffer."
  (interactive)
  (majutsu--assert-mode 'majutsu-op-log-mode)
  (setq majutsu-op-log--cached-entries nil)
  (majutsu-row-clear-buffer-data)
  (majutsu-op-log-render))

(defun majutsu-op-log-show-at-point ()
  "Show the operation at point."
  (interactive)
  (if-let* ((op-id (majutsu-op--operation-at-point)))
      (majutsu-op-show op-id)
    (user-error "No operation at point")))

(defun majutsu-op-log--filter-buffer-substring (beg end &optional delete)
  "Filter copied operation log text between BEG and END."
  (majutsu-row-filter-buffer-substring
   beg end delete (majutsu-op-log--ensure-template)))

;;;###autoload
(defun majutsu-op-log-copy-operation-id ()
  "Copy the current operation log entry id."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-op-log--ensure-template))
           (entry (or (majutsu-row-entry-at-point
                       compiled majutsu-op-log--cached-entries)
                      (user-error "No operation at point"))))
      (majutsu-row-entry-field-value-to-kill
       entry 'op-id))))

;;;###autoload(autoload 'majutsu-op-log-copy-transient "majutsu-op" nil t)
(majutsu-row-define-copy-transient
 majutsu-op-log-copy-transient
 "Transient for semantic copy commands in `majutsu-op-log-mode'."
 ("o" "Operation id" majutsu-op-log-copy-operation-id))

(defvar-keymap majutsu-op-log-mode-map
  :doc "Keymap for `majutsu-op-log-mode'."
  :parent majutsu-mode-map
  "RET" 'majutsu-op-log-show-at-point
  "d" 'majutsu-op-log-show-at-point)

(define-derived-mode majutsu-op-log-mode majutsu-mode "Majutsu Op Log"
  "Major mode for viewing jj operation log."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer)
  (setq-local filter-buffer-substring-function
              #'majutsu-op-log--filter-buffer-substring)
  (setq-local majutsu-row-buffer-compiled
              (majutsu-op-log--ensure-template))
  (add-hook 'kill-buffer-hook #'majutsu-selection-session-end-if-owner nil t))

(put 'majutsu-op-log-mode 'majutsu-op-log-default-arguments
     '("--limit=64"))

(defclass majutsu-op-log-prefix (transient-prefix)
  ((major-mode :initform 'majutsu-op-log-mode)))

(cl-defmethod transient-init-value ((obj majutsu-op-log-prefix))
  (oset obj value
        (majutsu-transient-default-value
         'majutsu-op-log
         (oref obj major-mode)
         'majutsu-op-log-current-arguments
         'majutsu-op-log-default-arguments)))

(cl-defmethod transient-set-value ((obj majutsu-op-log-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode))
         (args (transient-args (oref obj command))))
    (put mode 'majutsu-op-log-current-arguments args)
    (transient--history-push obj)
    (when (eq major-mode mode)
      (setq-local majutsu-op-log--args args))))

(cl-defmethod transient-save-value ((obj majutsu-op-log-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode))
         (args (transient-args (oref obj command))))
    (put mode 'majutsu-op-log-current-arguments args)
    (setf (alist-get (majutsu-transient-global-default-key
                      'majutsu-op-log mode)
                     transient-values)
          args)
    (transient-save-values)
    (transient--history-push obj)
    (when (eq major-mode mode)
      (setq-local majutsu-op-log--args args))))

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
  :man-page "jj-operation-log"
  :transient-non-suffix t
  :class 'majutsu-op-log-prefix
  [:description
   "JJ Operation Log"
   ["Options"
    (majutsu-op-log:--limit)
    (majutsu-op-log:--reversed)]
   ["Actions"
    ("l" "Open log" majutsu-op-log)
    ("RET" "Open log" majutsu-op-log)
    ("s" "Save as default" transient-save-and-exit)
    ]])

;;;###autoload
(defun majutsu-op-log (&optional args)
  "Open the Majutsu operation log with ARGS."
  (interactive (list (or (majutsu-op-log-arguments)
                         (get 'majutsu-op-log-mode
                              'majutsu-op-log-default-arguments))))
  (let* ((args (or args (get 'majutsu-op-log-mode
                             'majutsu-op-log-default-arguments)))
         (root (majutsu--toplevel-safe))
         (repo (file-name-nondirectory (directory-file-name root))))
    (majutsu-setup-buffer #'majutsu-op-log-mode nil
      :buffer (format "*majutsu-op: %s*" repo)
      :directory root
      (majutsu-op-log--args args))))

;;; op restore/revert

(defun majutsu-op--extract-target-operation (args)
  "Return (OPERATION . ARGS) after stripping pseudo --operation= from ARGS."
  (let (operation rest)
    (dolist (arg args)
      (if (string-prefix-p "--operation=" arg)
          (setq operation (substring arg (length "--operation=")))
        (push arg rest)))
    (cons operation (nreverse rest))))

(defun majutsu-op--run-confirmed (action prompt command)
  "Run COMMAND after confirming ACTION with PROMPT."
  (if (not (majutsu-confirm action prompt))
      (message "%s canceled" (capitalize (symbol-name action)))
    (when (zerop (apply #'majutsu-run-jj command))
      (majutsu-refresh))))

(defun majutsu-op-restore (operation)
  "Restore repository state to OPERATION by running jj op restore."
  (interactive (list (majutsu-op--read-operation "Restore to operation")))
  (majutsu-op--run-confirmed
   'op-restore
   (format "Restore repository state to operation %s? " operation)
   (list "op" "restore" operation)))

(defun majutsu-op-revert (operation)
  "Revert OPERATION by running jj op revert."
  (interactive (list (majutsu-op--read-operation "Revert operation")))
  (majutsu-op--run-confirmed
   'op-revert
   (format "Revert operation %s? " operation)
   (list "op" "revert" operation)))

(defun majutsu-op-restore-execute (args)
  "Execute jj op restore with transient ARGS."
  (interactive (list (transient-args 'majutsu-op-restore-transient)))
  (pcase-let* ((`(,operation . ,rest) (majutsu-op--extract-target-operation args)))
    (unless operation
      (user-error "Please select an operation first"))
    (majutsu-op--run-confirmed
     'op-restore
     (format "Restore repository state to operation %s? " operation)
     (append '("op" "restore") rest (list operation)))))

(defun majutsu-op-revert-execute (args)
  "Execute jj op revert with transient ARGS."
  (interactive (list (transient-args 'majutsu-op-revert-transient)))
  (pcase-let* ((`(,operation . ,rest) (majutsu-op--extract-target-operation args)))
    (unless operation
      (user-error "Please select an operation first"))
    (majutsu-op--run-confirmed
     'op-revert
     (format "Revert operation %s? " operation)
     (append '("op" "revert") rest (list operation)))))

;;;###autoload(autoload 'majutsu-op-restore-transient "majutsu-op" nil t)
(transient-define-prefix majutsu-op-restore-transient ()
  "Transient for jj operation restore."
  :man-page "jj-operation-restore"
  :class 'majutsu-op-target-prefix
  :transient-non-suffix t
  :description "JJ Operation Restore"
  [["Selection"
    (majutsu-op-arg:--operation)
    (majutsu-op-arg:operation)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["What"
    ("-r" "Repo state" "--what=repo")
    ("-t" "Remote-tracking" "--what=remote-tracking")]
   ["Actions"
    ("R" "Restore" majutsu-op-restore-execute)
    ("RET" "Restore" majutsu-op-restore-execute)]]
  (interactive)
  (transient-setup 'majutsu-op-restore-transient nil nil
                   :scope (majutsu-selection-session-begin)))

;;;###autoload(autoload 'majutsu-op-revert-transient "majutsu-op" nil t)
(transient-define-prefix majutsu-op-revert-transient ()
  "Transient for jj operation revert."
  :man-page "jj-operation-revert"
  :class 'majutsu-op-target-prefix
  :transient-non-suffix t
  :description "JJ Operation Revert"
  [["Selection"
    (majutsu-op-arg:--operation)
    (majutsu-op-arg:operation)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["What"
    ("-r" "Repo state" "--what=repo")
    ("-t" "Remote-tracking" "--what=remote-tracking")]
   ["Actions"
    ("V" "Revert" majutsu-op-revert-execute)
    ("RET" "Revert" majutsu-op-revert-execute)]]
  (interactive)
  (transient-setup 'majutsu-op-revert-transient nil nil
                   :scope (majutsu-selection-session-begin)))

;;; op show

(defvar-local majutsu-op-show--operation nil
  "Operation id shown in the current operation show buffer.")

(defun majutsu-op--show-output (operation template)
  "Return plain `jj op show' output for OPERATION rendered with TEMPLATE."
  (apply #'majutsu-jj-buffer-string
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

(defun majutsu-op--insert-field (label value)
  "Insert metadata LABEL and VALUE."
  (insert (propertize label 'face 'font-lock-keyword-face) ": " value "\n"))

(defun majutsu-op--insert-colored-block (string)
  "Insert STRING as a body block, preserving text properties."
  (let ((text (or string "")))
    (unless (string-empty-p (string-trim (substring-no-properties text)))
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
  "Return NODE text for display, preserving text properties."
  (plist-get node :text))

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
     (let ((value (plist-get node :value)))
       (magit-insert-section (jj-op-commit-line value)
         (if-let* ((commit-id (plist-get value :commit-id)))
             (magit-insert-section (jj-commit commit-id)
               (magit-insert-heading (majutsu-op--format-diff-line value)))
           (magit-insert-heading (majutsu-op--format-diff-line value))))))
    ('ref-line
     (let ((value (plist-get node :value)))
       (magit-insert-section (jj-op-ref-line value)
         (if-let* ((commit-id (and (not (plist-get value :absent))
                                   (plist-get value :commit-id))))
             (magit-insert-section (jj-commit commit-id)
               (magit-insert-heading (majutsu-op--format-diff-line value)))
           (magit-insert-heading (majutsu-op--format-diff-line value))))))
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

(defun majutsu-op--diff-line-string ()
  "Return the current diff line, preserving text properties."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun majutsu-op--delete-line ()
  "Delete current line, including trailing newline if present."
  (delete-region (line-beginning-position)
                 (min (point-max) (1+ (line-end-position)))))

(defun majutsu-op--diff-group-boundary-p ()
  "Return non-nil when point starts a new op-diff group or separator."
  (or (eobp)
      (let* ((line (majutsu-op--diff-line-string))
             (plain (substring-no-properties line))
             (trimmed (string-trim plain)))
        (or (string-empty-p trimmed)
            (majutsu-op--diff-operation-header-p plain)
            (majutsu-op--diff-group-kind plain)))))

(defun majutsu-op--diff-ref-boundary-p (group-kind)
  "Return non-nil when point leaves the current ref block in GROUP-KIND."
  (or (majutsu-op--diff-group-boundary-p)
      (let* ((line (majutsu-op--diff-line-string))
             (plain (substring-no-properties line)))
        (majutsu-op--diff-ref-header-p plain group-kind))))

(defun majutsu-op--wash-diff-line (group-kind &optional ref-name)
  "Wash one operation diff line in GROUP-KIND under REF-NAME."
  (let* ((colored-line (majutsu-op--diff-line-string))
         (plain-line (substring-no-properties colored-line))
         (trimmed (string-trim plain-line)))
    (cond
     ((string-empty-p trimmed)
      (majutsu-op--delete-line))
     ((majutsu-op--diff-warning-line-p plain-line)
      (majutsu-op--delete-line)
      (majutsu-op--insert-diff-node (list :type 'warning :text colored-line)))
     ((majutsu-op--diff-elision-line-p plain-line)
      (majutsu-op--delete-line)
      (majutsu-op--insert-diff-node (list :type 'elision :text colored-line)))
     ((majutsu-op--parse-diff-marker-line
       plain-line colored-line group-kind ref-name)
      (let ((node (majutsu-op--parse-diff-marker-line
                   plain-line colored-line group-kind ref-name)))
        (majutsu-op--delete-line)
        (majutsu-op--insert-diff-node node)))
     (t
      (majutsu-op--delete-line)
      (majutsu-op--insert-diff-node (list :type 'raw-line :text colored-line))))))

(defun majutsu-op--wash-diff-ref (group-kind)
  "Wash one named ref block in ref GROUP-KIND."
  (let* ((line (majutsu-op--diff-line-string))
         (plain (substring-no-properties line))
         (name (string-remove-suffix ":" (string-trim plain))))
    (majutsu-op--delete-line)
    (magit-insert-section (jj-op-ref name)
      (magit-insert-heading line)
      (while (and (not (eobp))
                  (not (majutsu-op--diff-ref-boundary-p group-kind)))
        (majutsu-op--wash-diff-line group-kind name)))))

(defun majutsu-op--wash-diff-group ()
  "Wash one top-level operation diff group at point."
  (let* ((line (majutsu-op--diff-line-string))
         (plain (substring-no-properties line))
         (title (string-trim plain))
         (kind (majutsu-op--diff-group-kind plain)))
    (majutsu-op--delete-line)
    (magit-insert-section (jj-op-group (list :kind kind :title title))
      (magit-insert-heading line)
      (while (and (not (eobp))
                  (not (majutsu-op--diff-group-boundary-p)))
        (if (and (majutsu-op--diff-ref-group-p kind)
                 (let* ((next-line (majutsu-op--diff-line-string))
                        (next-plain (substring-no-properties next-line)))
                   (majutsu-op--diff-ref-header-p next-plain kind)))
            (majutsu-op--wash-diff-ref kind)
          (majutsu-op--wash-diff-line kind))))))

(defun majutsu-op--wash-diff-output (_args)
  "Wash raw `jj op diff` output in the current narrowed region."
  (goto-char (point-min))
  (while (not (eobp))
    (let* ((line (majutsu-op--diff-line-string))
           (plain (substring-no-properties line))
           (trimmed (string-trim plain)))
      (cond
       ((string-empty-p trimmed)
        (majutsu-op--delete-line))
       ((majutsu-op--diff-operation-header-p plain)
        (majutsu-op--delete-line))
       ((majutsu-op--diff-group-kind plain)
        (majutsu-op--wash-diff-group))
       (t
        (majutsu-op--wash-diff-line nil)))))
  (when (= (point-min) (point-max))
    (insert (propertize "No repository changes\n" 'face 'shadow))))

(defun majutsu-op--insert-operation-diff (args)
  "Insert washed operation diff for ARGS."
  (apply #'majutsu-jj-wash
         #'majutsu-op--wash-diff-output
         'wash-anyway
         (majutsu-op--diff-command-args args)))

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
        (unless (string-empty-p (string-trim (substring-no-properties description)))
          (magit-insert-section (jj-op-show-description op-id)
            (magit-insert-heading "Description")
            (majutsu-op--insert-colored-block description))))
      (let ((tags (majutsu-op--operation-body op-id "self.tags()")))
        (unless (string-empty-p (string-trim (substring-no-properties tags)))
          (magit-insert-section (jj-op-show-tags op-id)
            (magit-insert-heading "Tags")
            (majutsu-op--insert-colored-block tags))))
      (magit-insert-section (jj-op-diff op-id)
        (magit-insert-heading "Repository Changes")
        (majutsu-op--insert-operation-diff (list "--operation" op-id))))
    (majutsu-selection-render)))

(defun majutsu-op-show-diff-at-point ()
  "Open the ordinary commit diff for the changed commit at point."
  (interactive)
  (if-let* ((commit-id (magit-section-value-if 'jj-commit)))
      (majutsu-diff-revset (format "commit_id(%s)" commit-id))
    (if-let* ((line (majutsu-op--diff-line-at-point))
              (revset (majutsu-op--diff-line-commit-revset line)))
        (majutsu-diff-revset revset)
      (user-error "No changed commit at point"))))

(defun majutsu-op-show-default-action ()
  "Run the default action for the operation show section at point."
  (interactive)
  (if (magit-section-value-if 'jj-commit)
      (majutsu-edit-changeset)
    (majutsu-op-show-diff-at-point)))

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
  "v" 'majutsu-op-show-evolog-at-point)

(define-derived-mode majutsu-op-show-mode majutsu-mode "Majutsu Op Show"
  "Major mode for viewing one jj operation."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer)
  (add-hook 'kill-buffer-hook #'majutsu-selection-session-end-if-owner nil t))

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
      (majutsu-op--insert-operation-diff args)))
  (majutsu-selection-render))

(defvar-keymap majutsu-op-diff-mode-map
  :doc "Keymap for `majutsu-op-diff-mode'."
  :parent majutsu-op-show-mode-map)

(define-derived-mode majutsu-op-diff-mode majutsu-mode "Majutsu Op Diff"
  "Major mode for comparing jj operations."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer)
  (add-hook 'kill-buffer-hook #'majutsu-selection-session-end-if-owner nil t))

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

;;;###autoload(autoload 'majutsu-op-diff-transient "majutsu-op" nil t)
(transient-define-prefix majutsu-op-diff-transient ()
  "Transient for jj operation diff."
  :man-page "jj-operation-diff"
  :class 'majutsu-op-target-prefix
  :transient-non-suffix t
  :incompatible '(("--operation=" "--from=")
                  ("--operation=" "--to="))
  [:description
   "JJ Operation Diff"
   ["Selection"
    (majutsu-op-arg:--operation)
    (majutsu-op-arg:operation)
    (majutsu-op-arg:--from)
    (majutsu-op-arg:from)
    (majutsu-op-arg:--to)
    (majutsu-op-arg:to)
    ("c" "Clear selections" majutsu-selection-clear :transient t)]
   ["Actions"
    ("d" "Open diff" majutsu-op-diff)
    ("RET" "Open diff" majutsu-op-diff)
    ]]
  (interactive)
  (transient-setup 'majutsu-op-diff-transient nil nil
                   :scope (majutsu-selection-session-begin)))

;;; _
(provide 'majutsu-op)
;;; majutsu-op.el ends here
