;;; majutsu-log.el --- Log view for majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025-2026 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library builds the Majutsu log buffer: compiles jj templates,
;; parses log output, renders sections, and handles navigation.

;;; Code:

(require 'majutsu)

(defcustom majutsu-log-field-faces
  '((bookmarks . magit-branch-local)
    (tags . magit-tag)
    (working-copies . magit-branch-remote)
    (author . magit-log-author)
    (timestamp . magit-log-date)
    (flags . font-lock-comment-face))
  "Alist mapping log fields to face behavior.

Each entry is (FIELD . SPEC).  SPEC can be:

- t    Preserve existing font-lock-face properties produced
       by JJ and `ansi-color-apply-text-property-face'.
- nil  Remove face properties from that field.
- FACE Apply FACE to that field (overriding existing faces).

When a field is not present in this alist, it defaults to t."
  :type '(alist :tag "Field face behavior"
          :key-type (symbol :tag "Field")
          :value-type (choice (const :tag "Preserve existing faces" t)
                              (const :tag "No faces" nil)
                              (face :tag "Use this face")))
  :group 'majutsu)

;;; Section Keymaps

(defvar-keymap majutsu-commit-section-map
  :doc "Keymap for `jj-commit' sections."
  "<remap> <majutsu-visit-thing>" #'majutsu-edit-changeset)

;;; Log State

(defun majutsu-log--get-value (mode &optional use-buffer-args)
  "Get log arguments for MODE.

Returns (args revsets filesets) triple.  USE-BUFFER-ARGS follows
`majutsu-prefix-use-buffer-arguments' or
`majutsu-direct-use-buffer-arguments'."
  (setq use-buffer-args
        (pcase-exhaustive use-buffer-args
          ('prefix majutsu-prefix-use-buffer-arguments)
          ('direct majutsu-direct-use-buffer-arguments)
          ('nil majutsu-direct-use-buffer-arguments)
          ((or 'always 'selected 'current 'never)
           use-buffer-args)))
  (cond
   ((and (memq use-buffer-args '(always selected current))
         (eq major-mode mode))
    (list majutsu-buffer-log-args
          majutsu-buffer-log-revsets
          majutsu-buffer-log-filesets))
   ((and (memq use-buffer-args '(always selected))
         (when-let* ((buf (majutsu--get-mode-buffer mode (eq use-buffer-args 'selected))))
           (list (buffer-local-value 'majutsu-buffer-log-args buf)
                 (buffer-local-value 'majutsu-buffer-log-revsets buf)
                 (buffer-local-value 'majutsu-buffer-log-filesets buf)))))
   ((plist-member (symbol-plist mode) 'majutsu-log-current-arguments)
    (list (get mode 'majutsu-log-current-arguments)
          (get mode 'majutsu-log-current-revsets)
          (get mode 'majutsu-log-current-filesets)))
   ((when-let* ((elt (assq (intern (format "majutsu-log:%s" mode))
                           transient-values)))
      (list (cdr elt)
            (get mode 'majutsu-log-current-revsets)
            (get mode 'majutsu-log-current-filesets))))
   (t
    (list (get mode 'majutsu-log-default-arguments)
          (get mode 'majutsu-log-default-revsets)
          (get mode 'majutsu-log-default-filesets)))))

(defun majutsu-log--set-value (mode args revsets filesets &optional save)
  "Set current log values for MODE.

When SAVE is non-nil, also persist ARGS using `transient-values'."
  (setq args (seq-remove #'null (flatten-tree args)))
  (setq filesets
        (and filesets
             (seq-remove (lambda (s)
                           (or (null s)
                               (and (stringp s) (string-empty-p s))))
                         (flatten-tree filesets))))
  (put mode 'majutsu-log-current-arguments args)
  (put mode 'majutsu-log-current-revsets revsets)
  (put mode 'majutsu-log-current-filesets filesets)
  (when save
    (setf (alist-get (intern (format "majutsu-log:%s" mode)) transient-values) args)
    (transient-save-values))
  (when (eq major-mode mode)
    (setq-local majutsu-buffer-log-args args)
    (setq-local majutsu-buffer-log-revsets revsets)
    (setq-local majutsu-buffer-log-filesets filesets))
  nil)

(defvar-local majutsu-log--this-error nil
  "Last jj side-effect error summary for this log buffer.

This is set by process runners (see `majutsu-process-buffer') and
rendered by `majutsu-log-insert-error-header' on the next refresh.")

(defcustom majutsu-log-sections-hook
  (list #'majutsu-log-insert-error-header
        #'majutsu-log-insert-logs
        #'majutsu-log-insert-status
        #'majutsu-insert-workspaces)
  "Hook run to insert sections in the log buffer."
  :type 'hook
  :group 'majutsu)

(defun majutsu-log--args-member-p (args flag)
  (and args (member flag args)))

(defun majutsu-log--args-get-option (args opt)
  "Return OPT's value from ARGS, or nil.
Only supports simple OPT VALUE pairs."
  (let ((pos (seq-position args opt #'equal)))
    (and pos
         (nth (1+ pos) args))))

(defun majutsu-log--args-remove-option (args opt &optional takes-value)
  "Return ARGS with OPT removed.
When TAKES-VALUE is non-nil, also remove the following element."
  (let ((out nil))
    (while args
      (let ((a (pop args)))
        (if (equal a opt)
            (when takes-value
              (pop args))
          (push a out))))
    (nreverse out)))

(defun majutsu-log--args-toggle-flag (args flag)
  (if (member flag args)
      (remove flag args)
    (append args (list flag))))

(defun majutsu-log--args-set-option (args opt value)
  "Set OPT to VALUE inside ARGS (removing existing OPT)."
  (setq args (majutsu-log--args-remove-option args opt t))
  (if value
      (append args (list opt value))
    args))

(defun majutsu-log--summary-parts ()
  "Return a list of human-readable fragments describing current log buffer."
  (pcase-let* ((`(,args ,revsets ,filesets)
                (majutsu-log--get-value 'majutsu-log-mode 'current))
               (parts '()))
    (when revsets
      (push (format "rev=%s" revsets) parts))
    (when-let* ((limit (majutsu-log--args-get-option args "-n")))
      (push (format "limit=%s" limit) parts))
    (when (majutsu-log--args-member-p args "--reversed")
      (push "reversed" parts))
    (when (majutsu-log--args-member-p args "--no-graph")
      (push "no-graph" parts))
    (when filesets
      (push (if (= (length filesets) 1)
                (format "path=%s" (car filesets))
              (format "paths=%d" (length filesets)))
            parts))
    (nreverse parts)))

(defun majutsu-log--format-summary (prefix)
  "Return PREFIX annotated with active log state summary."
  (let ((parts (majutsu-log--summary-parts)))
    (if parts
        (format "%s (%s)" prefix (string-join parts ", "))
      prefix)))

(defun majutsu-log--heading-string ()
  "Return heading string for the log section."
  (majutsu-log--format-summary "Log Graph"))

(defun majutsu-log--transient-description ()
  "Return description string for the log transient."
  (majutsu-log--format-summary "JJ Log"))

;;; Log Template

(defconst majutsu-log--field-separator "\x1e"
  "Separator character inserted between fields inside each module payload.")

(defconst majutsu-log--field-line-separator "\x1f"
  "Encoded newline separator used inside template field payloads.
Log records are transported as single lines, then this separator is
decoded back to literal newlines after field splitting.")

(defconst majutsu-log--record-marker "\x1d"
  "Control marker prefix for module boundaries inside log output.")

(defconst majutsu-log--entry-start-token (concat majutsu-log--record-marker "S")
  "Marker that starts a commit entry and heading payload.")

(defconst majutsu-log--entry-body-token (concat majutsu-log--record-marker "B")
  "Marker that starts the body payload.")

(defconst majutsu-log--entry-right-token (concat majutsu-log--record-marker "R")
  "Marker that starts the right-margin payload.")

(defconst majutsu-log--entry-meta-token (concat majutsu-log--record-marker "M")
  "Marker that starts the metadata payload.")

(defconst majutsu-log--entry-end-token (concat majutsu-log--record-marker "E")
  "Marker that terminates a commit entry.")

(defconst majutsu-log--module-order '(heading body right-margin metadata)
  "Module parse/render order for sequential log payloads.")

(defconst majutsu-log--field-default-modules
  '((id . metadata)
    (change-id . heading)
    (commit-id . metadata)
    (bookmarks . heading)
    (tags . heading)
    (working-copies . heading)
    (flags . metadata)
    (git-head . heading)
    (signature . heading)
    (empty . heading)
    (description . heading)
    (author . right-margin)
    (timestamp . right-margin)
    (long-desc . body))
  "Default module placement for known log fields.")

(defconst majutsu-log--required-columns '(id)
  "Fields that must exist in `majutsu-log-commit-columns'.")

(defconst majutsu-log--default-column-postprocessors
  '(majutsu-log-post-decode-line-separator)
  "Default postprocessors applied to every field unless overridden.")

(defcustom majutsu-log-commit-columns
  '((:field change-id :module heading :face t)
    (:field bookmarks :module heading :face magit-branch-local)
    (:field tags :module heading :face magit-tag)
    (:field working-copies :module heading :face magit-branch-remote)
    (:field empty :module heading :face t)
    (:field git-head :module heading :face t)
    (:field description :module heading :face t)
    (:field long-desc :module body :face t)
    (:field author :module right-margin :face magit-log-author)
    (:field timestamp :module right-margin :face magit-log-date)
    (:field id :module metadata :face nil)
    (:field commit-id :module metadata :face nil)
    (:field flags :module metadata :face nil))
  "Field specification controlling log template and rendering.

Each element is a plist with at least `:field'. Supported keys:
- :field  - symbol identifying a known field.
- :module - one of `heading', `body', `right-margin', `metadata'.
- :face   - t (preserve jj highlighting), nil (strip), or FACE (override).
- :post   - postprocessor function or function list for field value transforms.

`heading' is the only module that may emit physical newlines. Other modules
remain on the final heading line and should encode logical newlines as
`majutsu-log--field-line-separator' (\\x1f)."
  :type '(repeat (plist :options (:field :module :face :post)))
  :group 'majutsu)

(defmacro majutsu-log-define-column (name template doc)
  "Define a log column template variable for NAME with default TEMPLATE and DOC.
The variable name will be `majutsu-log-template-NAME'.
Also registers a variable watcher to invalidate the template cache."
  (declare (indent 1) (debug t) (doc-string 3))
  (let ((var-name (intern (format "majutsu-log-template-%s" name))))
    `(progn
       (defcustom ,var-name ,template
         ,doc
         :type 'sexp
         :group 'majutsu)
       (when (fboundp 'add-variable-watcher)
         (add-variable-watcher ',var-name #'majutsu-log--invalidate-template-cache)))))

(majutsu-template-defun short-change-id ()
  (:returns Template :flavor :custom :doc "Shortest unique change id.")
  [:change_id :shortest 8])

(majutsu-template-defun git_head ()
  (:returns Template :flavor :custom :doc "Deprecated alias for .contained_in('first_parent(@)')")
  [:method 'self :contained_in "first_parent(@)"])

(majutsu-template-defun short-change-id-with-offset ()
  (:returns Template :flavor :custom :doc "Shortest unique change id with offset.")
  [[:short-change-id]
   [:label "change_offset" "/"]
   [:change_offset]])

(majutsu-log-define-column id
  [:if [:or [:hidden] [:divergent]]
      [:commit_id :shortest 8]
    [:change_id :shortest 8]]
  "Template for the commit-id column.")

(majutsu-log-define-column change-id
  [:label
   [:separate " "
              [:if [:current_working_copy] "working_copy"]
              [:if [:immutable] "immutable" "mutable"]
              [:if [:conflict] "conflicted"]]
   [:coalesce
    [:if [:hidden]
        [:label "hidden" [:short-change-id-with-offset]]]
    [:if [:divergent]
        [:label "divergent" [:short-change-id-with-offset]]]
    [:short-change-id]]]
  "Template for the change-id column.")

(majutsu-log-define-column commit-id
  [:commit_id :shortest 8]
  "Template for the commit-id column.")

(majutsu-log-define-column bookmarks
  [:bookmarks]
  "Template for the bookmarks column.")

(majutsu-log-define-column tags
  [:tags]
  "Template for the tags column.")

(majutsu-log-define-column working-copies
  [:working_copies]
  "Template for the working-copies column.")

(majutsu-log-define-column flags
  [:separate " "
             [:if [:current_working_copy] "@"]
             [:if [:immutable] "immutable" "mutable"]
             [:if [:conflict] [:label "conflict" "conflict"]]
             [:if [:git_head] "git_head"]
             [:if [:root] "root"]
             [:if [:empty] "(empty)"]]
  "Template for the flags column.")

(majutsu-log-define-column git-head
  [:if [:git_head] [:label "git_head" "(git_head)"]]
  "Template for the git-head column.")

(majutsu-log-define-column signature
  [:if [:method [:call 'config "ui.show-cryptographic-signatures"] :as_boolean]
      [:if [:signature]
          [:label "signature status"
                  ["["
                   [:label [:signature :status]
                           [:coalesce
                            [:if [:== [:signature :status] "good"] "✓︎"]
                            [:if [:== [:signature :status] "unknown"] "?"]
                            "x"]]
                   "]"]]]]
  "Template for the signature column.")

(majutsu-log-define-column empty
  [:if [:empty]
      [:label "empty" "(empty)"]]
  "Template for the empty column.")

(majutsu-log-define-column description
  [:if [:description]
      [:method [:description] :first_line]
    [:label
     [:if [:empty] "empty"]
     [:label
      "description placeholder"
      "(no desc)"]]]
  "Template for the description column.")

(majutsu-log-define-column author
  [:author :name]
  "Template for the author column.")

(majutsu-log-define-column timestamp
  [:committer :timestamp :ago]
  "Template for the timestamp column.")

(majutsu-log-define-column long-desc
  [:description :lines :skip 1 :join "\x1f"]
  "Template for the long-desc column.
Newlines are encoded as an internal separator so ANSI/label styling
can survive transport through the single-line log format.")

(defvar majutsu-log--compiled-template-cache nil
  "Cached structure holding the compiled log template and column metadata.")

(defun majutsu-log--invalidate-template-cache (&rest _)
  "Reset cached compiled template when layout changes."
  (setq majutsu-log--compiled-template-cache nil)
  (setq majutsu-log--cached-entries nil))

(defun majutsu-log-post-decode-line-separator (value &optional _ctx)
  "Decode `majutsu-log--field-line-separator' inside VALUE.

This is the default postprocessor for log fields so all modules can
transport logical newlines safely through single-line payload segments."
  (if (stringp value)
      (subst-char-in-string
       (aref majutsu-log--field-line-separator 0)
       ?\n
       value t)
    value))

(defun majutsu-log--default-module-for-field (field)
  "Return default module symbol for FIELD."
  (or (alist-get field majutsu-log--field-default-modules nil nil #'eq)
      (user-error "Field %S requires explicit :module" field)))

(defun majutsu-log--default-face-for-field (field)
  "Return default face policy for FIELD."
  (let ((spec (alist-get field majutsu-log-field-faces nil nil #'eq)))
    (if (null spec) t spec)))

(defun majutsu-log--normalize-postprocessors (post field)
  "Normalize POST value for FIELD into a function list."
  (let ((fns (cond
              ((eq post :default) majutsu-log--default-column-postprocessors)
              ((null post) nil)
              ((functionp post) (list post))
              ((and (listp post) (seq-every-p #'functionp post)) post)
              (t (user-error "Column %S has invalid :post %S" field post)))))
    (dolist (fn fns)
      (unless (functionp fn)
        (user-error "Column %S has non-callable postprocessor %S" field fn)))
    fns))

(defun majutsu-log--normalize-column-spec (spec)
  "Normalize a single column SPEC into a plist with defaults."
  (let* ((col (cond
               ((and (plistp spec) (plist-get spec :field)) spec)
               ((symbolp spec) (list :field spec))
               (t (user-error "Invalid column spec: %S" spec))))
         (field (plist-get col :field))
         (module (if (plist-member col :module)
                     (plist-get col :module)
                   (majutsu-log--default-module-for-field field)))
         (face (if (plist-member col :face)
                   (plist-get col :face)
                 (majutsu-log--default-face-for-field field)))
         (post (if (plist-member col :post)
                   (plist-get col :post)
                 :default)))
    (setq module (if (keywordp module)
                     (intern (substring (symbol-name module) 1))
                   module))
    (unless (memq module majutsu-log--module-order)
      (user-error "Column %S has invalid :module %S" field module))
    (unless (or (eq face t) (null face) (symbolp face))
      (user-error "Column %S has invalid :face %S" field face))
    (list :field field
          :module module
          :face face
          :post (majutsu-log--normalize-postprocessors post field))))

(defun majutsu-log--ensure-required-columns (columns)
  "Ensure required columns are present in COLUMNS list.
Missing required fields are appended with defaults."
  (let ((present (mapcar (lambda (c) (plist-get c :field)) columns)))
    (dolist (req majutsu-log--required-columns)
      (unless (memq req present)
        (setq columns (append columns (list (majutsu-log--normalize-column-spec req))))))
    columns))

(defun majutsu-log--module-columns (compiled module)
  "Return compiled column specs for MODULE from COMPILED metadata."
  (alist-get module (plist-get compiled :module-columns) nil nil #'eq))

(defun majutsu-log--column-template (field)
  "Return majutsu-template form for FIELD.
Looks up `majutsu-log-template-FIELD'."
  (let ((var (intern-soft (format "majutsu-log-template-%s" field))))
    (if (and var (boundp var))
        (symbol-value var)
      (user-error "Unknown column field %S" field))))

(defun majutsu-log--build-module-template-form (templates)
  "Return a template form joining TEMPLATES with field separators."
  (cond
   ((null templates) "")
   ((null (cdr templates)) (car templates))
   (t
    (let ((forms nil)
          (first t))
      (dolist (template templates)
        (unless first
          (setq forms (append forms (list majutsu-log--field-separator))))
        (setq first nil)
        (setq forms (append forms (list template))))
      (cons :concat forms)))))

(defun majutsu-log--compile-columns (&optional columns)
  "Compile COLUMNS (or `majutsu-log-commit-columns') into a jj template string.
Returns a plist with :template, :columns, and :module-columns."
  (let* ((normalized (mapcar #'majutsu-log--normalize-column-spec
                             (or columns majutsu-log-commit-columns)))
         (complete (majutsu-log--ensure-required-columns normalized))
         (module-columns
          (mapcar (lambda (module)
                    (cons module
                          (seq-filter (lambda (c)
                                        (eq (plist-get c :module) module))
                                      complete)))
                  majutsu-log--module-order))
         (heading-form
          (majutsu-log--build-module-template-form
           (mapcar (lambda (c)
                     (majutsu-log--column-template (plist-get c :field)))
                   (alist-get 'heading module-columns nil nil #'eq))))
         (body-form
          (majutsu-log--build-module-template-form
           (mapcar (lambda (c)
                     (majutsu-log--column-template (plist-get c :field)))
                   (alist-get 'body module-columns nil nil #'eq))))
         (right-form
          (majutsu-log--build-module-template-form
           (mapcar (lambda (c)
                     (majutsu-log--column-template (plist-get c :field)))
                   (alist-get 'right-margin module-columns nil nil #'eq))))
         (meta-form
          (majutsu-log--build-module-template-form
           (mapcar (lambda (c)
                     (majutsu-log--column-template (plist-get c :field)))
                   (alist-get 'metadata module-columns nil nil #'eq))))
         (compiled
          (majutsu-tpl
           `[:concat
             ,majutsu-log--entry-start-token
             ,heading-form
             ,majutsu-log--entry-body-token
             ,body-form
             ,majutsu-log--entry-right-token
             ,right-form
             ,majutsu-log--entry-meta-token
             ,meta-form
             ,majutsu-log--entry-end-token
             "\n"])))
    (list :template compiled
          :columns complete
          :module-columns module-columns)))

(defun majutsu-log--ensure-template ()
  "Return cached compiled template structure, recomputing if necessary."
  (or majutsu-log--compiled-template-cache
      (setq majutsu-log--compiled-template-cache
            (majutsu-log--compile-columns majutsu-log-commit-columns))))

(defun majutsu-log--build-args ()
  "Build argument list for `jj log' using current log variables."
  (pcase-let ((`(,args ,revsets ,filesets)
               (majutsu-log--get-value 'majutsu-log-mode 'current)))
    (let ((cmd '("log")))
      (setq cmd (append cmd args))
      (when revsets
        (setq cmd (append cmd (list "-r" revsets))))
      (setq cmd (append cmd (list "-T" (plist-get (majutsu-log--ensure-template) :template))))
      (setq cmd (append cmd filesets))
      cmd)))

;;; Log Parsing

(defvar-local majutsu-log--cached-entries nil
  "Cached log entries for the current buffer.")

(defun majutsu-log--split-by-separator (value separator)
  "Split VALUE by one-char string SEPARATOR, preserving empty fields."
  (if (not (stringp value))
      nil
    (let ((start 0)
          (len (length value))
          (sep (aref separator 0))
          out)
      (dotimes (idx len)
        (when (eq (aref value idx) sep)
          (push (substring value start idx) out)
          (setq start (1+ idx))))
      (push (substring value start len) out)
      (nreverse out))))

(defun majutsu-log--join-lines (lines)
  "Join LINES with literal newlines, preserving string properties."
  (if (null lines)
      ""
    (let ((out (car lines)))
      (dolist (line (cdr lines) out)
        (setq out (concat out "\n" line))))))

(defun majutsu-log--line-token-position (token bol eol &optional start)
  "Return start position of TOKEN between BOL and EOL, or nil."
  (save-excursion
    (goto-char (or start bol))
    (when (search-forward token eol t)
      (- (point) (length token)))))

(defun majutsu-log--parse-tail-payloads (tail)
  "Parse B/R/M/E payload segments from TAIL string.

TAIL must start with `majutsu-log--entry-body-token'."
  (when (string-prefix-p majutsu-log--entry-body-token tail)
    (let* ((body-start (length majutsu-log--entry-body-token))
           (right-pos (string-match (regexp-quote majutsu-log--entry-right-token)
                                    tail body-start))
           (meta-pos (and right-pos
                          (string-match (regexp-quote majutsu-log--entry-meta-token)
                                        tail (+ right-pos (length majutsu-log--entry-right-token)))))
           (end-pos (and meta-pos
                         (string-match (regexp-quote majutsu-log--entry-end-token)
                                       tail (+ meta-pos (length majutsu-log--entry-meta-token))))))
      (when (and right-pos meta-pos end-pos)
        (let ((trailing (substring tail (+ end-pos (length majutsu-log--entry-end-token)))))
          (when (string-empty-p trailing)
            (list :body (substring tail body-start right-pos)
                  :right-margin (substring tail
                                           (+ right-pos (length majutsu-log--entry-right-token))
                                           meta-pos)
                  :metadata (substring tail
                                       (+ meta-pos (length majutsu-log--entry-meta-token))
                                       end-pos))))))))

(defun majutsu-log--split-module-values (payload count)
  "Split PAYLOAD into COUNT field values using `majutsu-log--field-separator'."
  (if (<= count 0)
      nil
    (let ((values (majutsu-log--split-by-separator (or payload "") majutsu-log--field-separator)))
      (cond
       ((< (length values) count)
        (append values (make-list (- count (length values)) "")))
       ((> (length values) count)
        (seq-take values count))
       (t values)))))

(defun majutsu-log--apply-postprocessor (fn value ctx)
  "Apply postprocessor FN to VALUE with context CTX.

FN may accept either (VALUE) or (VALUE CTX). Errors return VALUE unchanged."
  (condition-case err
      (condition-case _
          (funcall fn value ctx)
        (wrong-number-of-arguments
         (funcall fn value)))
    (error
     (majutsu--debug "majutsu-log postprocessor failed (%S on %S): %s"
                     fn (plist-get ctx :field) (error-message-string err))
     value)))

(defun majutsu-log--apply-postprocessors (value postprocessors ctx)
  "Apply POSTPROCESSORS to VALUE with CTX sequentially."
  (let ((out value))
    (dolist (fn postprocessors out)
      (setq out (majutsu-log--apply-postprocessor fn out ctx)))))

(defun majutsu-log--apply-flags (entry value)
  "Set flag fields on ENTRY based on VALUE string."
  (dolist (flag (split-string (or value "") " " t))
    (pcase flag
      ("immutable" (setq entry (plist-put entry :immutable t)))
      ("mutable" (setq entry (plist-put entry :immutable nil)))
      ("conflict" (setq entry (plist-put entry :conflict t)))
      ("git_head" (setq entry (plist-put entry :git-head t)))
      ("root" (setq entry (plist-put entry :root t)))
      ("@" (setq entry (plist-put entry :current_working_copy t)))))
  entry)

(defun majutsu-log--record-column (entry field value)
  "Record FIELD VALUE onto ENTRY plist and column map."
  (pcase field
    ('id
     (setq entry (plist-put entry :id value)))
    ('change-id
     (setq entry (plist-put entry :change-id value)))
    ('commit-id
     (setq entry (plist-put entry :commit-id value)))
    ('bookmarks
     (setq entry (plist-put entry :bookmarks value)))
    ('tags
     (setq entry (plist-put entry :tags value)))
    ('working-copies
     (setq entry (plist-put entry :working-copies value)))
    ('description
     (setq entry (plist-put entry :short-desc value)))
    ('author
     (setq entry (plist-put entry :author value)))
    ('timestamp
     (setq value (string-remove-suffix " ago" value))
     (setq entry (plist-put entry :timestamp value)))
    ('long-desc
     (setq entry (plist-put entry :long-desc value)))
    ('flags
     (setq entry (majutsu-log--apply-flags entry value)))
    ('git-head
     (when (and value (not (string-empty-p value)))
       (setq entry (plist-put entry :git-head t))))
    ('signature
     (setq entry (plist-put entry :signature value)))
    ('empty
     (setq entry (plist-put entry :empty (not (string-empty-p value))))))
  (let* ((columns (plist-get entry :columns)))
    (setf (alist-get field columns nil nil #'eq) value)
    (setq entry (plist-put entry :columns columns)))
  entry)

(defun majutsu-log--record-module-fields (entry module payload compiled)
  "Record MODULE PAYLOAD values into ENTRY using COMPILED module layout."
  (let* ((columns (majutsu-log--module-columns compiled module))
         (values (majutsu-log--split-module-values payload (length columns)))
         (stored nil))
    (cl-loop for column in columns
             for value in values
             for field = (plist-get column :field)
             for ctx = (list :field field :module module)
             for out = (majutsu-log--apply-postprocessors value (plist-get column :post) ctx)
             do (setq entry (majutsu-log--record-column entry field out))
             do (push out stored))
    (let ((modules (plist-get entry :modules)))
      (setf (alist-get module modules nil nil #'eq) (nreverse stored))
      (setq entry (plist-put entry :modules modules)))
    entry))

(defun majutsu-log--parse-entry-at-point (compiled)
  "Parse one sequentially-encoded log entry at point using COMPILED.

Point must be at a line potentially containing `majutsu-log--entry-start-token'.
Returns entry plist and moves point past the consumed entry, or nil."
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (start-pos (majutsu-log--line-token-position majutsu-log--entry-start-token bol eol)))
    (when start-pos
      (let* ((indent (- start-pos bol))
             (heading-prefixes nil)
             (heading-segments nil)
             (tail nil)
             (done nil)
             (first-line t))
        (while (and (not done) (not (eobp)))
          (setq bol (line-beginning-position)
                eol (line-end-position))
          (let* ((prefix-end (min (+ bol indent) eol))
                 (prefix (buffer-substring bol prefix-end))
                 (content-start (if first-line
                                    (+ start-pos (length majutsu-log--entry-start-token))
                                  prefix-end))
                 (body-pos (majutsu-log--line-token-position
                            majutsu-log--entry-body-token bol eol content-start)))
            (if body-pos
                (progn
                  (push prefix heading-prefixes)
                  (push (buffer-substring content-start body-pos) heading-segments)
                  (setq tail (buffer-substring body-pos eol))
                  (setq done t)
                  (forward-line 1))
              (push prefix heading-prefixes)
              (push (buffer-substring content-start eol) heading-segments)
              (forward-line 1)
              (when (eobp)
                (setq done :incomplete))))
          (setq first-line nil))
        (when (eq done t)
          (when-let* ((tail-payloads (majutsu-log--parse-tail-payloads tail)))
            (let* ((entry (list :indent indent
                                :columns nil
                                :modules nil
                                :heading-prefixes (nreverse heading-prefixes)))
                   (heading-payload (majutsu-log--join-lines (nreverse heading-segments))))
              (setq entry (majutsu-log--record-module-fields entry 'heading heading-payload compiled))
              (setq entry (majutsu-log--record-module-fields
                           entry 'body (plist-get tail-payloads :body) compiled))
              (setq entry (majutsu-log--record-module-fields
                           entry 'right-margin (plist-get tail-payloads :right-margin) compiled))
              (setq entry (majutsu-log--record-module-fields
                           entry 'metadata (plist-get tail-payloads :metadata) compiled))
              (let ((suffix-lines nil))
                ;; Preserve graph continuation lines between the current entry's
                ;; end marker and the next entry start marker. These lines stay
                ;; visible as part of the current section heading area.
                (while (and (not (eobp))
                            (let ((next-bol (line-beginning-position))
                                  (next-eol (line-end-position)))
                              (not (majutsu-log--line-token-position
                                    majutsu-log--entry-start-token next-bol next-eol))))
                  (push (buffer-substring (line-beginning-position) (line-end-position))
                        suffix-lines)
                  (forward-line 1))
                (setq entry (plist-put entry :suffix-lines (nreverse suffix-lines))))
              entry)))))))

(defun majutsu-log--parse-entries-in-buffer (compiled)
  "Parse all sequentially-encoded log entries in current buffer using COMPILED."
  (let (entries)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((entry (majutsu-log--parse-entry-at-point compiled)))
          (if entry
              (push entry entries)
            (forward-line 1)))))
    (nreverse entries)))

(defun majutsu--indent-string (s column)
  "Insert STRING into the current buffer, indenting each line to COLUMN."
  (let ((indentation (make-string column ?\s))) ; Create a string of spaces for indentation
    (mapconcat (lambda (line)
                 (concat indentation line))
               (split-string s "\n")
               "\n"))) ; Join lines with newline, prefixed by indentation

(defun majutsu-log--entry-column (entry field)
  "Return string value for FIELD stored on ENTRY."
  (alist-get field (plist-get entry :columns) nil nil #'eq))

(defun majutsu-log--apply-face-policy (value face)
  "Apply FACE policy to VALUE and return display string."
  (let ((v (or value "")))
    (cond
     ((eq face t) v)
     ((null face) (substring-no-properties v))
     (t (propertize (substring-no-properties v) 'font-lock-face face)))))

(defun majutsu-log--max-line-width (value)
  "Return max display width among VALUE's lines."
  (if (or (null value) (string-empty-p value))
      0
    (let ((widths (mapcar #'string-width (split-string value "\n" nil))))
      (if widths (apply #'max widths) 0))))

(defun majutsu-log--compute-column-widths (entries compiled)
  "Compute right-margin widths from ENTRIES according to COMPILED layout."
  (let* ((right-cols (majutsu-log--module-columns compiled 'right-margin))
         (right-widths ()))
    (dolist (entry entries)
      (dolist (col right-cols)
        (let* ((field (plist-get col :field))
               (val (replace-regexp-in-string "\n" " "
                                              (or (majutsu-log--entry-column entry field) "")
                                              nil t))
               (w (majutsu-log--max-line-width val)))
          (setf (alist-get field right-widths nil nil #'eq)
                (max w (or (alist-get field right-widths nil nil #'eq) 0))))))
    (let* ((right-total
            (let ((sum 0) (first t))
              (dolist (col right-cols)
                (let* ((field (plist-get col :field))
                       (w (or (alist-get field right-widths nil nil #'eq) 0)))
                  (unless first (setq sum (1+ sum)))
                  (setq first nil)
                  (setq sum (+ sum w))))
              sum)))
      (list :right right-widths :right-total right-total))))

(defun majutsu-log--pad-display (text width align)
  "Pad TEXT to WIDTH using ALIGN (`left' | `right' | `center')."
  (let* ((txt (or text ""))
         (len (string-width txt))
         (pad (max 0 (- width len))))
    (pcase align
      ('right (concat (make-string pad ?\s) txt))
      ('center (let* ((left (/ pad 2))
                      (right (- pad left)))
                 (concat (make-string left ?\s) txt (make-string right ?\s))))
      (_ (concat txt (make-string pad ?\s))))))

(defun majutsu-log--concat-heading-parts (parts)
  "Concatenate heading PARTS without adding spaces after newlines."
  (let ((out ""))
    (dolist (part parts out)
      (unless (string-empty-p part)
        (let ((need-space
               (and (> (length out) 0)
                    (not (eq (aref out (1- (length out))) ?\n))
                    (not (eq (aref part 0) ?\n)))))
          (setq out (concat out (if need-space " " "") part)))))))

(defun majutsu-log--render-heading-lines (entry compiled)
  "Render ENTRY heading module as visible lines with graph prefixes."
  (let* ((columns (majutsu-log--module-columns compiled 'heading))
         (parts nil))
    (dolist (column columns)
      (let* ((field (plist-get column :field))
             (face (plist-get column :face))
             (raw (or (majutsu-log--entry-column entry field) ""))
             (value (majutsu-log--apply-face-policy raw face)))
        (unless (string-empty-p value)
          (push value parts))))
    (setq parts (nreverse parts))
    (let* ((content (if parts (majutsu-log--concat-heading-parts parts) ""))
           (content-lines (split-string content "\n" nil))
           (prefixes (or (plist-get entry :heading-prefixes) (list "")))
           (last-prefix (or (car (last prefixes)) ""))
           (count (max (length content-lines) (length prefixes)))
           out)
      (cl-loop for idx below count
               do (let ((prefix (or (nth idx prefixes) last-prefix))
                        (line (or (nth idx content-lines) "")))
                    (push (concat prefix line) out)))
      (nreverse out))))

(defun majutsu-log--render-right-margin (entry compiled widths)
  "Render ENTRY right-margin module string using WIDTHS and COMPILED layout."
  (let* ((columns (majutsu-log--module-columns compiled 'right-margin))
         (parts nil)
         (has-content nil))
    (dolist (column columns)
      (let* ((field (plist-get column :field))
             (face (plist-get column :face))
             (raw (replace-regexp-in-string "\n" " "
                                            (or (majutsu-log--entry-column entry field) "")
                                            nil t))
             (col-width (or (alist-get field (plist-get widths :right) nil nil #'eq)
                            (string-width raw)))
             (padded (majutsu-log--pad-display raw col-width 'right))
             (value (majutsu-log--apply-face-policy padded face)))
        (unless (string-empty-p (string-trim raw))
          (setq has-content t))
        (push value parts)))
    (when has-content
      (string-join (nreverse parts) " "))))

(defun majutsu-log--render-body (entry compiled)
  "Render ENTRY body module as foldable multiline content."
  (let* ((columns (majutsu-log--module-columns compiled 'body))
         (parts nil))
    (dolist (column columns)
      (let* ((field (plist-get column :field))
             (face (plist-get column :face))
             (raw (or (majutsu-log--entry-column entry field) ""))
             (value (majutsu-log--apply-face-policy raw face)))
        (unless (string-empty-p (string-trim value))
          (push value parts))))
    (when parts
      (string-join (nreverse parts) "\n"))))

(defun majutsu-log--entry-id (entry)
  "Return stable section id string from ENTRY metadata fields."
  (or (let ((id (plist-get entry :id)))
        (and (stringp id)
             (not (string-empty-p (string-trim id)))
             (substring-no-properties id)))
      (let ((change-id (plist-get entry :change-id)))
        (and (stringp change-id)
             (not (string-empty-p (string-trim change-id)))
             (substring-no-properties change-id)))
      "unknown"))

(defun majutsu-log--set-right-margin (width)
  "Set right margin WIDTH (in columns) for all windows showing current buffer."
  (dolist (win (get-buffer-window-list (current-buffer) nil t))
    (with-selected-window win
      (let ((left (car (window-margins win))))
        (if (and width (> width 0))
            (set-window-margins win left width)
          (set-window-margins win left nil))))))

(defun majutsu-log--make-margin-overlay (string)
  "Display STRING in the right margin of the current (or previous) line."
  (save-excursion
    (forward-line (if (bolp) -1 0))
    (let ((o (make-overlay (1+ (point)) (line-end-position) nil t)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'before-string
                   (propertize " " 'display
                               (list (list 'margin 'right-margin)
                                     (or string " ")))))))

(defun majutsu-log-insert-error-header ()
  "Insert the message about the jj error that just occurred.

This function only knows about the last error that occurred when jj was
run for side-effects.  Refreshing the log buffer causes this section to
disappear again."
  (when majutsu-log--this-error
    (magit-insert-section (error 'jj)
      (insert (propertize (format "%-10s" "JJError! ")
                          'font-lock-face 'magit-section-heading))
      (insert (propertize majutsu-log--this-error 'font-lock-face 'error))
      (when-let* ((_ majutsu-show-process-buffer-hint)
                  (key (car (where-is-internal 'majutsu-process-buffer))))
        (insert (format "  [Type %s for details]" (key-description key))))
      (insert ?\n))
    (setq majutsu-log--this-error nil)))


(defun majutsu-log--insert-entry (entry compiled widths)
  "Insert parsed ENTRY as a `jj-commit' section using COMPILED and WIDTHS."
  (let* ((id (majutsu-log--entry-id entry))
         (heading-lines (majutsu-log--render-heading-lines entry compiled))
         (heading (majutsu-log--join-lines heading-lines))
         (suffix-lines (plist-get entry :suffix-lines))
         (margin (majutsu-log--render-right-margin entry compiled widths))
         (body (majutsu-log--render-body entry compiled))
         (has-body (and (stringp body)
                        (not (string-empty-p (string-trim body))))))
    (magit-insert-section (jj-commit id t)
      (insert heading)
      (insert "\n")
      (when margin
        (majutsu-log--make-margin-overlay margin))
      (dolist (suffix-line suffix-lines)
        (insert suffix-line)
        (insert "\n"))
      (when has-body
        (magit-insert-heading)
        (let ((indented (majutsu--indent-string body (or (plist-get entry :indent) 0))))
          (magit-insert-section-body
            (insert indented)
            (insert "\n")))))))

(defun majutsu-log--wash-logs (_args)
  "Wash jj log output in the current (narrowed) buffer region.

This function is meant to be used as a WASHER for `majutsu-jj-wash'."
  (let* ((compiled (majutsu-log--ensure-template))
         (entries (majutsu-log--parse-entries-in-buffer compiled))
         (widths (majutsu-log--compute-column-widths entries compiled)))
    (setq majutsu-log--cached-entries entries)
    (delete-region (point-min) (point-max))
    (majutsu-log--set-right-margin (plist-get widths :right-total))
    (dolist (entry entries)
      (majutsu-log--insert-entry entry compiled widths))
    (insert "\n")))

(defun majutsu-log-insert-logs ()
  "Insert jj log graph into current buffer."
  (magit-insert-section (lograph)
    (magit-insert-heading (majutsu-log--heading-string))
    (majutsu-jj-wash #'majutsu-log--wash-logs nil (majutsu-log--build-args))
    (insert "\n")))

;;; Log insert status

(defun majutsu-log--wash-status (_args)
  "Keep `jj status` output as-is in the current section."
  (goto-char (point-max)))

;; TODO: Enhance status output parsing to create sections per file and conflicts.
(defun majutsu-log-insert-status ()
  "Insert jj status into current buffer."
  (magit-insert-section (status)
    (magit-insert-heading "Working Copy Status")
    (majutsu-jj-wash #'majutsu-log--wash-status nil "status")))

;;; Log insert conflicts

(defun majutsu-log-insert-conflicts ()
  "Insert conflicted files section."
  (let ((lines (majutsu-jj-lines "resolve" "--list")))
    (when lines
      (magit-insert-section (conflict)
        (magit-insert-heading "Unresolved Conflicts")
        (dolist (line lines)
          (let ((file (string-trim line)))
            (magit-insert-section (jj-file file)
              (magit-insert-heading (propertize file 'face 'error))
              (insert "\n"))))
        (insert "\n")))))

;;; Log Navigation

(defconst majutsu--show-id-template
  (majutsu-tpl [:if [:or [:hidden] [:divergent]]
                   [:commit_id :shortest 8]
                 [:change_id :shortest 8]]))

(defun majutsu-current-id ()
  (when-let* ((output (majutsu-jj-string "log" "--no-graph" "-r" "@" "-T" majutsu--show-id-template)))
    (string-trim output)))

(defun majutsu-log-goto-@ ()
  "Jump to the current changeset (@)."
  (interactive)
  (majutsu--goto-log-entry (majutsu-current-id)))

(defun majutsu-goto-commit (commit-id)
  "Jump to a specific COMMIT-ID in the log."
  (interactive "sCommit ID: ")
  (let ((start-pos (point)))
    (goto-char (point-min))
    (if (re-search-forward (regexp-quote commit-id) nil t)
        (goto-char (line-beginning-position))
      (goto-char start-pos)
      (message "Commit %s not found" commit-id))))

(defun majutsu-goto-change (change-id)
  "Jump to a specific CHANGE-ID in the log."
  (interactive "sChange ID: ")
  (let ((start-pos (point)))
    (goto-char (point-min))
    (if (re-search-forward (regexp-quote change-id) nil t)
        (goto-char (line-beginning-position))
      (goto-char start-pos)
      (message "Change %s not found" change-id))))

(defun majutsu--goto-log-entry (id)
  "Move point to the log entry section matching ID.
Return non-nil when the section could be located."
  (when-let* ((id (and id (string-trim id)))
              (_(not (string-empty-p id)))
              (section (majutsu-selection-find-section id 'jj-commit)))
    (magit-section-goto section)
    (goto-char (oref section start))
    t))

;;;###autoload
(defun majutsu-goto-next-changeset ()
  "Navigate to the next changeset in the log."
  (interactive)
  (let ((pos (point))
        found)
    (while (and (not found)
                (< (point) (point-max)))
      (magit-section-forward)
      (when-let* ((section (magit-current-section)))
        (when (and (magit-section-match 'jj-commit section)
                   (> (point) pos))
          (setq found t))))
    (unless found
      (goto-char pos)
      (message "No more changesets"))))

;;;###autoload
(defun majutsu-goto-prev-changeset ()
  "Navigate to the previous changeset in the log."
  (interactive)
  (let ((pos (point))
        found)
    (while (and (not found)
                (> (point) (point-min)))
      (magit-section-backward)
      (when-let* ((section (magit-current-section)))
        (when (and (magit-section-match 'jj-commit section)
                   (< (point) pos))
          (setq found t))))
    (unless found
      (goto-char pos)
      (message "No more changesets"))))

;;; Log Mode

(defvar-keymap majutsu-log-mode-map
  :doc "Keymap for `majutsu-log-mode'."
  :parent majutsu-mode-map
  "n" 'majutsu-goto-next-changeset
  "p" 'majutsu-goto-prev-changeset
  "O" 'majutsu-new-dwim
  "D" 'majutsu-diff-dwim
  "Y" 'majutsu-duplicate-dwim
  "B" 'majutsu-new-with-before
  "A" 'majutsu-new-with-after)

(define-derived-mode majutsu-log-mode majutsu-mode "Majutsu Log"
  "Major mode for interacting with jj version control system."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer)
  (add-hook 'kill-buffer-hook #'majutsu-selection-session-end-if-owner nil t))

(cl-defmethod majutsu-buffer-value (&context (major-mode majutsu-log-mode))
  (list majutsu-buffer-log-args
        majutsu-buffer-log-revsets
        majutsu-buffer-log-filesets))

(defun majutsu-log-render ()
  "Render the log buffer using cached data."
  (magit-insert-section (logbuf)
    (run-hooks 'majutsu-log-sections-hook)
    (majutsu-selection-render)))

(defun majutsu-log-refresh-buffer ()
  "Refresh the current Majutsu log buffer."
  (majutsu--assert-mode 'majutsu-log-mode)
  (setq majutsu-log--cached-entries nil)
  (majutsu-log-render))

;;;###autoload
(defun majutsu-log-refresh ()
  "Refresh a majutsu log buffer for the current repository.
When called outside a log buffer, try to refresh an existing log
buffer for the same repository.  If none exists and the command
is invoked interactively, signal a user error instead of
mutating the wrong buffer."
  (interactive)
  (let* ((root (majutsu--buffer-root))
         (buffer (and root (majutsu--resolve-mode-buffer 'majutsu-log-mode root))))
    (cond
     (buffer
      (with-current-buffer buffer
        (majutsu-refresh-buffer)))
     ((called-interactively-p 'interactive)
      (user-error "Not in a Majutsu buffer; open one with `majutsu-log`"))
     (t
      (majutsu--debug "Skipping log refresh: no log buffer for %s" (or root "unknown repo"))))))

(defun majutsu-log-setup-buffer (&optional commit locked)
  "Set up a Majutsu log buffer and optionally focus COMMIT.

When LOCKED is non-nil, avoid reusing existing unlocked log buffers."
  (with-current-buffer
      (pcase-let ((`(,args ,revsets ,filesets)
                   (majutsu-log--get-value 'majutsu-log-mode 'direct)))
        (majutsu-setup-buffer #'majutsu-log-mode locked
          (majutsu-buffer-log-args (copy-sequence args))
          (majutsu-buffer-log-revsets revsets)
          (majutsu-buffer-log-filesets (copy-sequence filesets))))
    (when commit
      (unless (majutsu--goto-log-entry commit)
        (majutsu-log-goto-@)))
    (current-buffer)))

;;;###autoload
(defun majutsu-log (&optional directory)
  "Open the majutsu log buffer.

If the current directory isn't located within a jj repository, then
prompt for a directory.  If that directory isn't a repository either,
offer to create one using `jj git init`."
  (interactive
   (list (and (or current-prefix-arg (not (majutsu-toplevel)))
              (file-name-as-directory
               (expand-file-name
                (read-directory-name "Repository or directory: "
                                     nil nil nil))))))
  (let* ((default-directory (or directory default-directory))
         (topdir (majutsu-toplevel default-directory)))
    (cond
     (topdir
      (let ((default-directory topdir))
        (majutsu-log-setup-buffer)))
     ((y-or-n-p (format "Create jj repository in %s? "
                        (abbreviate-file-name default-directory)))
      (let* ((dest (file-name-as-directory (expand-file-name default-directory)))
             (default-directory dest)
             (_ (majutsu--assert-usable-jj))
             (jj (majutsu-jj--executable))
             (args (majutsu-process-jj-arguments (list "git" "init"
                                                       (majutsu-convert-filename-for-jj dest))))
             (exit nil)
             (out ""))
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix)
                (coding-system-for-write 'utf-8-unix))
            (setq exit (apply #'process-file jj nil t nil args)))
          (setq out (string-trim (buffer-string))))
        (if (zerop exit)
            (let ((default-directory dest))
              (majutsu-log-setup-buffer))
          (user-error "jj git init failed: %s"
                      (if (string-empty-p out)
                          (format "exit %s" exit)
                        out)))))
     (t
      (user-error "Abort")))))

;;; Commands

(defun majutsu-log-transient-set-revisions ()
  "Prompt for a revset and store it in the current log variables."
  (interactive)
  (let* ((current (cadr (majutsu-log--get-value 'majutsu-log-mode 'direct)))
         (input (string-trim (read-from-minibuffer "Revset (empty to clear): " current))))
    (pcase-let ((`(,args ,_revsets ,filesets)
                 (majutsu-log--get-value 'majutsu-log-mode 'direct)))
      (majutsu-log--set-value 'majutsu-log-mode args
                              (unless (string-empty-p input) input)
                              filesets))
    (majutsu-log-transient--redisplay)))

(defun majutsu-log-transient-clear-revisions ()
  "Clear the stored revset."
  (interactive)
  (pcase-let ((`(,args ,_revsets ,filesets)
               (majutsu-log--get-value 'majutsu-log-mode 'direct)))
    (majutsu-log--set-value 'majutsu-log-mode args nil filesets))
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-reset ()
  "Reset log options to defaults."
  (interactive)
  (majutsu-log--set-value 'majutsu-log-mode nil nil nil)
  (if (fboundp 'transient-reset)
      (transient-reset)
    (majutsu-log-transient--redisplay)))

(defun majutsu-log--toggle-desc (label flag)
  "Return LABEL annotated with ON/OFF state for FLAG in log args."
  (if (member flag (car (majutsu-log--get-value 'majutsu-log-mode 'direct)))
      (format "%s [on]" label)
    (format "%s [off]" label)))

(defun majutsu-log--value-desc (label value)
  "Return LABEL annotated with VALUE, when VALUE is non-nil."
  (if value
      (format "%s (%s)" label value)
    label))

(defun majutsu-log-transient--redisplay ()
  "Redisplay the log transient, compatible with older transient versions."
  (if (fboundp 'transient-redisplay)
      (transient-redisplay)
    (when (fboundp 'transient--redisplay)
      (transient--redisplay))))

;;; Arguments
;;;; Prefix Classes

(defclass majutsu-log-prefix (transient-prefix)
  ((history-key :initform 'majutsu-log)
   (major-mode :initform 'majutsu-log-mode)))

;;;; Prefix Methods

(cl-defmethod transient-prefix-value ((obj majutsu-log-prefix))
  "Return (args files) from transient value."
  (let ((args (cl-call-next-method obj)))
    (list (seq-filter #'atom args)
          (cdr (assoc "--" args)))))

(cl-defmethod transient-init-value ((obj majutsu-log-prefix))
  (pcase-let ((`(,args ,_revsets ,filesets)
               (majutsu-log--get-value (oref obj major-mode) 'prefix)))
    (oset obj value (if filesets `(("--" ,@filesets) ,@args) args))))

(cl-defmethod transient-set-value ((obj majutsu-log-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode)))
    (pcase-let ((`(,args ,files) (transient-args (oref obj command)))
                (`(,_old-args ,revsets ,_filesets)
                 (majutsu-log--get-value mode 'direct)))
      (majutsu-log--set-value mode args revsets files)
      (transient--history-push obj)
      (majutsu-refresh))))

(cl-defmethod transient-save-value ((obj majutsu-log-prefix))
  (let* ((obj (oref obj prototype))
         (mode (or (oref obj major-mode) major-mode)))
    (pcase-let ((`(,args ,files) (transient-args (oref obj command)))
                (`(,_old-args ,revsets ,_filesets)
                 (majutsu-log--get-value mode 'direct)))
      (majutsu-log--set-value mode args revsets files t)
      (transient--history-push obj)
      (majutsu-refresh))))

(transient-define-argument majutsu-log:--limit ()
  :description "Limit"
  :class 'transient-option
  :key "-n"
  :argument "--limit="
  :reader #'transient-read-number-N+)

(transient-define-argument majutsu-log:--reversed ()
  :description "Reverse order"
  :class 'transient-switch
  :key "-v"
  :argument "--reversed")

(transient-define-argument majutsu-log:--no-graph ()
  :description "Hide graph"
  :class 'transient-switch
  :key "-G"
  :argument "--no-graph")

(transient-define-argument majutsu-log:-- ()
  :description "Limit to filesets"
  :class 'transient-files
  :key "--"
  :argument "--"
  :prompt "Limit to filesets"
  :reader #'majutsu-read-files
  :multi-value t)

;;;###autoload
(transient-define-prefix majutsu-log-transient ()
  "Transient interface for adjusting jj log options."
  :man-page "jj-log"
  :class 'majutsu-log-prefix
  :transient-non-suffix t
  [:description majutsu-log--transient-description
   :class transient-columns
   ["Revisions"
    ("r" "Set revset" majutsu-log-transient-set-revisions
     :description (lambda ()
                    (majutsu-log--value-desc
                     "Set revset"
                     (cadr (majutsu-log--get-value 'majutsu-log-mode 'direct))))
     :transient t)
    (majutsu-log:--limit)
    (majutsu-log:--reversed)
    (majutsu-log:--no-graph)
    ("R" "Clear revset" majutsu-log-transient-clear-revisions
     :if (lambda () (cadr (majutsu-log--get-value 'majutsu-log-mode 'direct)))
     :transient t)
    ]
   ["Paths"
    (majutsu-log:--)]
   ["Actions"
    ("g" "buffer" majutsu-log-transient)
    ("s" "buffer and set defaults" transient-set-and-exit)
    ("w" "buffer and save defaults" transient-save-and-exit)
    ("0" "Reset options" majutsu-log-transient-reset :transient t)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (cond
   ((not (eq transient-current-command 'majutsu-log-transient))
    (transient-setup 'majutsu-log-transient))
   (t
    (unless (derived-mode-p 'majutsu-log-mode)
      (user-error "Not in a Majutsu log buffer"))
    (pcase-let ((`(,args ,filesets) (transient-args transient-current-command)))
      (setq-local majutsu-buffer-log-args args)
      (setq-local majutsu-buffer-log-filesets filesets))
    (majutsu-refresh-buffer))))

;;; _
(provide 'majutsu-log)
;;; majutsu-log.el ends here
