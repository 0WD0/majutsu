;;; majutsu-row.el --- Structured row rendering for Majutsu  -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared row protocol for Majutsu log, evolog, and op-log views.
;; jj renders the graph; Majutsu embeds module tokens inside graph nodes,
;; parses the output back into entries, renders real Magit sections, and
;; provides semantic copy commands for structured entry fields.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magit-section)
(require 'majutsu-base)
(require 'majutsu-template)
(require 'majutsu-section)

;;; Transport constants

(defconst majutsu-row-field-separator "\x1e"
  "Separator inserted between fields inside one module payload.")

(defconst majutsu-row-field-line-separator "\x1f"
  "Encoded newline separator used inside template field payloads.")

(defconst majutsu-row-record-marker "\x1d"
  "Control marker prefix for module boundaries inside graph output.")

(defconst majutsu-row-start-token
  (concat majutsu-row-record-marker "S")
  "Marker that starts an entry and heading payload.")

(defconst majutsu-row-role-token
  (concat majutsu-row-record-marker "R")
  "Marker that brackets the optional row role payload after start.")

(defconst majutsu-row-tail-token
  (concat majutsu-row-record-marker "T")
  "Marker that starts the tail payload.")

(defconst majutsu-row-body-token
  (concat majutsu-row-record-marker "B")
  "Marker that starts the body payload.")

(defconst majutsu-row-meta-token
  (concat majutsu-row-record-marker "M")
  "Marker that starts the metadata payload.")

(defconst majutsu-row-end-token
  (concat majutsu-row-record-marker "E")
  "Marker that terminates an entry.")

(defconst majutsu-row-push-token
  (concat majutsu-row-record-marker "[")
  "Marker that pushes the latest parsed entry as the current parent.")

(defconst majutsu-row-pop-token
  (concat majutsu-row-record-marker "]")
  "Marker that pops the current parent entry.")

(defconst majutsu-row-reset-token
  (concat majutsu-row-record-marker "=")
  "Marker that resets the row parser stack.")

(defconst majutsu-row-module-order '(heading tail body metadata)
  "Module parse/render order for sequential row payloads.")

(defconst majutsu-row--tail-terminal-padding 1
  "Terminal columns reserved to the right of tail-aligned text.")

;;; UI text-property names (stripped during copy)

(defconst majutsu-row--ui-properties
  '(majutsu-row-module
    majutsu-row-field
    majutsu-row-column
    majutsu-row-entry-id
    majutsu-row-role
    majutsu-row-decoration
    majutsu-row-profile
    majutsu-row-tail-spacer
    line-prefix
    wrap-prefix
    display)
  "Text properties removed from copied row text.")

;;; Buffer-local variables

(defvar-local majutsu-row-buffer-compiled nil
  "Compiled row metadata for the current buffer.")

(defvar-local majutsu-row-cached-entries nil
  "Parsed row entries for the current buffer.")

(defvar-local majutsu-row-cached-roots nil
  "Parsed top-level row entries for the current buffer.")

(defvar-local majutsu-row-entry-index nil
  "Hash table mapping stable entry ids to cached entries.")

(defvar-local majutsu-row-section-ident-index nil
  "Hash table mapping Magit section identities to cached entries.")

;;; Compiled helpers

(defun majutsu-row--profile (compiled)
  "Return row profile from COMPILED."
  (plist-get compiled :profile))

;;; Current compiled access

(defun majutsu-row-current-compiled (&optional compiled)
  "Return COMPILED or the current buffer's compiled row metadata."
  (or compiled
      majutsu-row-buffer-compiled
      (user-error "No compiled row data in current buffer")))

;;; Post-decode

(defun majutsu-row-post-decode-line-separator (value &optional _ctx)
  "Decode field line separators inside VALUE."
  (if (stringp value)
      (subst-char-in-string
       (aref majutsu-row-field-line-separator 0)
       ?\n
       value t)
    value))

;;; Profile helpers

(defconst majutsu-row-base-profile
  '(:default-postprocessors nil
    :field-postprocessors nil
    :decode-function majutsu-row-post-decode-line-separator
    :record-field-function nil
    :entry-id-function nil
    :section-class nil
    :section-class-function nil
    :section-value-function nil
    :section-hide nil
    :section-hide-function nil
    :show-child-count :inherit
    :tail-align nil
    :compat-property-prefix nil)
  "Shared defaults for row profiles.")

(defun majutsu-row-make-profile (&rest properties)
  "Return a row profile plist seeded from `majutsu-row-base-profile'.
PROPERTIES are alternating keyword/value pairs that override the defaults."
  (let ((profile (copy-sequence majutsu-row-base-profile)))
    (while properties
      (when (null (cdr properties))
        (user-error "Row profile property %S lacks a value"
                    (car properties)))
      (setq profile (plist-put profile (pop properties) (pop properties))))
    profile))

;;; Default field helpers

(defun majutsu-row--default-postprocessors-for-field (profile field)
  "Return default postprocessors for FIELD using PROFILE."
  (append (plist-get profile :default-postprocessors)
          (alist-get field (plist-get profile :field-postprocessors) nil nil #'eq)))

;;; Column normalization

(defun majutsu-row-column-spec-plist (spec)
  "Return plist representation of column SPEC."
  (cond
   ((and (plistp spec) (plist-get spec :field)) spec)
   ((and (consp spec) (symbolp (car spec)))
    (let ((tail (cdr spec)))
      (cond
       ((plistp tail) (append (list :field (car spec)) tail))
       ((null (cdr tail)) (list :field (car spec) :template (car tail)))
       (t (list :field (car spec) :template tail)))))
   ((symbolp spec) (list :field spec))
   (t (user-error "Invalid column spec: %S" spec))))

(defun majutsu-row-normalize-postprocessors (profile post field)
  "Normalize POST for FIELD using PROFILE into a function list."
  (let* ((defaults (majutsu-row--default-postprocessors-for-field
                    profile field))
         (fns (cond
               ((eq post :default) defaults)
               ((null post) nil)
               ((functionp post) (append defaults (list post)))
               ((and (listp post) (seq-every-p #'functionp post))
                (append defaults post))
               (t (user-error "Column %S has invalid :post %S" field post)))))
    (dolist (fn fns)
      (unless (functionp fn)
        (user-error "Column %S has non-callable postprocessor %S" field fn)))
    fns))

(defun majutsu-row-normalize-column-spec (profile spec)
  "Normalize one column SPEC using PROFILE."
  (let* ((col (majutsu-row-column-spec-plist spec))
         (field (plist-get col :field))
         (module (if (plist-member col :module)
                     (plist-get col :module)
                   (user-error "Column %S requires :module" field)))
         (face (if (plist-member col :face)
                   (plist-get col :face)
                 t))
         (post (if (plist-member col :post)
                   (plist-get col :post)
                 :default))
         (template (plist-get col :template)))
    (when (plist-member col :instance)
      (user-error "Column %S must not declare reserved :instance" field))
    (setq module (if (keywordp module)
                     (intern (substring (symbol-name module) 1))
                   module))
    (unless (memq module majutsu-row-module-order)
      (user-error "Column %S has invalid :module %S" field module))
    (unless (or (eq face t) (null face) (symbolp face))
      (user-error "Column %S has invalid :face %S" field face))
    (append (list :field field
                  :module module
                  :face face
                  :post (majutsu-row-normalize-postprocessors
                         profile post field))
            (when (plist-member col :template)
              (list :template template)))))

;;; Module columns (from compiled)

(defun majutsu-row-module-columns (compiled module)
  "Return compiled column specs for MODULE from COMPILED."
  (alist-get module (plist-get compiled :module-columns) nil nil #'eq))

;;; Column lookup

(defun majutsu-row-column-by-instance (compiled instance)
  "Return column spec from COMPILED identified by INSTANCE.
INSTANCE is a compiler-assigned column occurrence id, not a layout key."
  (seq-find (lambda (column)
              (eql (plist-get column :instance) instance))
            (plist-get compiled :columns)))

(defun majutsu-row-assign-column-instances (columns)
  "Return COLUMNS with compiler-internal occurrence ids assigned."
  (cl-loop for column in columns
           for idx from 0
           collect (plist-put (copy-sequence column) :instance idx)))

;;; Row template lowering

(defun majutsu-row-resolve-template-form (form)
  "Resolve symbolic row template FORM."
  (if (and (symbolp form) (not (keywordp form)) (boundp form))
      (symbol-value form)
    form))

(defun majutsu-row--column-template (column)
  "Return template form for COLUMN."
  (if (plist-member column :template)
      (majutsu-row-resolve-template-form (plist-get column :template))
    (user-error "Column %S has no :template"
                (plist-get column :field))))

(defun majutsu-row-build-module-template-form (templates)
  "Return a template form joining TEMPLATES with field separators."
  (cond
   ((null templates) "")
   ((null (cdr templates)) (car templates))
   (t
    (cons :concat
          (cdr (cl-mapcan (lambda (template)
                            (list majutsu-row-field-separator template))
                          templates))))))

(defun majutsu-row-default-column-template-alist (compiled)
  "Return default column templates for COMPILED."
  (mapcar (lambda (column)
            (cons column (majutsu-row--column-template column)))
          (plist-get compiled :columns)))

(defun majutsu-row--column-template-form (column-templates column)
  "Return COLUMN template from COLUMN-TEMPLATES."
  (if-let* ((cell (assq column column-templates)))
      (cdr cell)
    (user-error "Missing row template for field %S"
                (plist-get column :field))))

(defun majutsu-row-module-template-form (compiled module column-templates)
  "Return template form for COMPILED MODULE using COLUMN-TEMPLATES."
  (majutsu-row-build-module-template-form
   (mapcar (lambda (column)
             (majutsu-row--column-template-form column-templates column))
           (majutsu-row-module-columns compiled module))))

(defun majutsu-row--role-template-form (role)
  "Return transport template form for ROLE."
  (when role
    (unless (symbolp role)
      (user-error "Row role must be a symbol, got %S" role))
    `[:concat ,majutsu-row-role-token ,(symbol-name role)
      ,majutsu-row-role-token]))

(cl-defun majutsu-row-template-form
    (compiled &optional column-templates role body-suffix
              (newline t newline-supplied-p))
  "Return one row template form for COMPILED.
COLUMN-TEMPLATES, when non-nil, must contain every compiled column.  ROLE,
when non-nil, is encoded as compiler-owned row metadata after the start token.
BODY-SUFFIX, when non-nil, is inserted after the body module and before the
metadata module.
When NEWLINE is non-nil, append a trailing newline to the row transport.
This constructs the row transport wrapper from compiled row metadata, so
callers only declare row column values and do not duplicate the row protocol."
  (let ((templates (or column-templates
                       (majutsu-row-default-column-template-alist compiled))))
    (apply #'vector
           (append
            (list :concat majutsu-row-start-token)
            (when role
              (list (majutsu-row--role-template-form role)))
            (list
             (majutsu-row-module-template-form compiled 'heading templates)
             majutsu-row-tail-token
             (majutsu-row-module-template-form compiled 'tail templates)
             majutsu-row-body-token
             (majutsu-row-module-template-form compiled 'body templates))
            (when body-suffix
              (list body-suffix))
            (list
             majutsu-row-meta-token
             (majutsu-row-module-template-form compiled 'metadata templates)
             majutsu-row-end-token)
            (when (or (not newline-supplied-p) newline)
              (list "\n"))))))

(defun majutsu-row--template-concat (&rest forms)
  "Return one template form concatenating FORMS."
  (setq forms (delq nil forms))
  (cond
   ((null forms) "")
   ((null (cdr forms)) (car forms))
   (t (cons :concat forms))))

;;; Layout normalization

(defun majutsu-row--layout-plist-p (value)
  "Return non-nil when VALUE is a row layout plist."
  (and (consp value) (keywordp (car value))))

(defun majutsu-row--layout-normalize-module (module)
  "Return normalized layout MODULE symbol."
  (if (keywordp module)
      (intern (substring (symbol-name module) 1))
    module))

(defun majutsu-row--layout-schema (layout)
  "Return layout-wide column schema plists declared by LAYOUT."
  (when (majutsu-row--layout-plist-p layout)
    (mapcar #'majutsu-row-column-spec-plist
            (plist-get layout :schema))))

(defun majutsu-row--layout-column-attr (column attr)
  "Return normalized layout COLUMN ATTR when present."
  (when (plist-member column attr)
    (let ((value (plist-get column attr)))
      (if (eq attr :module)
          (majutsu-row--layout-normalize-module value)
        value))))

(defun majutsu-row--layout-schema-column-matches-p (column schema-column)
  "Return non-nil when SCHEMA-COLUMN can configure COLUMN."
  (and (eq (plist-get column :field) (plist-get schema-column :field))
       (or (not (plist-member column :module))
           (eq (majutsu-row--layout-column-attr column :module)
               (majutsu-row--layout-column-attr schema-column :module)))))

(defun majutsu-row--layout-schema-column (column schema)
  "Return the unique schema column matching COLUMN."
  (let ((matches (seq-filter
                  (lambda (schema-column)
                    (majutsu-row--layout-schema-column-matches-p
                     column schema-column))
                  schema)))
    (cond
     ((null matches) nil)
     ((null (cdr matches)) (car matches))
     (t (user-error "Ambiguous row layout column %S/%S"
                    (plist-get column :field)
                    (or (majutsu-row--layout-column-attr column :module)
                        '*))))))

(defun majutsu-row--layout-merge-column-spec (spec schema)
  "Merge layout column SPEC with matching SCHEMA defaults."
  (let* ((column (majutsu-row-column-spec-plist spec))
         (schema-column (majutsu-row--layout-schema-column column schema)))
    (append column schema-column)))

(defun majutsu-row--layout-normalize-column (profile spec schema)
  "Return normalized layout column SPEC using PROFILE and SCHEMA."
  (majutsu-row-normalize-column-spec
   profile
   (majutsu-row--layout-merge-column-spec spec schema)))

(defun majutsu-row--layout-column-key (column)
  "Return schema identity key for normalized layout COLUMN."
  (list (plist-get column :field)
        (plist-get column :module)))

(defun majutsu-row--column-key-index (columns)
  "Return hash table indexing COLUMNS by layout column key."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (column columns index)
      (puthash (majutsu-row--layout-column-key column) column index))))

(defun majutsu-row--layout-column-compatible-p (old new)
  "Return non-nil when normalized columns OLD and NEW can share schema."
  (and (eq (plist-get old :face) (plist-get new :face))
       (equal (plist-get old :post) (plist-get new :post))))

(defun majutsu-row--layout-node-columns (node)
  "Return column specs declared by layout NODE."
  (plist-get node :columns))

(defconst majutsu-row--layout-role-property-keys
  '(:entry-id :section-value :section-class :section-hide)
  "Layout node keys that configure a row role.")

(defun majutsu-row--normalize-role (role)
  "Return normalized ROLE symbol."
  (cond
   ((null role) nil)
   ((keywordp role) (intern (substring (symbol-name role) 1)))
   ((symbolp role) role)
   (t (user-error "Invalid row role %S" role))))

(defun majutsu-row--layout-node-role (node)
  "Return normalized role declared by layout NODE, or nil."
  (majutsu-row--normalize-role (plist-get node :role)))

(defun majutsu-row--layout-node-role-properties-p (node)
  "Return non-nil if NODE declares role-only properties."
  (seq-some (lambda (key) (plist-member node key))
            majutsu-row--layout-role-property-keys))

(defun majutsu-row--layout-node-role-spec (node)
  "Return role spec declared by canonical layout NODE, or nil."
  (let ((role (majutsu-row--layout-node-role node)))
    (when (and (not role)
               (majutsu-row--layout-node-role-properties-p node))
      (user-error "Row layout node declares role properties without :role"))
    (when role
      (append (list :role role)
              (cl-mapcan (lambda (key)
                           (when (plist-member node key)
                             (list key (plist-get node key))))
                         majutsu-row--layout-role-property-keys)))))

(defun majutsu-row--layout-normalize-children (profile children schema)
  "Return canonical CHILDREN group using PROFILE and SCHEMA."
  (when children
    (let* ((group (if (majutsu-row--layout-plist-p children)
                      (copy-sequence children)
                    (list :nodes children)))
           (nodes (mapcar (lambda (node)
                            (majutsu-row--layout-normalize-node
                             profile node schema))
                          (plist-get group :nodes))))
      (plist-put group :nodes nodes))))

(defun majutsu-row--layout-normalize-node (profile node schema)
  "Return canonical layout NODE using PROFILE and SCHEMA."
  (unless (majutsu-row--layout-plist-p node)
    (user-error "Invalid row layout node %S" node))
  (let ((out (copy-sequence node)))
    (setq out (plist-put out :columns
                         (mapcar (lambda (spec)
                                   (majutsu-row--layout-normalize-column
                                    profile spec schema))
                                 (majutsu-row--layout-node-columns node))))
    (if-let* ((role (majutsu-row--layout-node-role node)))
        (setq out (plist-put out :role role))
      (when (majutsu-row--layout-node-role-properties-p node)
        (user-error "Row layout node declares role properties without :role")))
    (if-let* ((children (majutsu-row--layout-normalize-children
                         profile (plist-get node :children) schema)))
        (plist-put out :children children)
      out)))

;;; Layout template lowering

(defun majutsu-row--layout-column-by-key (compiled key)
  "Return compiled layout column identified by KEY."
  (gethash key (plist-get compiled :column-key-index)))

(defun majutsu-row--layout-declared-column-templates (compiled node)
  "Return templates declared directly by canonical layout NODE."
  (let ((seen (make-hash-table :test #'equal))
        templates)
    (dolist (node-column (majutsu-row--layout-node-columns node))
      (let* ((key (majutsu-row--layout-column-key node-column))
             (column (majutsu-row--layout-column-by-key compiled key)))
        (unless column
          (user-error "Unknown row layout column %S" key))
        (when (gethash key seen)
          (user-error "Duplicate row layout column %S" key))
        (puthash key t seen)
        (push (cons column (majutsu-row--column-template node-column))
              templates)))
    (nreverse templates)))

(defun majutsu-row--layout-column-templates (compiled node)
  "Return complete column template alist for layout NODE."
  (let ((declared (majutsu-row--layout-declared-column-templates
                   compiled node)))
    (mapcar (lambda (column)
              (cons column
                    (if-let* ((cell (assq column declared)))
                        (cdr cell)
                      "")))
            (plist-get compiled :columns))))

(defun majutsu-row--layout-node-list-template-form (compiled nodes)
  "Return template form for layout NODES using COMPILED."
  (apply #'majutsu-row--template-concat
         (mapcar (lambda (node)
                   (majutsu-row-layout-node-template-form compiled node t))
                 nodes)))

(defun majutsu-row--layout-children-template-form (compiled children)
  "Return child-section template form for layout CHILDREN."
  (when children
    (let* ((group (if (majutsu-row--layout-plist-p children)
                      children
                    (list :nodes children)))
           (nodes (plist-get group :nodes))
           (when-form (plist-get group :when))
           (child-form (majutsu-row--layout-node-list-template-form
                        compiled nodes)))
      (unless (equal child-form "")
        (let ((form (majutsu-row--template-concat
                     majutsu-row-push-token
                     child-form
                     majutsu-row-pop-token)))
          (if when-form
              `[:if ,when-form ,form ""]
            form))))))

(defun majutsu-row--layout-single-node-template-form (compiled node &optional inline)
  "Return template form for one layout NODE without :each expansion.
When INLINE is non-nil, omit the trailing newline for the rendered node."
  (let* ((children-form (majutsu-row--layout-children-template-form
                         compiled (plist-get node :children)))
         (row-form (majutsu-row-template-form
                    compiled
                    (majutsu-row--layout-column-templates compiled node)
                    (majutsu-row--layout-node-role node)
                    children-form
                    (not inline))))
    row-form))

(defun majutsu-row-layout-node-template-form (compiled node &optional inline)
  "Return template form for layout NODE using COMPILED.
When INLINE is non-nil, omit the trailing newline for the rendered node."
  (unless (majutsu-row--layout-plist-p node)
    (user-error "Invalid row layout node %S" node))
  (let ((each-form (plist-get node :each))
        (when-form (plist-get node :when))
        (var (or (plist-get node :as) 'it)))
    (unless (symbolp var)
      (user-error "Row layout :as must be a symbol, got %S" var))
    (let ((form (if each-form
                    `[:method ,each-form
                      :map [:lambda (,var)
                                    ,(majutsu-row--layout-single-node-template-form
                                      compiled node inline)]
                      :join ""]
                  (majutsu-row--layout-single-node-template-form
                   compiled node inline))))
      (if when-form
          `[:if ,when-form ,form ""]
        form))))

(defun majutsu-row-layout-template-form (compiled layout)
  "Return template form for COMPILED from declarative row LAYOUT."
  (majutsu-row-layout-node-template-form compiled layout))

(defun majutsu-row--profile-layout (profile)
  "Return declarative row layout for PROFILE, if any."
  (when-let* ((layout-var (plist-get profile :layout-var)))
    (unless (boundp layout-var)
      (user-error "Row layout variable %S is unbound" layout-var))
    (symbol-value layout-var)))

;;; Layout column collection

(defun majutsu-row--layout-children-nodes (node)
  "Return child nodes declared by layout NODE."
  (let ((children (plist-get node :children)))
    (cond
     ((null children) nil)
     ((and (majutsu-row--layout-plist-p children)
           (plist-member children :nodes))
      (plist-get children :nodes))
     ((listp children) children)
     (t (user-error "Invalid row layout children %S" children)))))

(defun majutsu-row--layout-add-column (column seen)
  "Add normalized COLUMN to SEEN and return it if new."
  (let* ((key (majutsu-row--layout-column-key column))
         (old (gethash key seen)))
    (if old
        (progn
          (unless (majutsu-row--layout-column-compatible-p old column)
            (user-error "Conflicting row layout column %S" key))
          nil)
      (puthash key column seen)
      column)))

(defun majutsu-row--layout-collect-columns (node seen)
  "Collect normalized columns from canonical layout NODE."
  (let (columns)
    (dolist (column (majutsu-row--layout-node-columns node))
      (when-let* ((new-column (majutsu-row--layout-add-column
                               column seen)))
        (push new-column columns)))
    (setq columns (nreverse columns))
    (dolist (child (majutsu-row--layout-children-nodes node))
      (setq columns
            (append columns
                    (majutsu-row--layout-collect-columns
                     child seen))))
    columns))

(defun majutsu-row--layout-columns (schema layout)
  "Return normalized columns declared by SCHEMA and canonical LAYOUT."
  (let ((seen (make-hash-table :test #'equal))
        columns)
    (dolist (column schema)
      (when-let* ((new-column (majutsu-row--layout-add-column
                               column seen)))
        (push new-column columns)))
    (append (nreverse columns)
            (majutsu-row--layout-collect-columns layout seen))))

(defun majutsu-row--layout-role-compatible-p (old new)
  "Return non-nil when role specs OLD and NEW agree."
  (equal old new))

(defun majutsu-row--layout-add-role (role-spec seen)
  "Add ROLE-SPEC to SEEN and return it if new."
  (when role-spec
    (let* ((role (plist-get role-spec :role))
           (old (gethash role seen)))
      (if old
          (progn
            (unless (majutsu-row--layout-role-compatible-p old role-spec)
              (user-error "Conflicting row role %S" role))
            nil)
        (puthash role role-spec seen)
        role-spec))))

(defun majutsu-row--layout-collect-roles (node seen)
  "Collect role specs from canonical layout NODE."
  (let (roles)
    (when-let* ((role-spec (majutsu-row--layout-add-role
                            (majutsu-row--layout-node-role-spec node)
                            seen)))
      (push role-spec roles))
    (dolist (child (majutsu-row--layout-children-nodes node))
      (setq roles (append roles
                          (majutsu-row--layout-collect-roles child seen))))
    roles))

(defun majutsu-row--layout-roles (layout)
  "Return normalized role specs declared by canonical LAYOUT."
  (majutsu-row--layout-collect-roles layout (make-hash-table :test #'eq)))

(defun majutsu-row--role-index (roles)
  "Return hash table indexing ROLES by role symbol."
  (let ((index (make-hash-table :test #'eq)))
    (dolist (role-spec roles index)
      (puthash (plist-get role-spec :role) role-spec index))))

;;; Layout compilation

(defun majutsu-row--module-columns-alist (columns)
  "Return module-to-columns alist for COLUMNS."
  (mapcar (lambda (module)
            (cons module
                  (seq-filter (lambda (column)
                                (eq (plist-get column :module) module))
                              columns)))
          majutsu-row-module-order))

(defun majutsu-row--normalize-layout-schema (profile schema)
  "Return normalized layout SCHEMA using PROFILE."
  (mapcar (lambda (spec)
            (majutsu-row-normalize-column-spec profile spec))
          schema))

(defun majutsu-row--compile-layout-metadata (profile schema layout)
  "Compile PROFILE SCHEMA and canonical LAYOUT into row metadata."
  (let* ((columns (majutsu-row-assign-column-instances
                   (majutsu-row--layout-columns schema layout)))
         (roles (majutsu-row--layout-roles layout)))
    (list :profile profile
          :columns columns
          :module-columns (majutsu-row--module-columns-alist columns)
          :column-key-index (majutsu-row--column-key-index columns)
          :roles roles
          :role-index (majutsu-row--role-index roles)
          :layout-schema schema
          :layout layout)))

(defun majutsu-row-compile (profile)
  "Compile PROFILE layout into jj template and row metadata."
  (let* ((raw-layout (or (majutsu-row--profile-layout profile)
                         (user-error "Row profile %S has no :layout-var"
                                     (plist-get profile :name))))
         (raw-schema (majutsu-row--layout-schema raw-layout))
         (schema (majutsu-row--normalize-layout-schema profile raw-schema))
         (layout (majutsu-row--layout-normalize-node
                  profile raw-layout raw-schema))
         (compiled (majutsu-row--compile-layout-metadata
                    profile schema layout))
         (template-form (majutsu-row-layout-template-form compiled layout))
         (template (majutsu-template-compile
                    template-form (plist-get profile :self-type))))
    (plist-put compiled :template template)))

;;; String splitting/joining

(defun majutsu-row-split-by-separator (value separator)
  "Split VALUE by one-character SEPARATOR, preserving empty fields."
  (majutsu--split-fields value separator))

(defun majutsu-row-join-lines (lines)
  "Join LINES with literal newlines, preserving string properties."
  (if (null lines)
      ""
    (let ((out (car lines)))
      (dolist (line (cdr lines) out)
        (setq out (concat out "\n" line))))))

;;; Parse helpers

(defun majutsu-row-line-token-position (token bol eol &optional start)
  "Return start position of TOKEN between BOL and EOL, or nil."
  (save-excursion
    (goto-char (or start bol))
    (when (search-forward token eol t)
      (- (point) (length token)))))

(defun majutsu-row--parse-role-at (pos eol)
  "Parse optional role payload at POS before EOL.
Return (ROLE . NEXT-POS) when a role is present, otherwise nil."
  (let ((token-len (length majutsu-row-role-token)))
    (when (and (<= (+ pos token-len) eol)
               (equal (buffer-substring pos (+ pos token-len))
                      majutsu-row-role-token))
      (let* ((role-start (+ pos token-len))
             (role-end (majutsu-row-line-token-position
                        majutsu-row-role-token pos eol role-start)))
        (unless role-end
          (user-error "Incomplete row role marker"))
        (let ((role-name (buffer-substring-no-properties
                          role-start role-end)))
          (cons (unless (string-empty-p role-name)
                  (intern role-name))
                (+ role-end token-len)))))))

(defun majutsu-row--parse-inline-child-stream-at-point (compiled start end)
  "Parse inline child stream from START to END using COMPILED.
Return a plist with :children and :end-pos when the stream is well-formed,
or nil otherwise."
  (save-excursion
    (goto-char start)
    (catch 'done
      (let (children)
        (while (< (point) end)
          (cond
           ((majutsu-row--looking-at-token-p majutsu-row-pop-token)
            (forward-char (length majutsu-row-pop-token))
            (throw 'done (list :children (nreverse children)
                               :end-pos (point))))
           ((majutsu-row--looking-at-token-p majutsu-row-start-token)
            (if-let* ((parsed (majutsu-row--parse-entry-record-at-point
                               compiled (point))))
                (let ((entry (car parsed))
                      (record-end (cdr parsed)))
                  (push entry children)
                  (goto-char record-end))
              (throw 'done nil)))
           ((looking-at-p "[ \t\n]+")
            (skip-chars-forward " \t\n"))
           (t
            (throw 'done nil)))))
      nil)))

(defun majutsu-row-parse-trailing-payloads (compiled tail-pos eol)
  "Parse tail, body, child stream, and metadata from the current line.
TAIL-POS points at the tail token and EOL is the end of the physical line
containing the row.  Return a plist with module payloads, :children, and
:record-end when the row is well-formed."
  (save-excursion
    (goto-char tail-pos)
    (when (majutsu-row--looking-at-token-p majutsu-row-tail-token)
      (let* ((tail-start (+ tail-pos (length majutsu-row-tail-token)))
             (body-pos (majutsu-row-line-token-position
                        majutsu-row-body-token tail-start eol tail-start)))
        (when body-pos
          (let* ((body-start (+ body-pos (length majutsu-row-body-token)))
                 (push-pos (majutsu-row-line-token-position
                            majutsu-row-push-token body-start eol body-start))
                 (meta-pos (majutsu-row-line-token-position
                            majutsu-row-meta-token body-start eol body-start))
                 (child-result (and push-pos
                                    (or (null meta-pos) (< push-pos meta-pos))
                                    (majutsu-row--parse-inline-child-stream-at-point
                                     compiled
                                     (+ push-pos (length majutsu-row-push-token))
                                     eol)))
                 (body-end (if child-result
                               push-pos
                             meta-pos))
                 (metadata-start (if child-result
                                     (plist-get child-result :end-pos)
                                   meta-pos))
                 (meta-end (and metadata-start
                                (save-excursion
                                  (goto-char metadata-start)
                                  (when (majutsu-row--looking-at-token-p
                                         majutsu-row-meta-token)
                                    (+ metadata-start
                                       (length majutsu-row-meta-token))))))
                 (end-pos (and meta-end
                               (majutsu-row-line-token-position
                                majutsu-row-end-token meta-end eol meta-end))))
            (when (and body-end metadata-start end-pos)
              (list :tail (buffer-substring tail-start body-pos)
                    :body (buffer-substring body-start body-end)
                    :children (plist-get child-result :children)
                    :metadata (buffer-substring meta-end end-pos)
                    :record-end (+ end-pos (length majutsu-row-end-token))))))))))

(defun majutsu-row-split-module-values (payload count)
  "Split PAYLOAD into COUNT field values."
  (if (<= count 0)
      nil
    (let ((values (majutsu-row-split-by-separator
                   (or payload "")
                   majutsu-row-field-separator)))
      (cond
       ((< (length values) count)
        (append values (make-list (- count (length values)) "")))
       ((> (length values) count)
        (seq-take values count))
       (t values)))))

;;; Record helpers

(defun majutsu-row--decode-transport-value (profile value)
  "Decode transport-level escapes in VALUE using PROFILE."
  (let ((fn (or (plist-get profile :decode-function)
                #'majutsu-row-post-decode-line-separator)))
    (funcall fn value)))

(defun majutsu-row-apply-postprocessor (fn value ctx)
  "Apply postprocessor FN to VALUE with CTX."
  (condition-case err
      (condition-case _
          (funcall fn value ctx)
        (wrong-number-of-arguments
         (funcall fn value)))
    (error
     (message "majutsu row postprocessor failed (%S on %S): %s"
              fn (plist-get ctx :field) (error-message-string err))
     value)))

(defun majutsu-row-apply-postprocessors (value postprocessors ctx)
  "Apply POSTPROCESSORS to VALUE with CTX sequentially."
  (let ((out value))
    (dolist (fn postprocessors out)
      (setq out (majutsu-row-apply-postprocessor fn out ctx)))))

(defun majutsu-row--canonical-field-value (profile field value)
  "Return canonical FIELD VALUE using PROFILE postprocessors."
  (majutsu-row-apply-postprocessors
   value
   (majutsu-row--default-postprocessors-for-field profile field)
   (list :field field :module 'canonical :raw-value value :canonical t)))

(defun majutsu-row-record-canonical-field (entry field value)
  "Record canonical FIELD VALUE onto ENTRY."
  (let ((columns (plist-get entry :columns)))
    (setf (alist-get field columns nil nil #'eq) value)
    (plist-put entry :columns columns)))

(defun majutsu-row--record-field (profile entry field value)
  "Record FIELD VALUE on ENTRY using PROFILE."
  (if-let* ((fn (plist-get profile :record-field-function)))
      (funcall fn entry field value)
    (majutsu-row-record-canonical-field entry field value)))

(defun majutsu-row-record-column-value (entry column value)
  "Record per-instance VALUE for COLUMN onto ENTRY."
  (let* ((instance (plist-get column :instance))
         (column-values (plist-get entry :column-values)))
    (when instance
      (setf (alist-get instance column-values nil nil #'eql) value)
      (setq entry (plist-put entry :column-values column-values)))
    entry))

(defun majutsu-row-record-module-fields (entry module payload compiled)
  "Record MODULE PAYLOAD values into ENTRY using COMPILED layout."
  (let* ((profile (majutsu-row--profile compiled))
         (columns (majutsu-row-module-columns compiled module))
         (values (majutsu-row-split-module-values payload (length columns)))
         (stored nil))
    (cl-loop for column in columns
             for raw-value in values
             do (let* ((field (plist-get column :field))
                       (decoded (majutsu-row--decode-transport-value
                                 profile raw-value))
                       (canonical (majutsu-row--canonical-field-value
                                   profile field decoded))
                       (base-ctx (list :field field
                                       :module module
                                       :column column
                                       :entry entry
                                       :raw-value decoded
                                       :canonical-value canonical))
                       (out (majutsu-row-apply-postprocessors
                             decoded (plist-get column :post) base-ctx))
                       (record-value (if (plist-get profile :record-field-function)
                                         canonical
                                       out)))
                  (setq entry (majutsu-row--record-field
                               profile entry field record-value))
                  (setq entry (majutsu-row-record-column-value
                               entry column out))
                  (push out stored)))
    (let ((modules (plist-get entry :modules)))
      (setf (alist-get module modules nil nil #'eq) (nreverse stored))
      (setq entry (plist-put entry :modules modules)))
    entry))

(defun majutsu-row--collect-entry-tree (entry)
  "Return ENTRY and all descendants in preorder."
  (cons entry
        (apply #'append
               (mapcar #'majutsu-row--collect-entry-tree
                       (plist-get entry :children)))))

(defun majutsu-row--rebase-entry-tree (entry parent depth)
  "Rebase ENTRY and descendants onto PARENT at DEPTH."
  (plist-put entry :parent parent)
  (plist-put entry :depth depth)
  (plist-put entry :children
             (mapcar (lambda (child)
                       (majutsu-row--rebase-entry-tree child entry (1+ depth)))
                     (plist-get entry :children)))
  entry)

;;; Parse

(defun majutsu-row--looking-at-token-p (token)
  "Return non-nil when point is at TOKEN."
  (and (stringp token)
       (looking-at-p (regexp-quote token))))

(defun majutsu-row--event-at-point ()
  "Return (KIND . TOKEN) for the row protocol event at point."
  (seq-some
   (pcase-lambda (`(,token . ,kind))
     (and (majutsu-row--looking-at-token-p token)
          (cons kind token)))
   `((,majutsu-row-start-token . row)
     (,majutsu-row-push-token . push)
     (,majutsu-row-pop-token . pop)
     (,majutsu-row-reset-token . reset))))

(defun majutsu-row--next-marker-position (start end)
  "Return next row marker position between START and END, or nil."
  (save-excursion
    (goto-char start)
    (when-let* ((found (search-forward majutsu-row-record-marker end t)))
      (1- found))))

(defun majutsu-row--suffix-lines-from-region (beg end)
  "Return suffix lines represented by raw text from BEG to END."
  (when (< beg end)
    (let ((text (buffer-substring beg end)))
      (when (string-match-p "\n" text)
        (when (string-prefix-p "\n" text)
          (setq text (substring text 1)))
        (when (string-suffix-p "\n" text)
          (setq text (substring text 0 -1)))
        (unless (string-empty-p text)
          (majutsu-row-split-by-separator text "\n"))))))

(defun majutsu-row--append-suffix-lines (entry lines)
  "Append LINES to ENTRY's suffix lines."
  (when lines
    (plist-put entry :suffix-lines
               (append (plist-get entry :suffix-lines) lines)))
  entry)

(defun majutsu-row--finalize-entry-gap (entry gap-beg gap-end next-marker)
  "Finalize ENTRY's inter-event text from GAP-BEG to GAP-END.
When NEXT-MARKER is non-nil, exclude the graph/carrier prefix before that
marker from ENTRY's source span and suffix lines."
  (when entry
    (let* ((entry-end
            (if next-marker
                (let ((line-beg (save-excursion
                                  (goto-char next-marker)
                                  (line-beginning-position))))
                  (if (> line-beg gap-beg)
                      line-beg
                    next-marker))
              gap-end))
           (suffix-lines (majutsu-row--suffix-lines-from-region
                          gap-beg entry-end)))
      (majutsu-row--append-suffix-lines entry suffix-lines)
      (plist-put entry :end entry-end)))
  entry)

(defun majutsu-row--parse-entry-record-at-point (compiled &optional stream-start)
  "Parse one row record at point using COMPILED.
When STREAM-START is non-nil, treat it as the current stream origin for prefix
calculation while keeping the physical line end as the record boundary.
Return (ENTRY . RECORD-END), where RECORD-END is just after the end token."
  (let* ((entry-beg (or stream-start (line-beginning-position)))
         (bol entry-beg)
         (eol (line-end-position))
         (start-pos (if (majutsu-row--looking-at-token-p
                         majutsu-row-start-token)
                        (point)
                      (majutsu-row-line-token-position
                       majutsu-row-start-token bol eol))))
    (when start-pos
      (let* ((indent (- start-pos bol))
             (role nil)
             (heading-prefixes nil)
             (heading-segments nil)
             (record-end nil)
             (done nil)
             (first-line t))
        (save-excursion
          (goto-char start-pos)
          (while (and (not done) (not (eobp)))
            (setq bol (line-beginning-position)
                  eol (line-end-position))
            (let* ((prefix-end (min (+ bol indent) eol))
                   (prefix (buffer-substring bol prefix-end))
                   (content-start (if first-line
                                      (+ start-pos
                                         (length majutsu-row-start-token))
                                    prefix-end)))
              (when first-line
                (when-let* ((parsed-role (majutsu-row--parse-role-at
                                          content-start eol)))
                  (setq role (car parsed-role)
                        content-start (cdr parsed-role))))
              (let ((segment-pos (majutsu-row-line-token-position
                                  majutsu-row-tail-token
                                  bol eol content-start)))
                (if segment-pos
                    (progn
                      (push prefix heading-prefixes)
                      (push (buffer-substring content-start segment-pos)
                            heading-segments)
                      (when-let* ((payloads (majutsu-row-parse-trailing-payloads
                                             compiled segment-pos eol)))
                        (setq record-end (plist-get payloads :record-end))
                        (setq done payloads)))
                  (push prefix heading-prefixes)
                  (push (buffer-substring content-start eol) heading-segments)
                  (forward-line 1)
                  (when (eobp)
                    (setq done :incomplete)))))
            (setq first-line nil)))
        (when (plistp done)
          (let* ((profile (majutsu-row--profile compiled))
                 (entry (list :beg entry-beg
                              :record-end record-end
                              :end record-end
                              :row-profile (plist-get profile :name)
                              :role role
                              :indent indent
                              :columns nil
                              :column-values nil
                              :modules nil
                              :heading-prefixes (nreverse heading-prefixes)
                              :suffix-lines nil
                              :children nil
                              :parent nil
                              :depth 0))
                 (heading-payload
                  (majutsu-row-join-lines
                   (nreverse heading-segments))))
            (setq entry (majutsu-row-record-module-fields
                         entry 'heading heading-payload compiled))
            (setq entry (majutsu-row-record-module-fields
                         entry 'tail (plist-get done :tail) compiled))
            (setq entry (majutsu-row-record-module-fields
                         entry 'body (plist-get done :body) compiled))
            (setq entry (majutsu-row-record-module-fields
                         entry 'metadata (plist-get done :metadata) compiled))
            (when-let* ((children (plist-get done :children)))
              (setq entry (plist-put entry :children
                                     (mapcar (lambda (child)
                                               (majutsu-row--rebase-entry-tree
                                                child entry (1+ (plist-get entry :depth))))
                                             children))))
            (cons entry record-end)))))))

(defun majutsu-row--attach-entry (entry stack)
  "Attach ENTRY under STACK's top parent, or leave it as a root entry."
  (if-let* ((parent (car stack)))
      (progn
        (plist-put entry :parent parent)
        (plist-put entry :depth (1+ (or (plist-get parent :depth) 0)))
        (plist-put parent :children
                   (append (plist-get parent :children) (list entry)))
        nil)
    (plist-put entry :parent nil)
    (plist-put entry :depth 0)
    entry))

(defun majutsu-row-read-region (compiled beg end &optional _options)
  "Read row protocol events between BEG and END using COMPILED.
Return a plist with :roots, :entries, and :diagnostics."
  (let (roots entries diagnostics suffix-owner)
    (save-excursion
      (let ((cursor beg))
        (while (< cursor end)
          (if-let* ((marker (majutsu-row--next-marker-position cursor end)))
              (progn
                (majutsu-row--finalize-entry-gap
                 suffix-owner cursor marker marker)
                (goto-char marker)
                (pcase-let ((`(,kind . ,token)
                             (or (majutsu-row--event-at-point)
                                 (cons 'unknown majutsu-row-record-marker))))
                  (pcase kind
                    ('row
                     (if-let* ((parsed (majutsu-row--parse-entry-record-at-point
                                        compiled)))
                         (let ((entry (car parsed))
                               (record-end (cdr parsed)))
                           (push entry roots)
                           (setq entries (nconc entries
                                                (majutsu-row--collect-entry-tree
                                                 entry)))
                           (setq suffix-owner entry
                                 cursor record-end))
                       (push (list :position marker
                                   :message "Incomplete row record")
                             diagnostics)
                       (setq suffix-owner nil
                             cursor (+ marker (length token)))))
                    ('push
                     (push (list :position marker
                                 :message "Row push without previous entry")
                           diagnostics)
                     (setq suffix-owner nil
                           cursor (+ marker (length token))))
                    ('pop
                     (push (list :position marker
                                 :message "Row pop without parent")
                           diagnostics)
                     (setq suffix-owner nil
                           cursor (+ marker (length token))))
                    ('reset
                     (setq suffix-owner nil
                           cursor (+ marker (length token))))
                    (_
                     (push (list :position marker
                                 :message "Unknown row protocol marker")
                           diagnostics)
                     (setq suffix-owner nil
                           cursor (min end (1+ marker)))))))
            (majutsu-row--finalize-entry-gap suffix-owner cursor end nil)
            (setq cursor end)))))
    (list :roots (nreverse roots)
          :entries entries
          :diagnostics (nreverse diagnostics))))

(defun majutsu-row--diagnostic-position (diagnostic)
  "Return buffer position described by row parser DIAGNOSTIC."
  (or (plist-get diagnostic :position)
      (when-let* ((entry (plist-get diagnostic :entry)))
        (plist-get entry :beg))))

(defun majutsu-row--format-diagnostic (diagnostic)
  "Return a one-line message for row parser DIAGNOSTIC."
  (let ((message (or (plist-get diagnostic :message)
                     "Unknown row parser diagnostic"))
        (position (majutsu-row--diagnostic-position diagnostic)))
    (if position
        (format "%s at %d" message position)
      message)))

(defun majutsu-row-report-diagnostics (diagnostics)
  "Report row parser DIAGNOSTICS in the echo area."
  (when diagnostics
    (message "majutsu row parser: %s%s"
             (majutsu-row--format-diagnostic (car diagnostics))
             (if (cdr diagnostics)
                 (format " (+%d more)" (1- (length diagnostics)))
               ""))))

(defun majutsu-row-read-buffer (compiled &optional options)
  "Read the current buffer as a row protocol stream using COMPILED."
  (majutsu-row-read-region compiled (point-min) (point-max) options))

(defun majutsu-row-parse-at-point (compiled)
  "Parse one sequentially encoded row entry at point using COMPILED."
  (let* ((entry-beg (line-beginning-position))
         (start-pos (majutsu-row-line-token-position
                     majutsu-row-start-token entry-beg (line-end-position))))
    (when start-pos
      (seq-find (lambda (entry)
                  (= (plist-get entry :beg) entry-beg))
                (plist-get (majutsu-row-read-region
                            compiled entry-beg (point-max))
                           :entries)))))

(defun majutsu-row-parse-buffer (compiled)
  "Parse all sequentially encoded row entries in current buffer."
  (plist-get (majutsu-row-read-buffer compiled) :entries))

;;; Line prefix/insertion helpers

(defun majutsu-row-apply-line-prefix-span
    (start end line-prefix-str &optional wrap-prefix-str)
  "Apply display-only prefix strings to START..END."
  (when (< start end)
    (add-text-properties
     start end
     (list 'line-prefix (or line-prefix-str "")
           'wrap-prefix (or wrap-prefix-str line-prefix-str "")))))

(defun majutsu-row-insert-prefixed-line (content prefix)
  "Insert CONTENT as one line with display-only PREFIX."
  (let ((start (point)))
    (insert (or content "") "\n")
    (majutsu-row-apply-line-prefix-span start (point) prefix)))

(defun majutsu-row-split-prefix-line (line prefix-width)
  "Split LINE into (PREFIX . CONTENT) using PREFIX-WIDTH characters."
  (let* ((width (max 0 (min (or prefix-width 0) (length (or line "")))))
         (text (or line "")))
    (cons (substring text 0 width)
          (substring text width))))

;;; Column access

(defun majutsu-row-column (entry field)
  "Return canonical value for FIELD stored on ENTRY."
  (alist-get field (plist-get entry :columns) nil nil #'eq))

(defun majutsu-row-column-value (entry column)
  "Return per-instance value for COLUMN stored on ENTRY."
  (let* ((instance (plist-get column :instance))
         (column-values (plist-get entry :column-values))
         (missing (make-symbol "majutsu-row-missing-instance"))
         (value (if instance
                    (alist-get instance column-values missing nil #'eql)
                  missing)))
    (if (eq value missing)
        (majutsu-row-column entry (plist-get column :field))
      value)))

;;; Role access

(defun majutsu-row-role (entry)
  "Return ENTRY's row role."
  (plist-get entry :role))

(defun majutsu-row-role-spec (entry compiled)
  "Return role spec for ENTRY in COMPILED."
  (when-let* ((role (majutsu-row-role entry)))
    (gethash role (plist-get compiled :role-index))))

(defun majutsu-row--call-entry-function (fn entry compiled)
  "Call FN with ENTRY and COMPILED, accepting one-argument functions."
  (condition-case _
      (funcall fn entry compiled)
    (wrong-number-of-arguments
     (funcall fn entry))))

(defun majutsu-row-resolve-role-ref (ref entry compiled)
  "Resolve role REF on ENTRY."
  (cond
   ((null ref) nil)
   ((keywordp ref)
    (majutsu-row-column entry (intern (substring (symbol-name ref) 1))))
   ((symbolp ref)
    (majutsu-row-column entry ref))
   ((functionp ref)
    (majutsu-row--call-entry-function ref entry compiled))
   ((and (consp ref) (eq (car ref) 'function))
    (majutsu-row--call-entry-function (cadr ref) entry compiled))
   (t (user-error "Invalid row role ref %S" ref))))

;;; Display helpers

(defun majutsu-row-display-string (value)
  "Return VALUE converted to a display string."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((listp value) (mapconcat #'majutsu-row-display-string value " "))
   (t (format "%s" value))))

(defun majutsu-row-single-line-string (value)
  "Return VALUE flattened to a single display line."
  (if (stringp value)
      (string-trim (replace-regexp-in-string "[\n\r]+" " " value nil t))
    value))

(defun majutsu-row-concat-heading-parts (parts)
  "Concatenate heading PARTS without spaces after newlines."
  (let ((out ""))
    (dolist (part parts out)
      (unless (string-empty-p part)
        (let ((need-space
               (and (> (length out) 0)
                    (not (eq (aref out (1- (length out))) ?\n))
                    (not (eq (aref part 0) ?\n)))))
          (setq out (concat out (if need-space " " "") part)))))))

;;; Face helpers

(defun majutsu-row-apply-face-policy (value face)
  "Apply FACE policy to VALUE and return display string."
  (let ((text (or value "")))
    (cond
     ((eq face t) text)
     ((null face) (substring-no-properties text))
     (t (propertize (substring-no-properties text) 'font-lock-face face)))))

;;; Compat property helpers

(defun majutsu-row--compat-prefixes (compiled)
  "Return compatibility property prefixes for COMPILED."
  (let* ((profile (majutsu-row--profile compiled))
         (prefixes (or (plist-get profile :compat-property-prefixes)
                       (plist-get profile :compat-property-prefix))))
    (ensure-list prefixes)))

(defun majutsu-row--property-symbol (prefix suffix)
  "Return property symbol PREFIX-SUFFIX."
  (intern (format "%s-%s" prefix suffix)))

(defun majutsu-row--append-compat-properties (props compiled values)
  "Append compatibility property VALUES to PROPS for COMPILED."
  (dolist (prefix (majutsu-row--compat-prefixes compiled) props)
    (while values
      (let ((suffix (pop values))
            (value (pop values)))
        (setq props
              (append props
                      (list (majutsu-row--property-symbol prefix suffix)
                            value)))))))

;;; Content/deco properties

(defun majutsu-row-content-properties
    (compiled entry-id module &optional column)
  "Return content properties for ENTRY-ID in MODULE."
  (let ((props `(majutsu-row-profile
                 ,(plist-get (majutsu-row--profile compiled) :name)
                 majutsu-row-module ,module
                 majutsu-row-entry-id ,entry-id)))
    (when column
      (setq props (append props
                          `(majutsu-row-field ,(plist-get column :field)
                            majutsu-row-column ,(plist-get column :instance)))))
    (majutsu-row--append-compat-properties
     props compiled
     (append (list 'module module 'entry-id entry-id)
             (when column
               (list 'field (plist-get column :field)
                     'column (plist-get column :instance)))))))

(defun majutsu-row-decoration-properties
    (compiled entry-id module decoration)
  "Return decoration properties for ENTRY-ID in MODULE."
  (majutsu-row--append-compat-properties
   (append
    `(majutsu-row-profile
      ,(plist-get (majutsu-row--profile compiled) :name)
      majutsu-row-module ,module
      majutsu-row-entry-id ,entry-id
      majutsu-row-decoration ,decoration))
   compiled
   (list 'module module 'entry-id entry-id 'decoration decoration)))

(defun majutsu-row-tail-spacer-properties (compiled entry-id display)
  "Return tail spacer properties for ENTRY-ID and DISPLAY."
  (majutsu-row--append-compat-properties
   (append
    `(majutsu-row-module tail
      majutsu-row-entry-id ,entry-id
      majutsu-row-decoration tail-spacer
      majutsu-row-tail-spacer t
      display ,display))
   compiled
   (list 'module 'tail
         'entry-id entry-id
         'decoration 'tail-spacer
         'tail-spacer t)))

;;; Propertize helpers

(defun majutsu-row-propertize-content
    (text compiled entry-id module &optional column)
  "Return TEXT tagged as MODULE content for ENTRY-ID and COLUMN."
  (if (stringp text)
      (apply #'propertize
             text
             (majutsu-row-content-properties
              compiled entry-id module column))
    text))

(defun majutsu-row-propertize-decoration
    (text compiled entry-id module decoration)
  "Return TEXT tagged as MODULE DECORATION for ENTRY-ID."
  (if (stringp text)
      (apply #'propertize
             text
             (majutsu-row-decoration-properties
              compiled entry-id module decoration))
    text))

;;; Column text rendering

(defun majutsu-row-render-column-text (entry column &optional plain)
  "Return rendered text for ENTRY COLUMN.
When PLAIN is non-nil, omit faces and text properties."
  (let* ((module (plist-get column :module))
         (face (plist-get column :face))
         (value (majutsu-row-display-string
                 (majutsu-row-column-value entry column))))
    (when (eq module 'tail)
      (setq value (majutsu-row-single-line-string value)))
    (if plain
        (substring-no-properties value)
      (majutsu-row-apply-face-policy value face))))

;;; Module rendering

(defun majutsu-row-render-module-parts
    (entry compiled module &optional annotate plain)
  "Return rendered ENTRY parts for MODULE using COMPILED."
  (let ((entry-id (majutsu-row-entry-id entry compiled))
        parts)
    (dolist (column (majutsu-row-module-columns compiled module))
      (let ((value (majutsu-row-render-column-text entry column plain)))
        (unless (if (eq module 'body)
                    (string-empty-p (string-trim value))
                  (string-empty-p value))
          (push (if annotate
                    (majutsu-row-propertize-content
                     value compiled entry-id module column)
                  value)
                parts))))
    (nreverse parts)))

(defun majutsu-row-render-heading-content
    (entry compiled &optional annotate plain)
  "Render ENTRY heading module content without graph prefixes."
  (let ((parts (majutsu-row-render-module-parts
                entry compiled 'heading annotate plain)))
    (if parts
        (majutsu-row-concat-heading-parts parts)
      "")))

(defun majutsu-row-render-heading-content-lines
    (entry compiled &optional annotate plain)
  "Render ENTRY heading module content lines without graph prefixes."
  (majutsu-row-split-by-separator
   (majutsu-row-render-heading-content entry compiled annotate plain)
   "\n"))

(defun majutsu-row-render-heading-lines (entry compiled)
  "Render ENTRY heading module as visible lines with graph prefixes."
  (let* ((content-lines (majutsu-row-render-heading-content-lines
                         entry compiled))
         (prefixes (or (plist-get entry :heading-prefixes) (list "")))
         (last-prefix (or (car (last prefixes)) ""))
         (count (max (length content-lines) (length prefixes)))
         out)
    (cl-loop for idx below count
             do (let ((prefix (or (nth idx prefixes) last-prefix))
                      (line (or (nth idx content-lines) "")))
                  (push (concat prefix line) out)))
    (nreverse out)))

(defun majutsu-row-render-tail (entry compiled &optional annotate plain)
  "Render ENTRY tail module as a single-line auxiliary string."
  (let ((parts (majutsu-row-render-module-parts
                entry compiled 'tail annotate plain)))
    (when parts
      (majutsu-row-concat-heading-parts parts))))

(defun majutsu-row-render-body (entry compiled &optional annotate plain)
  "Render ENTRY body module as foldable multiline content."
  (let ((parts (majutsu-row-render-module-parts
                entry compiled 'body annotate plain)))
    (when parts
      (string-join parts "\n"))))

;;; Tail alignment

(defun majutsu-row--tail-owner-window ()
  "Return the window that currently owns tail layout."
  (let ((selected (selected-window)))
    (if (eq (window-buffer selected) (current-buffer))
        selected
      (get-buffer-window (current-buffer) 0))))

(defun majutsu-row-tail-align-to-width (width &optional window)
  "Return a display target that right-aligns content of WIDTH."
  (cond
   ((and (display-graphic-p) window)
    (list (max 0 (- (window-body-width window t) width))))
   ((display-graphic-p)
    `(- right (,width)))
   (window
    (max 0 (- (window-body-width window)
              width
              majutsu-row--tail-terminal-padding)))
   (t
    `(- right ,(+ width majutsu-row--tail-terminal-padding)))))

(defun majutsu-row-tail-spacer-display (tail &optional window)
  "Return the spacer display spec used to right-align TAIL."
  (setq window (or window (majutsu-row--tail-owner-window)))
  `(space :align-to
    ,(majutsu-row-tail-align-to-width
      (if (display-graphic-p)
          (string-pixel-width tail)
        (string-width tail))
      window)))

(defun majutsu-row--tail-width-at (pos &optional window)
  "Return displayed width of tail text following spacer at POS."
  (let ((tail (save-excursion
                (goto-char pos)
                (buffer-substring (1+ pos) (line-end-position))))
        (window (or window (majutsu-row--tail-owner-window))))
    (if (and (display-graphic-p) window)
        (save-excursion
          (goto-char (1+ pos))
          (car (window-text-pixel-size window (point) (line-end-position) t)))
      (if (display-graphic-p)
          (save-excursion
            (goto-char pos)
            (string-pixel-width tail))
        (string-width tail)))))

(defun majutsu-row-refresh-tail-spacers (&optional beg end window)
  "Recompute right-alignment display specs in BEG..END."
  (setq window (or window (majutsu-row--tail-owner-window)))
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (save-excursion
        (let ((pos (or beg (point-min)))
              (end (or end (point-max))))
          (while (and (< pos end)
                      (setq pos (text-property-any
                                 pos end 'majutsu-row-tail-spacer t)))
            (put-text-property
             pos (1+ pos) 'display
             `(space :align-to
               ,(majutsu-row-tail-align-to-width
                 (majutsu-row--tail-width-at pos window)
                 window)))
            (setq pos (1+ pos))))))))

;;; Entry insertion

(defun majutsu-row--insert-heading-anchor-line
    (anchor-left tail entry-id prefix compiled)
  "Insert ANCHOR-LEFT and optional TAIL on one prefixed line."
  (let* ((profile (majutsu-row--profile compiled))
         (tail-align (plist-get profile :tail-align))
         (start (point))
         (spacer-pos nil)
         (window (majutsu-row--tail-owner-window)))
    (insert anchor-left)
    (when (and (stringp tail) (not (string-empty-p tail)))
      (setq spacer-pos (point))
      (insert " ")
      (add-text-properties
       spacer-pos (point)
       (majutsu-row-tail-spacer-properties
        compiled entry-id
        (and tail-align
             (majutsu-row-tail-spacer-display tail window))))
      (insert tail))
    (insert "\n")
    (majutsu-row-apply-line-prefix-span start (point) prefix)
    (when (and spacer-pos tail-align)
      (majutsu-row-refresh-tail-spacers spacer-pos (1+ spacer-pos)
                                        window))))

(defun majutsu-row-entry-section-class (entry compiled)
  "Return the Magit section class for ENTRY using COMPILED."
  (let* ((profile (majutsu-row--profile compiled))
         (role-spec (majutsu-row-role-spec entry compiled))
         (fn (plist-get profile :section-class-function)))
    (or (and (plist-member role-spec :section-class)
             (plist-get role-spec :section-class))
        (and fn (funcall fn entry))
        (plist-get profile :section-class)
        'magit-section)))

(defun majutsu-row-entry-section-hide (entry compiled)
  "Return the initial hide value for ENTRY using COMPILED."
  (let* ((profile (majutsu-row--profile compiled))
         (role-spec (majutsu-row-role-spec entry compiled))
         (fn (plist-get profile :section-hide-function)))
    (cond
     ((plist-member role-spec :section-hide)
      (plist-get role-spec :section-hide))
     (fn (funcall fn entry))
     (t (plist-get profile :section-hide)))))

(defun majutsu-row--call-with-child-count-policy (compiled thunk)
  "Call THUNK with COMPILED's child-count display policy."
  (let ((policy (plist-get (majutsu-row--profile compiled)
                           :show-child-count)))
    (cond
     ((or (null policy) (eq policy :inherit))
      (funcall thunk))
     (t
      (let ((magit-section-show-child-count policy))
        (funcall thunk))))))

(defun majutsu-row-insert-entry (entry compiled)
  "Insert parsed ENTRY as a Magit section using COMPILED."
  (let* ((id (majutsu-row-entry-id entry compiled))
         (section-class (majutsu-row-entry-section-class entry compiled))
         (section-value (majutsu-row-section-value entry compiled))
         (section-hide (majutsu-row-entry-section-hide entry compiled))
         (indent (or (plist-get entry :indent) 0))
         (prefixes (or (plist-get entry :heading-prefixes) (list "")))
         (last-prefix (or (car (last prefixes)) ""))
         (content-lines (majutsu-row-render-heading-content-lines
                         entry compiled t))
         (count (max (length content-lines) (length prefixes)))
         (heading-lines nil)
         (tail (majutsu-row-render-tail entry compiled t))
         (suffix-lines (plist-get entry :suffix-lines))
         (children (plist-get entry :children))
         (body (majutsu-row-render-body entry compiled t))
         (has-body (and (stringp body)
                        (not (string-empty-p (string-trim body))))))
    (cl-loop for idx below count
             do (let ((prefix (majutsu-row-propertize-decoration
                               (or (nth idx prefixes) last-prefix)
                               compiled id 'heading
                               (if (= idx 0) 'graph-prefix 'graph-carry)))
                      (line (or (nth idx content-lines) "")))
                  (push (cons prefix line) heading-lines)))
    (setq heading-lines (nreverse heading-lines))
    (majutsu-row--call-with-child-count-policy
     compiled
     (lambda ()
       (magit-insert-section ((eval section-class) section-value section-hide)
         (when heading-lines
           (majutsu-row--insert-heading-anchor-line
            (cdar heading-lines) tail id (caar heading-lines) compiled))
         (dolist (line (cdr heading-lines))
           (majutsu-row-insert-prefixed-line (cdr line) (car line)))
         (dolist (suffix-line suffix-lines)
           (pcase-let* ((`(,prefix . ,content)
                         (majutsu-row-split-prefix-line suffix-line indent))
                        (decorated-prefix
                         (majutsu-row-propertize-decoration
                          prefix compiled id 'heading 'graph-carry))
                        (decorated-content
                         (majutsu-row-propertize-content
                          content compiled id 'heading)))
             (majutsu-row-insert-prefixed-line
              decorated-content decorated-prefix)))
         (when (or has-body children)
           (magit-insert-heading)
           (magit-insert-section-body
             (when has-body
               (let ((body-prefix (majutsu-row-propertize-decoration
                                   (make-string indent ?\s)
                                   compiled id 'body 'body-prefix))
                     (start (point)))
                 (insert body)
                 (insert "\n")
                 (majutsu-row-apply-line-prefix-span
                  start (point) body-prefix)))
             (majutsu-row-insert-forest children compiled))))))))

(defun majutsu-row-insert-forest (entries compiled)
  "Insert row ENTRIES recursively as Magit sections using COMPILED."
  (dolist (entry entries)
    (majutsu-row-insert-entry entry compiled)))

;;; Wash

(defun majutsu-row-wash-entry (compiled)
  "Wash the entry at point using COMPILED."
  (when-let* ((entry (save-excursion
                       (majutsu-row-parse-at-point compiled))))
    (let ((beg (plist-get entry :beg))
          (end (plist-get entry :end)))
      (delete-region beg end)
      (goto-char beg)
      (majutsu-row-insert-entry entry compiled)
      entry)))

(defun majutsu-row-wash-buffer (compiled)
  "Wash all row entries in the current narrowed buffer."
  (let* ((result (majutsu-row-read-buffer compiled))
         (roots (plist-get result :roots))
         (entries (plist-get result :entries))
         (diagnostics (plist-get result :diagnostics))
         (inhibit-read-only t))
    (majutsu-row-report-diagnostics diagnostics)
    (delete-region (point-min) (point-max))
    (majutsu-row-insert-forest roots compiled)
    (setq-local majutsu-row-cached-roots roots)
    entries))

;;; Copy property cleanup

(defun majutsu-row--copy-properties (&optional compiled)
  "Return text properties removed from copied row text."
  (let ((props (copy-sequence majutsu-row--ui-properties)))
    (when compiled
      (dolist (prefix (majutsu-row--compat-prefixes compiled))
        (dolist (suffix '(module field column entry-id decoration tail-spacer))
          (push (majutsu-row--property-symbol prefix suffix) props))))
    props))

(defun majutsu-row-cleanup-copied-string (string &optional compiled)
  "Strip row UI properties from copied STRING."
  (when (stringp string)
    (remove-list-of-text-properties
     0 (length string)
     (majutsu-row--copy-properties compiled)
     string))
  string)

(defun majutsu-row-string-has-module-p (string module)
  "Return non-nil if STRING contains text marked with MODULE."
  (let ((pos 0)
        found)
    (while (and (< pos (length string)) (not found))
      (setq found (eq (get-text-property
                       pos 'majutsu-row-module string)
                      module)
            pos (or (next-single-property-change
                     pos 'majutsu-row-module string)
                    (length string))))
    found))

(defun majutsu-row-string-remove-module (string module)
  "Return STRING with text marked as MODULE removed."
  (let ((pos 0)
        parts)
    (while (< pos (length string))
      (let* ((next (or (next-single-property-change
                        pos 'majutsu-row-module string)
                       (length string)))
             (current (get-text-property
                       pos 'majutsu-row-module string)))
        (unless (eq current module)
          (push (substring string pos next) parts))
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(defun majutsu-row-filter-buffer-substring
    (beg end &optional delete compiled)
  "Filter copied row text between BEG and END.
Drops tail text when both heading and tail are present in the copied region."
  (let ((string (buffer-substring--filter beg end delete))
        (trim-tail nil))
    (when (and (stringp string)
               (majutsu-row-string-has-module-p string 'heading)
               (majutsu-row-string-has-module-p string 'tail))
      (setq string (majutsu-row-string-remove-module string 'tail))
      (setq trim-tail t))
    (setq string (majutsu-row-cleanup-copied-string string compiled))
    (when (and trim-tail (stringp string))
      (setq string (replace-regexp-in-string "[ \t]+$" "" string)))
    string))

;;; Copy index building

(defun majutsu-row-entry-id (entry compiled)
  "Return stable entry id for ENTRY using COMPILED."
  (let* ((profile (majutsu-row--profile compiled))
         (role-spec (majutsu-row-role-spec entry compiled)))
    (cond
     ((plist-member role-spec :entry-id)
      (majutsu-row-resolve-role-ref
       (plist-get role-spec :entry-id) entry compiled))
     ((plist-member role-spec :section-value)
      (majutsu-row-resolve-role-ref
       (plist-get role-spec :section-value) entry compiled))
     ((plist-get profile :entry-id-function)
      (funcall (plist-get profile :entry-id-function) entry))
     (t (majutsu-row-column entry 'id)))))

(defun majutsu-row-section-value (entry compiled)
  "Return Magit section value for ENTRY using COMPILED."
  (let* ((profile (majutsu-row--profile compiled))
         (role-spec (majutsu-row-role-spec entry compiled)))
    (cond
     ((plist-member role-spec :section-value)
      (majutsu-row-resolve-role-ref
       (plist-get role-spec :section-value) entry compiled))
     ((plist-get profile :section-value-function)
      (funcall (plist-get profile :section-value-function) entry))
     (t (majutsu-row-entry-id entry compiled)))))

(defun majutsu-row-entry-section-type (entry compiled)
  "Return the Magit section type symbol for ENTRY using COMPILED."
  (let ((section-class (majutsu-row-entry-section-class entry compiled)))
    (if (class-p section-class)
        (or (car (rassq section-class magit--section-type-alist))
            section-class)
      section-class)))

(defun majutsu-row-section-ident (entry compiled)
  "Return Magit-style section identity for ENTRY using COMPILED."
  (cons (cons (majutsu-row-entry-section-type entry compiled)
              (majutsu-row-section-value entry compiled))
        (when-let* ((parent (plist-get entry :parent)))
          (majutsu-row-section-ident parent compiled))))

(defun majutsu-row-build-indexes (compiled entries)
  "Return (ENTRY-INDEX . SECTION-IDENT-INDEX) for COMPILED ENTRIES."
  (let ((entry-index (make-hash-table :test #'equal))
        (section-ident-index (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (when-let* ((id (majutsu-row-entry-id entry compiled)))
        (puthash id entry entry-index))
      (puthash (majutsu-row-section-ident entry compiled)
               entry section-ident-index))
    (cons entry-index section-ident-index)))

;;; Copy buffer data management

(defun majutsu-row-set-buffer-data (compiled entries &optional roots)
  "Set current buffer row COMPILED, ENTRIES, and optional ROOTS."
  (let ((indexes (majutsu-row-build-indexes compiled entries)))
    (setq-local majutsu-row-buffer-compiled compiled)
    (setq-local majutsu-row-cached-entries entries)
    (setq-local majutsu-row-cached-roots (or roots entries))
    (setq-local majutsu-row-entry-index (car indexes))
    (setq-local majutsu-row-section-ident-index (cdr indexes))))

(defun majutsu-row-set-buffer-forest-data (compiled roots entries)
  "Set current buffer row COMPILED, ROOTS, and flat ENTRIES."
  (majutsu-row-set-buffer-data compiled entries roots))

(defun majutsu-row-clear-buffer-data ()
  "Clear current buffer row data."
  (setq-local majutsu-row-buffer-compiled nil)
  (setq-local majutsu-row-cached-entries nil)
  (setq-local majutsu-row-cached-roots nil)
  (setq-local majutsu-row-entry-index nil)
  (setq-local majutsu-row-section-ident-index nil))

;;; Copy text-property lookup

(defun majutsu-row-text-property-near-point (property &optional pos)
  "Return PROPERTY near POS, preferring the previous character."
  (let ((pos (or pos (point))))
    (or (and (> pos (point-min))
             (get-text-property (1- pos) property))
        (get-text-property pos property))))

;;; Copy entry lookup

(defun majutsu-row-entry-for-id (id compiled &optional entries)
  "Return entry ID using COMPILED and optional ENTRIES."
  (when (and id (not (and (stringp id) (string-empty-p id))))
    (or (and (not entries)
             (hash-table-p majutsu-row-entry-index)
             (gethash id majutsu-row-entry-index))
        (seq-find (lambda (entry)
                    (equal id (majutsu-row-entry-id entry compiled)))
                  (or entries majutsu-row-cached-entries)))))

(defun majutsu-row-entry-for-section-ident (ident compiled &optional entries)
  "Return entry with Magit section IDENT using COMPILED and optional ENTRIES."
  (when ident
    (or (and (not entries)
             (hash-table-p majutsu-row-section-ident-index)
             (gethash ident majutsu-row-section-ident-index))
        (seq-find (lambda (entry)
                    (equal ident (majutsu-row-section-ident entry compiled)))
                  (or entries majutsu-row-cached-entries)))))

(defun majutsu-row--entry-at-current-section (compiled entries)
  "Return row entry matching current Magit section using COMPILED."
  (when-let* ((section (magit-current-section)))
    (majutsu-row-entry-for-section-ident
     (magit-section-ident section) compiled entries)))

(defun majutsu-row-entry-at-point (&optional compiled entries)
  "Return cached row entry at point, or nil."
  (let ((compiled (majutsu-row-current-compiled compiled)))
    (or (when-let* ((entry-id (majutsu-row-text-property-near-point
                               'majutsu-row-entry-id)))
          (majutsu-row-entry-for-id entry-id compiled entries))
        (majutsu-row--entry-at-current-section compiled entries))))

;;; Copy field value access

(defun majutsu-row-field-copy-string (value)
  "Return canonical clipboard text for field VALUE."
  (cond
   ((null value) "")
   ((stringp value) (substring-no-properties value))
   ((listp value) (mapconcat #'majutsu-row-field-copy-string value "\n"))
   (t (format "%s" value))))

(defun majutsu-row-field-value-present-p (value)
  "Return non-nil if canonical field VALUE should be offered for copying."
  (cond
   ((null value) nil)
   ((stringp value) (not (string-empty-p value)))
   ((listp value) (not (null value)))
   (t t)))

(defun majutsu-row-entry-field-candidate-preview (entry field)
  "Return one-line preview string for FIELD on ENTRY."
  (let* ((text (majutsu-row-field-copy-string
                (majutsu-row-column entry field)))
         (text (replace-regexp-in-string "[\n\r\t ]+" " " text nil t)))
    (if (> (length text) 48)
        (concat (substring text 0 48) "…")
      text)))

(defun majutsu-row-entry-copyable-fields (entry compiled)
  "Return copyable canonical field symbols for ENTRY using COMPILED order."
  (let ((fields nil)
        (seen nil))
    (dolist (column (plist-get compiled :columns))
      (let* ((field (plist-get column :field))
             (value (majutsu-row-column entry field)))
        (when (and (not (memq field seen))
                   (majutsu-row-field-value-present-p value))
          (push field seen)
          (push field fields))))
    (dolist (cell (plist-get entry :columns))
      (let ((field (car cell))
            (value (cdr cell)))
        (when (and (not (memq field seen))
                   (majutsu-row-field-value-present-p value))
          (push field seen)
          (push field fields))))
    (nreverse fields)))

(defun majutsu-row-read-entry-field (entry compiled &optional prompt)
  "Read one canonical field from ENTRY using COMPILED."
  (let* ((default-field (majutsu-row-text-property-near-point
                         'majutsu-row-field))
         (candidates (mapcar (lambda (field)
                               (cons (format "%s\t%s"
                                             field
                                             (majutsu-row-entry-field-candidate-preview
                                              entry field))
                                     field))
                             (majutsu-row-entry-copyable-fields
                              entry compiled)))
         (default (car (rassoc default-field candidates)))
         (choice (completing-read (or prompt "Copy entry field: ")
                                  (mapcar #'car candidates)
                                  nil t nil nil default)))
    (or (cdr (assoc choice candidates))
        (user-error "Unknown entry field %S" choice))))

;;; Copy string helper

(defun majutsu-row-copy-string (string)
  "Copy STRING to the kill ring and echo it."
  (kill-new string)
  (message "%s" string))

(defun majutsu-row-entry-field-value-to-kill (entry field)
  "Copy canonical FIELD value from ENTRY to the kill ring."
  (let* ((value (majutsu-row-column entry field)))
    (unless value
      (user-error "Field %s is unavailable for the current entry" field))
    (unless (majutsu-row-field-value-present-p value)
      (user-error "Field %s is empty for the current entry" field))
    (majutsu-row-copy-string
     (majutsu-row-field-copy-string value))))

;;; Interactive copy commands

;;;###autoload
(defun majutsu-row-copy-field (&optional compiled entries)
  "Copy the rendered value of the structured field at point.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-row-current-compiled compiled))
           (entry (or (majutsu-row-entry-at-point compiled entries)
                      (user-error "No entry at point")))
           (instance (majutsu-row-text-property-near-point
                      'majutsu-row-column))
           (field (majutsu-row-text-property-near-point
                   'majutsu-row-field))
           (module (majutsu-row-text-property-near-point
                    'majutsu-row-module))
           (column (or (and instance
                            (majutsu-row-column-by-instance
                             compiled instance))
                       (and field module
                            (seq-find (lambda (candidate)
                                        (and (eq (plist-get candidate :field) field)
                                             (eq (plist-get candidate :module) module)))
                                      (plist-get compiled :columns))))))
      (unless column
        (user-error "No entry field at point"))
      (majutsu-row-copy-string
       (majutsu-row-render-column-text entry column t)))))

;;;###autoload
(defun majutsu-row-copy-module (&optional compiled entries)
  "Copy the rendered structured module at point.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-row-current-compiled compiled))
           (entry (or (majutsu-row-entry-at-point compiled entries)
                      (user-error "No entry at point")))
           (module (majutsu-row-text-property-near-point
                    'majutsu-row-module))
           (text (cl-case module
                   (heading (majutsu-row-render-heading-content
                             entry compiled nil t))
                   (tail (or (majutsu-row-render-tail entry compiled nil t)
                             ""))
                   (body (or (majutsu-row-render-body entry compiled nil t)
                             ""))
                   (t nil))))
      (unless text
        (user-error "No entry module at point"))
      (majutsu-row-copy-string text))))

;;;###autoload
(defun majutsu-row-copy-entry-field (&optional compiled entries)
  "Copy a canonical field from the current structured entry.

Unlike `majutsu-row-copy-field', this can target fields that are parsed and
stored on the entry but not currently visible.  When called interactively,
prompts with completion over the current entry's available canonical fields.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-row-current-compiled compiled))
           (entry (or (majutsu-row-entry-at-point compiled entries)
                      (user-error "No entry at point")))
           (field (majutsu-row-read-entry-field entry compiled)))
      (majutsu-row-entry-field-value-to-kill entry field))))

;;;###autoload
(defun majutsu-row-copy-commit-id (&optional compiled entries)
  "Copy the current structured entry's commit hash.

When the region is active, copy it literally using `copy-region-as-kill'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (let* ((compiled (majutsu-row-current-compiled compiled))
           (entry (or (majutsu-row-entry-at-point compiled entries)
                      (user-error "No entry at point"))))
      (majutsu-row-entry-field-value-to-kill entry 'commit-id))))

;;; Copy transient macro

(defmacro majutsu-row-define-copy-transient (name doc &rest suffixes)
  "Define NAME as a copy transient with DOC and extra SUFFIXES."
  `(transient-define-prefix ,name ()
     ,doc
     [[("s" "Section value" majutsu-copy-section-value)
       ("f" "Visible field at point" majutsu-row-copy-field)
       ("F" "Entry field…" majutsu-row-copy-entry-field)
       ("m" "Visible module at point" majutsu-row-copy-module)
       ,@suffixes]]))

;;;###autoload(autoload 'majutsu-row-copy-transient "majutsu-row" nil t)
(majutsu-row-define-copy-transient
 majutsu-row-copy-transient
 "Transient for semantic copy commands in structured row buffers."
 ("h" "Commit hash" majutsu-row-copy-commit-id))

(provide 'majutsu-row)
;;; majutsu-row.el ends here
