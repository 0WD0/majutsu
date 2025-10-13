;;; majutsu-template.el -*- lexical-binding: t; -*-

;; A small Elisp EDSL to build jj template language strings.
;; v0 focused on safe string construction and a few core combinators.
;; v3 grows type metadata and richer sugar so we can mirror jj's semantics.

(require 'cl-lib)
(require 'subr-x)

(defgroup majutsu-template nil
  "Elisp wrapper for composing jj templates."
  :group 'majutsu)

(defcustom majutsu-template-compact-output t
  "If non-nil, compile without extra whitespace beyond commas and spaces."
  :type 'boolean
  :group 'majutsu-template)

(defvar majutsu-template--allow-eval nil
  "When non-nil, cons forms encountered during sugar transformation may be eval'd.")

;;;; Type and callable metadata ################################################

(cl-defstruct (majutsu-template--type
               (:constructor majutsu-template--make-type))
  "Metadata describing a template value type."
  name
  doc
  converts-to)

(defvar majutsu-template--type-registry (make-hash-table :test #'eq)
  "Registry of known template types keyed by symbol.")

(defun majutsu-template--normalize-converts (value)
  "Normalize VALUE describing type conversions into a canonical list.
Accepts nil, a list of symbols, or a list of (TYPE . STATUS) pairs."
  (cond
   ((null value) nil)
   ((and (listp value) (consp (car value)) (symbolp (caar value)))
    value)
   ((listp value)
    (mapcar (lambda (type) (cons type 'yes)) value))
   ((symbolp value)
    (list (cons value 'yes)))
   (t
    (user-error "majutsu-template: invalid :converts specification %S" value))))

(defun majutsu-template-define-type (name &rest plist)
  "Register template type NAME with optional metadata PLIST.
Recognised keys: :doc (string), :converts or :converts-to (list)."
  (cl-check-type name symbol)
  (let* ((doc (plist-get plist :doc))
         (raw-converts (or (plist-get plist :converts)
                           (plist-get plist :converts-to)))
         (converts-to (majutsu-template--normalize-converts raw-converts)))
    (puthash name
             (majutsu-template--make-type
              :name name
              :doc doc
              :converts-to converts-to)
             majutsu-template--type-registry)))

(defun majutsu-template--lookup-type (name)
  "Return registered type metadata for NAME or nil."
  (gethash name majutsu-template--type-registry))

(cl-defstruct (majutsu-template--arg
               (:constructor majutsu-template--make-arg))
  name type optional rest converts doc)

(cl-defstruct (majutsu-template--fn
               (:constructor majutsu-template--make-fn))
  name
  symbol
  args
  returns
  return-converts
  doc
  scope
  owner)

(defvar majutsu-template--function-registry (make-hash-table :test #'equal)
  "Map template function names to `majutsu-template--fn' metadata.")

(defvar majutsu-template--function-name-map (make-hash-table :test #'eq)
  "Lookup table from symbols/keywords to template function names (strings).")

(defvar majutsu-template--method-registry (make-hash-table :test #'equal)
  "Map (TYPE . NAME) to template method metadata (keywords included).")

(defun majutsu-template--symbol->template-name (sym)
  "Return the template name string corresponding to SYM.
Accepts symbols and keywords."
  (cond
   ((keywordp sym) (substring (symbol-name sym) 1))
   ((symbolp sym) (symbol-name sym))
   (t (user-error "majutsu-template: invalid function name %S" sym))))

(defun majutsu-template--normalize-call-name (name)
  "Return template function name string derived from NAME.
NAME may be a symbol, keyword, string, or a quoted symbol form."
  (cond
   ((and (consp name) (eq (car name) 'quote))
    (majutsu-template--symbol->template-name (cadr name)))
   ((symbolp name) (majutsu-template--symbol->template-name name))
   ((stringp name) name)
   (t (user-error "majutsu-template: unsupported call name %S" name))))

(defun majutsu-template--normalize-type-symbol (type)
  "Normalize TYPE expression to a symbol, validating format.
TYPE may be a symbol, keyword, or string."
  (cond
   ((symbolp type)
    (if (keywordp type)
        (intern (substring (symbol-name type) 1))
      type))
   ((stringp type)
    (intern type))
   (t
    (user-error "majutsu-template: invalid type specifier %S" type))))

(defun majutsu-template--lookup-function-meta (key)
  "Return metadata struct for function identified by KEY, or nil."
  (let ((name (cond
               ((stringp key) key)
               (t (gethash key majutsu-template--function-name-map)))))
    (when name
      (gethash name majutsu-template--function-registry))))

(defun majutsu-template--lookup-function-name (key)
  "Return registered template name string for KEY, or nil if unknown."
  (let ((meta (majutsu-template--lookup-function-meta key)))
    (when meta
      (majutsu-template--fn-name meta))))

(defun majutsu-template--lookup-method (owner name)
  "Return metadata for method NAME on OWNER type."
  (gethash (cons owner name) majutsu-template--method-registry))

(defun majutsu-template--lookup-keyword (owner name)
  "Return metadata for keyword NAME on OWNER type (alias of method lookup)."
  (majutsu-template--lookup-method owner name))

(defun majutsu-template--reserved-name-p (name)
  "Return non-nil if NAME (string) conflicts with built-in helpers."
  (when (boundp 'majutsu-template--sugar-ops)
    (let ((sym (intern name))
          (keyword (intern (concat ":" name))))
      (or (alist-get sym majutsu-template--sugar-ops)
          (alist-get keyword majutsu-template--sugar-ops)))))

(defun majutsu-template--register-function (meta)
  "Register custom function described by META (a `majutsu-template--fn')."
  (let* ((name (majutsu-template--fn-name meta))
         (fn-symbol (majutsu-template--fn-symbol meta))
         (scope (or (majutsu-template--fn-scope meta) :function))
         (owner (majutsu-template--fn-owner meta))
         (sym (intern name))
         (keyword (intern (concat ":" name)))
         (pre-existing (gethash name majutsu-template--function-registry)))
    (pcase scope
      (:function
       (when (majutsu-template--reserved-name-p name)
         (user-error "majutsu-template: %s is reserved" name))
       (when pre-existing
         (message "majutsu-template: redefining template helper %s" name))
       (puthash name meta majutsu-template--function-registry)
       (puthash sym name majutsu-template--function-name-map)
       (puthash keyword name majutsu-template--function-name-map))
      (:method
       (unless owner
         (user-error "majutsu-template: method %s must declare :owner type" name))
       (puthash (cons owner name) meta majutsu-template--method-registry))
      (:keyword
       (unless owner
         (user-error "majutsu-template: keyword %s must declare :owner type" name))
       (puthash (cons owner name) meta majutsu-template--method-registry))
      (_
       (user-error "majutsu-template: unsupported callable scope %S" scope)))
    fn-symbol))

(eval-and-compile
  (defun majutsu-template--arg->metadata (arg)
    "Convert ARG struct to a plist suitable for metadata export."
    (list :name (majutsu-template--arg-name arg)
          :type (majutsu-template--arg-type arg)
          :optional (majutsu-template--arg-optional arg)
          :rest (majutsu-template--arg-rest arg)
          :converts (majutsu-template--arg-converts arg)
          :doc (majutsu-template--arg-doc arg)))

  (defun majutsu-template--parse-arg-options (name opts)
    "Internal helper to parse OPTS plist for argument NAME."
    (let ((optional nil)
          (rest nil)
          (converts nil)
          (doc nil))
      (while opts
        (let ((key (pop opts))
              (val (pop opts)))
          (pcase key
            (:optional (setq optional (not (null val))))
            (:rest (setq rest (not (null val))))
            (:converts (setq converts val))
            (:doc (setq doc val))
            (_ (user-error "majutsu-template-defun %s: unknown option %S" name key)))))
      (list optional rest converts doc)))

  (defun majutsu-template--parse-args (fn-name arg-specs)
    "Parse ARG-SPECS for FN-NAME into `majutsu-template--arg' structs.
Also validates placement of optional/rest arguments."
    (let ((parsed '())
          (rest-seen nil))
      (dolist (spec arg-specs)
        (unless (and (consp spec) (symbolp (car spec)) (>= (length spec) 2))
          (user-error "majutsu-template-defun %s: invalid parameter spec %S" fn-name spec))
        (let* ((arg-name (car spec))
               (type (cadr spec))
               (opts (cddr spec))
               (_ (unless (symbolp type)
                    (user-error "majutsu-template-defun %s: argument %s has invalid type %S"
                                fn-name arg-name type)))
               (opt-data (majutsu-template--parse-arg-options arg-name opts))
               (optional (nth 0 opt-data))
               (rest (nth 1 opt-data))
               (converts (nth 2 opt-data))
               (doc (nth 3 opt-data)))
          (when (and rest (not (null (cdr (memq spec arg-specs)))))
            (user-error "majutsu-template-defun %s: :rest parameter must be last" fn-name))
          (when (and rest rest-seen)
            (user-error "majutsu-template-defun %s: only one :rest parameter allowed" fn-name))
          (when (and rest optional)
            (user-error "majutsu-template-defun %s: parameter %s cannot be both optional and :rest"
                        fn-name arg-name))
          (when (and optional rest-seen)
            (user-error "majutsu-template-defun %s: optional parameters must precede :rest" fn-name))
          (when rest (setq rest-seen t))
          (push (majutsu-template--make-arg
                 :name arg-name
                 :type type
                 :optional optional
                 :rest rest
                 :converts converts
                 :doc doc)
                parsed)))
      (nreverse parsed)))

  (defun majutsu-template--build-lambda-list (args)
    "Return lambda list corresponding to ARGS metadata."
    (let ((required '())
          (optional '())
          (rest nil))
      (dolist (arg args)
        (when (majutsu-template--arg-rest arg)
          (when rest
            (user-error "majutsu-template: multiple :rest parameters not allowed"))
          (setq rest (majutsu-template--arg-name arg)))
        (cond
         ((majutsu-template--arg-rest arg))
         ((majutsu-template--arg-optional arg)
          (push (majutsu-template--arg-name arg) optional))
         (t
          (push (majutsu-template--arg-name arg) required))))
      (setq required (nreverse required)
            optional (nreverse optional))
      (append required
              (when optional (cons '&optional optional))
              (when rest (list '&rest rest)))))

  (defun majutsu-template--build-arg-normalizers (args)
    "Return forms that normalize function parameters described by ARGS."
    (cl-loop for arg in args
             collect
             (let ((name (majutsu-template--arg-name arg)))
               (cond
                ((majutsu-template--arg-rest arg)
                 `(setq ,name (mapcar #'majutsu-template--normalize ,name)))
                ((majutsu-template--arg-optional arg)
                 `(when ,name (setq ,name (majutsu-template--normalize ,name))))
                (t
                 `(setq ,name (majutsu-template--normalize ,name)))))))

  (defun majutsu-template--parse-signature (fn-name signature)
    "Parse SIGNATURE plist for FN-NAME, returning plist with metadata."
    (unless (and (consp signature) (eq (car signature) :returns) (cadr signature))
      (user-error "majutsu-template-defun %s: signature must start with (:returns TYPE ...)" fn-name))
    (let ((returns (cadr signature))
          (rest (cddr signature))
          (doc nil)
          (converts nil)
          (scope nil)
          (owner nil)
          (template-name nil))
      (unless (symbolp returns)
        (user-error "majutsu-template-defun %s: invalid return type %S" fn-name returns))
      (while rest
        (let ((key (pop rest))
              (value (pop rest)))
          (pcase key
            (:converts (setq converts value))
            (:doc (setq doc value))
            (:scope (setq scope value))
            (:owner (setq owner value))
            (:template-name (setq template-name value))
            (_ (user-error "majutsu-template-defun %s: unknown signature key %S" fn-name key)))))
      (when (and scope (not (memq scope '(:function :method :keyword))))
        (user-error "majutsu-template-defun %s: unsupported scope %S" fn-name scope))
      (when (and owner (not (symbolp owner)))
        (user-error "majutsu-template-defun %s: :owner expects a symbol, got %S" fn-name owner))
      (when (and template-name (not (stringp template-name)))
        (user-error "majutsu-template-defun %s: :template-name expects string, got %S"
                    fn-name template-name))
      (list :returns returns
            :converts converts
            :doc doc
            :scope scope
            :owner owner
            :template-name template-name)))

  (defun majutsu-template--compose-docstring (name base-doc args)
    "Compose docstring for helper NAME using BASE-DOC and ARGS metadata."
    (let ((header (or base-doc (format "Template helper %s." name)))
          (param-lines
           (when args
             (mapconcat
              (lambda (arg)
                (let ((arg-name (majutsu-template--arg-name arg))
                      (type (majutsu-template--arg-type arg))
                      (optional (majutsu-template--arg-optional arg))
                      (rest (majutsu-template--arg-rest arg))
                      (doc (majutsu-template--arg-doc arg)))
                  (concat "  " (symbol-name arg-name)
                          " (" (symbol-name type) ")"
                          (cond
                           (rest " [rest]")
                           (optional " [optional]")
                           (t ""))
                          (if doc
                              (format ": %s" doc)
                            ""))))
              args
              "\n"))))
      (if param-lines
          (concat header "\n\nParameters:\n" param-lines)
        header)))

  (defun majutsu-template--ensure-owner-type (scope owner fn-name)
    "Ensure OWNER is provided (and roughly valid) for scoped callable FN-NAME."
    (when (memq scope '(:method :keyword))
      (unless owner
        (user-error "majutsu-template-defun %s: :owner is required for %s"
                    fn-name scope))
      (unless (majutsu-template--lookup-type owner)
        (message "majutsu-template: warning – declaring %s for unknown type %S"
                 fn-name owner)))
    owner)

  (defun majutsu-template-def--inherit-signature (signature scope owner)
    (append signature (list :scope scope :owner owner))))

;;;###autoload
(defmacro majutsu-template-defun (name args signature &rest body)
  "Define a majutsu template helper NAME with ARGS, SIGNATURE and BODY.
Generates `majutsu-template-NAME' and registers it for template DSL usage."
  (declare (indent defun))
  (unless (and (symbolp name) (not (keywordp name)))
    (user-error "majutsu-template-defun: NAME must be an unprefixed symbol"))
  (when (null body)
    (user-error "majutsu-template-defun %s: body may not be empty" name))
  (let* ((name-str (symbol-name name))
         (signature-info (majutsu-template--parse-signature name signature))
         (scope (or (plist-get signature-info :scope) :function))
         (owner (majutsu-template--ensure-owner-type scope
                                                     (plist-get signature-info :owner)
                                                     name))
         (template-name (or (plist-get signature-info :template-name) name-str))
         (fn-symbol (intern (format "majutsu-template-%s" name-str)))
         (parsed-args (majutsu-template--parse-args name args))
         (lambda-list (majutsu-template--build-lambda-list parsed-args))
         (normalizers (majutsu-template--build-arg-normalizers parsed-args))
         (return-type (plist-get signature-info :returns))
         (return-converts (plist-get signature-info :converts))
         (docstring (majutsu-template--compose-docstring
                     template-name (plist-get signature-info :doc) parsed-args))
         (arg-metadata (mapcar #'majutsu-template--arg->metadata parsed-args))
         (meta `(majutsu-template--make-fn
                 :name ,template-name
                 :symbol ',fn-symbol
                 :args ',arg-metadata
                 :returns ',return-type
                 :return-converts ',return-converts
                 :doc ,docstring
                 :scope ',scope
                 :owner ',owner)))
    `(progn
       (majutsu-template--register-function ,meta)
       (defun ,fn-symbol ,lambda-list
         ,docstring
         ,@normalizers
         (let ((majutsu-template--result (progn ,@body)))
           (majutsu-template--normalize majutsu-template--result))))))

(defmacro majutsu-template-defmethod (name owner args signature &rest body)
  "Define template method NAME applicable to OWNER type.
ARGS describe parameters after the implicit SELF argument."
  (declare (indent defun))
  (let ((extended-args (cons `(self ,owner) args)))
    `(majutsu-template-defun ,name ,extended-args
       ,(majutsu-template-def--inherit-signature signature :method owner)
       ,@body)))

(defmacro majutsu-template-defkeyword (name owner signature &rest body)
  "Define keyword NAME (a 0-argument method) on OWNER type."
  (declare (indent defun))
  `(majutsu-template-defun ,name ((self ,owner))
     ,(majutsu-template-def--inherit-signature signature :keyword owner)
     ,@body))

(defun majutsu-template--method-stub (&rest _args)
  "Placeholder for template methods/keywords that are not executable in Elisp."
  (error "majutsu-template: method stubs are not callable at runtime"))

(defun majutsu-template--parse-type-name (name)
  "Return canonical symbol corresponding to type NAME (string or symbol)."
  (cond
   ((symbolp name) name)
   ((stringp name)
    (let ((base (if (string-match "\\`\\([^<]+\\)" name)
                    (match-string 1 name)
                  name)))
      (intern base)))
   (t
    (error "majutsu-template: invalid type name %S" name))))

(defun majutsu-template--register-method-spec (owner method-spec)
  "Register method METADATA described by METHOD-SPEC for OWNER type."
  (let* ((method-name (car method-spec))
         (plist (cdr method-spec))
         (scope (or (plist-get plist :scope)
                    (if (plist-get plist :keyword) :keyword :method)))
         (template-name (or (plist-get plist :template-name)
                            (symbol-name method-name)))
         (raw-args (plist-get plist :args))
         (returns (majutsu-template--parse-type-name
                   (or (plist-get plist :returns) 'Template)))
         (return-converts (plist-get plist :return-converts))
         (doc (plist-get plist :doc))
         (args-specs (cons `(self ,owner) (or raw-args '())))
         (parsed-args (majutsu-template--parse-args method-name args-specs))
         (meta (majutsu-template--make-fn
                :name template-name
                :symbol 'majutsu-template--method-stub
                :args parsed-args
                :returns returns
                :return-converts return-converts
                :doc doc
                :scope scope
                :owner owner)))
    (majutsu-template--register-function meta)))

(defun majutsu-template--register-methods (specs)
  "Register built-in method metadata from SPECS list."
  (dolist (entry specs)
    (let ((owner (car entry))
          (methods (cdr entry)))
      (dolist (method methods)
        (majutsu-template--register-method-spec owner method)))))

(defconst majutsu-template--builtin-type-specs
  '((AnnotationLine
     :doc "Annotation/annotate line context."
     :converts ((Boolean . no) (Serialize . no) (Template . no)))
    (Boolean
     :doc "Boolean value."
     :converts ((Boolean . yes) (Serialize . yes) (Template . yes)))
    (ChangeId
     :doc "Change identifier."
     :converts ((Boolean . no) (Serialize . yes) (Template . yes)))
    (Commit
     :doc "Commit object."
     :converts ((Boolean . no) (Serialize . yes) (Template . no)))
    (CommitEvolutionEntry
     :doc "Commit evolution information."
     :converts ((Boolean . no) (Serialize . yes) (Template . no)))
    (CommitId
     :doc "Commit identifier."
     :converts ((Boolean . no) (Serialize . yes) (Template . yes)))
    (CommitRef
     :doc "Commit reference (bookmark/tag)."
     :converts ((Boolean . no) (Serialize . yes) (Template . yes)))
    (ConfigValue
     :doc "Configuration value."
     :converts ((Boolean . no) (Serialize . yes) (Template . yes)))
    (CryptographicSignature
     :doc "Cryptographic signature metadata."
     :converts ((Boolean . no) (Serialize . no) (Template . no)))
    (DiffStats
     :doc "Diff statistics histogram."
     :converts ((Boolean . no) (Serialize . no) (Template . yes)))
    (Email
     :doc "Email address component."
     :converts ((Boolean . yes) (Serialize . yes) (Template . yes)))
    (Integer
     :doc "Integer value."
     :converts ((Boolean . no) (Serialize . yes) (Template . yes)))
    (Lambda
     :doc "Template lambda/expression."
     :converts ((Boolean . no) (Serialize . no) (Template . no)))
    (List
     :doc "Generic list."
     :converts ((Boolean . yes) (Serialize . maybe) (Template . maybe)))
    (List-Trailer
     :doc "List of trailers."
     :converts ((Boolean . yes) (Serialize . maybe) (Template . maybe)))
    (ListTemplate
     :doc "List of template fragments."
     :converts ((Boolean . no) (Serialize . no) (Template . yes)))
    (Operation
     :doc "Operation object."
     :converts ((Boolean . no) (Serialize . yes) (Template . no)))
    (OperationId
     :doc "Operation identifier."
     :converts ((Boolean . no) (Serialize . yes) (Template . yes)))
    (Option
     :doc "Optional value."
     :converts ((Boolean . yes) (Serialize . maybe) (Template . maybe)))
    (RefSymbol
     :doc "Reference symbol."
     :converts ((Boolean . no) (Serialize . yes) (Template . yes)))
    (RepoPath
     :doc "Repository path."
     :converts ((Boolean . no) (Serialize . yes) (Template . yes)))
    (Serialize
     :doc "Serializable expression."
     :converts ((Serialize . yes)))
    (ShortestIdPrefix
     :doc "Shortest id prefix."
     :converts ((Boolean . no) (Serialize . yes) (Template . yes)))
    (Signature
     :doc "Commit signature."
     :converts ((Boolean . no) (Serialize . yes) (Template . yes)))
    (SizeHint
     :doc "Size hint bounds."
     :converts ((Boolean . no) (Serialize . yes) (Template . no)))
    (String
     :doc "String value."
     :converts ((Boolean . yes) (Serialize . yes) (Template . yes)))
    (Stringify
     :doc "Stringified template value."
     :converts ((Boolean . no) (Serialize . maybe) (Template . yes)))
    (StringPattern
     :doc "String pattern literal."
     :converts ((Boolean . no) (Serialize . no) (Template . no)))
    (Template
     :doc "Template fragment."
     :converts ((Boolean . no) (Serialize . no) (Template . yes)))
    (Timestamp
     :doc "Timestamp value."
     :converts ((Boolean . no) (Serialize . yes) (Template . yes)))
    (TimestampRange
     :doc "Timestamp range."
     :converts ((Boolean . no) (Serialize . yes) (Template . yes)))
    (Trailer
     :doc "Commit trailer."
     :converts ((Boolean . no) (Serialize . no) (Template . yes)))
    (TreeDiff
     :doc "Tree diff."
     :converts ((Boolean . no) (Serialize . no) (Template . no)))
    (TreeDiffEntry
     :doc "Tree diff entry."
     :converts ((Boolean . no) (Serialize . no) (Template . no)))
    (TreeEntry
     :doc "Tree entry."
     :converts ((Boolean . no) (Serialize . no) (Template . no)))
    (WorkspaceRef
     :doc "Workspace reference."
     :converts ((Boolean . no) (Serialize . yes) (Template . yes))))
  "Built-in template types and their conversion metadata.")

(dolist (type-spec majutsu-template--builtin-type-specs)
  (apply #'majutsu-template-define-type type-spec))

(defconst majutsu-template--builtin-method-specs
  '((AnnotationLine
     (commit :returns Commit :keyword t)
     (content :returns Template :keyword t)
     (line_number :returns Integer :keyword t)
     (original_line_number :returns Integer :keyword t)
     (first_line_in_hunk :returns Boolean :keyword t))
    (Commit
     (description :returns String :keyword t)
     (trailers :returns List-Trailer :keyword t)
     (change_id :returns ChangeId :keyword t)
     (commit_id :returns CommitId :keyword t)
     (parents :returns List :keyword t)
     (author :returns Signature :keyword t)
     (committer :returns Signature :keyword t)
     (signature :returns Option :keyword t)
     (mine :returns Boolean :keyword t)
     (working_copies :returns List :keyword t)
     (current_working_copy :returns Boolean :keyword t)
     (bookmarks :returns List :keyword t)
     (local_bookmarks :returns List :keyword t)
     (remote_bookmarks :returns List :keyword t)
     (tags :returns List :keyword t)
     (git_refs :returns List :keyword t)
     (git_head :returns Boolean :keyword t)
     (divergent :returns Boolean :keyword t)
     (hidden :returns Boolean :keyword t)
     (immutable :returns Boolean :keyword t)
     (contained_in :args ((revset String)) :returns Boolean)
     (conflict :returns Boolean :keyword t)
     (empty :returns Boolean :keyword t)
     (diff :args ((files String :optional t)) :returns TreeDiff)
     (files :args ((files String :optional t)) :returns List)
     (root :returns Boolean :keyword t))
    (CommitEvolutionEntry
     (commit :returns Commit :keyword t)
     (operation :returns Operation :keyword t))
    (ChangeId
     (normal_hex :returns String :keyword t)
     (short :args ((len Integer :optional t)) :returns String)
     (shortest :args ((min_len Integer :optional t)) :returns ShortestIdPrefix))
    (CommitId
     (short :args ((len Integer :optional t)) :returns String)
     (shortest :args ((min_len Integer :optional t)) :returns ShortestIdPrefix))
    (CommitRef
     (name :returns RefSymbol :keyword t)
     (remote :returns Option :keyword t)
     (present :returns Boolean :keyword t)
     (conflict :returns Boolean :keyword t)
     (normal_target :returns Option :keyword t)
     (removed_targets :returns List :keyword t)
     (added_targets :returns List :keyword t)
     (tracked :returns Boolean :keyword t)
     (tracking_present :returns Boolean :keyword t)
     (tracking_ahead_count :returns SizeHint :keyword t)
     (tracking_behind_count :returns SizeHint :keyword t))
    (ConfigValue
     (as_boolean :returns Boolean :keyword t)
     (as_integer :returns Integer :keyword t)
     (as_string :returns String :keyword t)
     (as_string_list :returns List :keyword t))
    (CryptographicSignature
     (status :returns String :keyword t)
     (key :returns String :keyword t)
     (display :returns String :keyword t))
    (DiffStats
     (total_added :returns Integer :keyword t)
     (total_removed :returns Integer :keyword t))
    (Email
     (local :returns String :keyword t)
     (domain :returns String :keyword t))
    (List
     (len :returns Integer :keyword t)
     (join :args ((separator Template)) :returns Template)
     (filter :args ((predicate Lambda)) :returns List)
     (map :args ((mapper Lambda)) :returns ListTemplate)
     (any :args ((predicate Lambda)) :returns Boolean)
     (all :args ((predicate Lambda)) :returns Boolean))
    (List-Trailer
     (contains_key :args ((key Stringify)) :returns Boolean))
    (ListTemplate
     (join :args ((separator Template)) :returns Template))
    (Operation
     (current_operation :returns Boolean :keyword t)
     (description :returns String :keyword t)
     (id :returns OperationId :keyword t)
     (tags :returns String :keyword t)
     (time :returns TimestampRange :keyword t)
     (user :returns String :keyword t)
     (snapshot :returns Boolean :keyword t)
     (root :returns Boolean :keyword t)
     (parents :returns List :keyword t))
    (OperationId
     (short :args ((len Integer :optional t)) :returns String))
    (RepoPath
     (display :returns String :keyword t)
     (parent :returns Option :keyword t))
    (ShortestIdPrefix
     (prefix :returns String :keyword t)
     (rest :returns String :keyword t)
     (upper :returns ShortestIdPrefix :keyword t)
     (lower :returns ShortestIdPrefix :keyword t))
    (Signature
     (name :returns String :keyword t)
     (email :returns Email :keyword t)
     (timestamp :returns Timestamp :keyword t))
    (SizeHint
     (lower :returns Integer :keyword t)
     (upper :returns Option :keyword t)
     (exact :returns Option :keyword t)
     (zero :returns Boolean :keyword t))
    (String
     (len :returns Integer :keyword t)
     (contains :args ((needle Stringify)) :returns Boolean)
     (match :args ((needle StringPattern)) :returns String)
     (replace :args ((pattern StringPattern)
                     (replacement Stringify)
                     (limit Integer :optional t))
              :returns String)
     (first_line :returns String :keyword t)
     (lines :returns List :keyword t)
     (upper :returns String :keyword t)
     (lower :returns String :keyword t)
     (starts_with :args ((needle Stringify)) :returns Boolean)
     (ends_with :args ((needle Stringify)) :returns Boolean)
     (remove_prefix :args ((needle Stringify)) :returns String)
     (remove_suffix :args ((needle Stringify)) :returns String)
     (trim :returns String :keyword t)
     (trim_start :returns String :keyword t)
     (trim_end :returns String :keyword t)
     (substr :args ((start Integer) (end Integer)) :returns String)
     (escape_json :returns String :keyword t))
    (Timestamp
     (ago :returns String :keyword t)
     (format :args ((format String)) :returns String)
     (utc :returns Timestamp :keyword t)
     (local :returns Timestamp :keyword t)
     (after :args ((date String)) :returns Boolean))
    (TimestampRange
     (start :returns Timestamp :keyword t)
     (end :returns Timestamp :keyword t)
     (duration :returns String :keyword t))
    (Trailer
     (key :returns String :keyword t)
     (value :returns String :keyword t))
    (TreeDiff
     (files :returns List :keyword t)
     (color_words :args ((context Integer :optional t)) :returns Template)
     (git :args ((context Integer :optional t)) :returns Template)
     (stat :args ((width Integer :optional t)) :returns DiffStats)
     (summary :returns Template :keyword t))
    (TreeDiffEntry
     (path :returns RepoPath :keyword t)
     (status :returns String :keyword t)
     (source :returns TreeEntry :keyword t)
     (target :returns TreeEntry :keyword t))
    (TreeEntry
     (path :returns RepoPath :keyword t)
     (conflict :returns Boolean :keyword t)
     (file_type :returns String :keyword t)
     (executable :returns Boolean :keyword t))
    (WorkspaceRef
     (name :returns RefSymbol :keyword t)
     (target :returns Commit :keyword t)))
  "Built-in template method metadata.")

(majutsu-template--register-methods majutsu-template--builtin-method-specs)

;;;; AST representation (work in progress) ###################################

(cl-defstruct (majutsu-template-node
               (:constructor majutsu-template-node-create
                             (&key kind type value args props)))
  "Internal representation for compiled template expressions."
  kind
  type
  value
  args
  props)

(defun majutsu-template--literal-node (text &optional type props)
  "Return literal template node for TEXT with optional TYPE hint."
  (majutsu-template-node-create
   :kind :literal
   :type (or type 'String)
   :value text
   :props props))

(defun majutsu-template--raw-node (text &optional type props)
  "Return raw template node for verbatim TEXT."
  (majutsu-template-node-create
   :kind :raw
   :type (or type 'Template)
   :value text
   :props props))

(defun majutsu-template--call-node (name args &optional type props)
  "Return call node NAME with ARGS and optional TYPE metadata."
  (majutsu-template-node-create
   :kind :call
   :type type
   :value name
   :args args
   :props props))

(defun majutsu-template--expand-elisp (form)
  "Expand embedded Elisp expressions inside FORM when allowed."
  (cond
   ((majutsu-template-node-p form) form)
   ((and (consp form) (keywordp (car form)))
    (majutsu-template--sugar-apply (car form) (cdr form)))
   ((vectorp form)
    (let ((items (mapcar #'majutsu-template--expand-elisp (append form nil))))
      (apply #'vector items)))
   ((and (consp form) (eq (car form) 'quote)) form)
   ((and (consp form) (keywordp (car form)))
    (cons (car form) (mapcar #'majutsu-template--expand-elisp (cdr form))))
   ((and (consp form) majutsu-template--allow-eval)
    (majutsu-template--expand-elisp (eval form)))
   ((consp form)
    (cons (majutsu-template--expand-elisp (car form))
          (mapcar #'majutsu-template--expand-elisp (cdr form))))
   (t form)))

(defun majutsu-template--rewrite (form)
  "Rewrite literal FORM into normalized AST nodes.
Further passes (type-checking, rendering) operate on these nodes."
  (let ((expanded (if majutsu-template--allow-eval
                      (majutsu-template--expand-elisp form)
                    form)))
    (majutsu-template--sugar-transform expanded)))

(majutsu-template-defun concat ((forms Template :rest t))
  (:returns Template :doc "concat(FORMS...).")
  (apply #'majutsu-template-call 'concat forms))

(majutsu-template-defun if ((condition Template)
                            (then Template)
                            (else Template :optional t))
  (:returns Template :doc "if(COND, THEN [, ELSE]).")
  (if else
      (majutsu-template-call 'if condition then else)
    (majutsu-template-call 'if condition then)))

(majutsu-template-defun separate ((separator Template)
                                  (forms Template :rest t))
  (:returns Template :doc "separate(SEP, FORMS...).")
  (apply #'majutsu-template-call 'separate separator forms))

(majutsu-template-defun surround ((pre Template)
                                  (post Template)
                                  (body Template))
  (:returns Template :doc "surround(PRE, POST, BODY).")
  (majutsu-template-call 'surround pre post body))

(majutsu-template-defun json ((value Template))
  (:returns Template :doc "json(FORM).")
  (majutsu-template-call 'json value))
;; Internal node representation: (:tag ...)

(defun majutsu-template--str-escape (s)
  "Escape S into a jj double-quoted string literal content.

Supported escapes (aligned with jj docs):
  \" \\ \t \r \n \0 \e and generic control bytes as \\xHH.
All other characters are emitted verbatim (UTF-8 allowed)."
  (unless (stringp s)
    (user-error "majutsu-template: expected string, got %S" s))
  (apply #'concat
         (cl-loop for ch across s
                  collect
                  (pcase ch
                    (?\" "\\\"")          ; double quote
                    (?\\ "\\\\")          ; backslash
                    (?\t "\\t")            ; tab
                    (?\r "\\r")            ; carriage return
                    (?\n "\\n")            ; newline
                    (0   "\\0")            ; NUL
                    (27  "\\e")            ; ESC (0x1b)
                    (_
                     (if (or (< ch 32) (= ch 127))
                         (format "\\x%02X" ch) ; other ASCII controls as \xHH
                       (string ch)))))))

(defun majutsu-template-str (s)
  "Literal string node for jj template language."
  (majutsu-template--literal-node s 'String))

(defun majutsu-template-raw (text)
  "Raw snippet injected verbatim. Use sparingly."
  (majutsu-template--raw-node text))

(defun majutsu-template-call (name &rest args)
  "Call a jj template function NAME with ARGS (already normalized forms)."
  (let ((name-str (majutsu-template--normalize-call-name name)))
    (majutsu-template--call-node
     name-str
     (mapcar #'majutsu-template--ensure-node args))))

(defun majutsu-template--literal-string-from-node (node)
  "Return literal string content from NODE if it is :str or :raw."
  (cond
   ((majutsu-template-node-p node)
    (pcase (majutsu-template-node-kind node)
      (:literal (majutsu-template-node-value node))
      (:raw (majutsu-template-node-value node))
      (_ (user-error "majutsu-template: expected literal/raw node, got %S" node))))
   ((and (consp node) (eq (car node) :str))
    (cadr node))
   ((and (consp node) (eq (car node) :raw))
    (cadr node))
   (t
    (user-error "majutsu-template: expected literal string node, got %S" node))))

(defun majutsu-template--resolve-call-name (expr)
  "Resolve EXPR into a call name (string or symbol)."
  (cond
   ((or (stringp expr) (symbolp expr) (keywordp expr)) expr)
   ((majutsu-template-node-p expr)
    (majutsu-template--literal-string-from-node expr))
   ((vectorp expr)
    (let ((node (majutsu-template--sugar-transform expr)))
      (majutsu-template--literal-string-from-node node)))
   ((and (consp expr) (eq (car expr) 'quote))
    (majutsu-template--resolve-call-name (cadr expr)))
   ((consp expr)
    (majutsu-template--resolve-call-name (eval expr)))
   (t expr)))

(defun majutsu-template-label (name value)
  "label(NAME, VALUE). NAME is auto-quoted."
  (majutsu-template-call 'label (majutsu-template-str (format "%s" name)) value))

(defun majutsu-template-join (sep coll var body)
  "Emit COLL.map(|VAR| BODY).join(SEP).
SEP is a form/string. COLL is a raw expression form. VAR is a symbol/name.
BODY may reference VAR using raw sub-expressions."
  (let* ((sep-s (majutsu-template--compile sep))
         (coll-s (majutsu-template--compile coll))
         (body-s (majutsu-template--compile body)))
    (majutsu-template-raw
     (format "%s.map(|%s| %s).join(%s)"
             coll-s var body-s sep-s))))

(defun majutsu-template--normalize (x)
  "Normalize X into an AST node."
  (majutsu-template--rewrite x))

(defun majutsu-template--ensure-node (form)
  "Return AST node corresponding to FORM."
  (majutsu-template--normalize form))

(defun majutsu-template--render-node (node)
  "Render AST NODE to jj template string."
  (pcase (majutsu-template-node-kind node)
    (:literal
     (format "\"%s\"" (majutsu-template--str-escape (majutsu-template-node-value node))))
    (:raw
     (majutsu-template-node-value node))
    (:call
     (let* ((name (majutsu-template-node-value node))
            (args (majutsu-template-node-args node))
            (compiled-args (mapconcat #'majutsu-template--render-node args ", ")))
       (format "%s(%s)" name compiled-args)))
    (_
     (user-error "majutsu-template: unknown AST node kind %S" (majutsu-template-node-kind node)))))

(defun majutsu-template--compile-list (xs)
  (mapconcat (lambda (x)
               (majutsu-template--render-node (majutsu-template--ensure-node x)))
             xs ", "))

(defun majutsu-template--compile (form)
  "Compile FORM (node or literal) to jj template string."
  (majutsu-template--render-node (majutsu-template--ensure-node form)))

(defun majutsu-template-ast (form)
  "Return AST node representing FORM without rendering."
  (majutsu-template--ensure-node form))

(defun majutsu-template--compile-op (op args)
  "Compile a simple operator expression OP with ARGS to a raw node.
Parentheses are added to avoid precedence issues."
  (let ((to-s (lambda (x) (majutsu-template--compile (majutsu-template--sugar-transform x)))))
    (pcase op
      ((or :not 'not)
       (let ((a (funcall to-s (cadr args))))
         (majutsu-template-raw (format "(!%s)" a))))
      ((or :neg 'neg)
       (let ((a (funcall to-s (cadr args))))
         (majutsu-template-raw (format "(-%s)" a))))
      ((or :+ '+ :sub '- :* '* :/ '/ :% '% :>= '>= :> '> :<= '<= :< '< :== '== :!= '!=)
       (let* ((a (funcall to-s (cadr args)))
              (b (funcall to-s (caddr args)))
              (tok (pcase op
                     ((or :+ '+) "+")
                     ((or :sub '-) "-")
                     ((or :* '*) "*")
                     ((or :/ '/) "/")
                     ((or :% '%) "%")
                     ((or :>= '>=) ">=")
                     ((or :> '>) ">")
                     ((or :<= '<=) "<=")
                     ((or :< '<) "<")
                     ((or :== '==) "==")
                     ((or :!= '!=) "!="))))
         (majutsu-template-raw (format "(%s %s %s)" a tok b))))
      ((or :and 'and)
       (let ((a (funcall to-s (cadr args)))
             (b (funcall to-s (caddr args))))
         (majutsu-template-raw (format "(%s && %s)" a b))))
      ((or :or 'or)
       (let ((a (funcall to-s (cadr args)))
             (b (funcall to-s (caddr args))))
         (majutsu-template-raw (format "(%s || %s)" a b))))
      ((or :concat-op 'concat-op)
       (let ((a (funcall to-s (cadr args)))
             (b (funcall to-s (caddr args))))
         (majutsu-template-raw (format "(%s ++ %s)" a b))))
      (_ (user-error "majutsu-template: unknown operator %S" op)))))

;;;###autoload
(defun majutsu-template-compile (form)
  "Public entry point: compile FORM into a jj template string."
  (majutsu-template--compile form))

;; Convenience macro sugar (EmacSQL-inspired)
;; Vectors (e.g., [:concat ...]) and lists are both accepted.
;; Operators may be keywords (:if) or symbols (if); tpl-* aliases also work.

(defconst majutsu-template--sugar-ops
  '((:str . majutsu-template-str)
    (:raw . majutsu-template-raw)
    (:call . majutsu-template-call)
    (:concat . majutsu-template-concat)
    (:if . majutsu-template-if)
    (:label . majutsu-template-label)
    (:separate . majutsu-template-separate)
    (:surround . majutsu-template-surround)
    (:json . majutsu-template-json)
    (:join . majutsu-template-join)
    (:method . majutsu-template-call)
    (:map . majutsu-template-join)
    (:filter . majutsu-template-join)
    (:any . majutsu-template-join)
    (:all . majutsu-template-join))
  "Operator mapping for the `tpl` macro sugar.")

(defun majutsu-template--sugar-transform (form)
  "Transform compact FORM into the template AST."
  (cond
   ((majutsu-template-node-p form) form)
   ((numberp form) (majutsu-template-raw (number-to-string form)))
   ((eq form t) (majutsu-template-raw "true"))
   ((eq form nil) (majutsu-template-raw "false"))
   ((stringp form) (majutsu-template-str form))
   ((symbolp form) (majutsu-template-raw (symbol-name form)))
   ((vectorp form)
    (let* ((list-form (append form nil))
           (normalized (if (and list-form (symbolp (car list-form)))
                           list-form
                         (cons :concat list-form))))
      (majutsu-template--sugar-transform normalized)))
   ((and (consp form) (eq (car form) 'quote))
    (majutsu-template--sugar-transform (cadr form)))
   ((and (consp form) (symbolp (car form)))
    (majutsu-template--sugar-apply (car form) (cdr form)))
   ((and majutsu-template--allow-eval (consp form))
    (majutsu-template--sugar-transform (eval form)))
   ((consp form)
    (user-error "majutsu-template: use vector syntax [:op ...], lists are not accepted: %S" form))
   (t
    (user-error "majutsu-template: unsupported literal in sugar %S" form))))

(defun majutsu-template--sugar-apply (op args)
  "Dispatch helper applying OP to ARGS within sugar transformation."
  (let* ((original op)
         (head (cond
                ((keywordp op) (intern (substring (symbol-name op) 1)))
                (t op)))
         (_ (when (memq head '(method .)) (setq head 'method)))
         (custom-meta (or (majutsu-template--lookup-function-meta original)
                          (majutsu-template--lookup-function-meta head))))
    (when (and (not (keywordp original))
               (not custom-meta)
               (not (memq head '(method)))
               (not (memq original '(vector))))
      (user-error "majutsu-template: unknown operator %S" original))
    (cond
     (custom-meta
      (apply (majutsu-template--fn-symbol custom-meta)
             (mapcar #'majutsu-template--sugar-transform args)))
     ((eq head 'str)
      (unless (= (length args) 1)
        (user-error "majutsu-template: :str expects 1 argument"))
      (majutsu-template-str (car args)))
     ((eq head 'raw)
      (let* ((value (car args))
             (declared-type (cadr args))
             (type (and declared-type (majutsu-template--normalize-type-symbol declared-type)))
             (string
              (cond
               ((stringp value) value)
               ((symbolp value) (symbol-name value))
               ((vectorp value)
                (let ((node (majutsu-template--sugar-transform value)))
                  (majutsu-template--literal-string-from-node node)))
               ((consp value)
                (let ((result (eval value)))
                  (unless (stringp result)
                    (user-error "majutsu-template: :raw expression must produce a string, got %S" result))
                  result))
               (t
                (user-error "majutsu-template: :raw expects a string literal, got %S" value)))))
        (majutsu-template--raw-node string type (when type (list :declared type)))))
    ((eq head 'concat)
      (apply #'majutsu-template-call 'concat
             (mapcar #'majutsu-template--sugar-transform args)))
     ((eq head 'if)
      (let ((cond-form (majutsu-template--sugar-transform (car args)))
            (then-form (majutsu-template--sugar-transform (cadr args)))
            (else-form (when (cddr args)
                         (majutsu-template--sugar-transform (caddr args)))))
        (if else-form
            (majutsu-template-if cond-form then-form else-form)
          (majutsu-template-if cond-form then-form))))
     ((eq head 'label)
      (let ((name (car args))
            (value (majutsu-template--sugar-transform (cadr args))))
        (majutsu-template-label name value)))
     ((eq head 'separate)
      (apply #'majutsu-template-separate
             (majutsu-template--sugar-transform (car args))
             (mapcar #'majutsu-template--sugar-transform (cdr args))))
     ((eq head 'surround)
      (let ((pre (majutsu-template--sugar-transform (car args)))
            (post (majutsu-template--sugar-transform (cadr args)))
            (body (majutsu-template--sugar-transform (caddr args))))
        (majutsu-template-surround pre post body)))
     ((eq head 'json)
      (majutsu-template-json (majutsu-template--sugar-transform (car args))))
     ((eq head 'call)
      (let* ((name-expr (car args))
             (resolved-name (majutsu-template--resolve-call-name name-expr))
             (meta (or (majutsu-template--lookup-function-meta resolved-name)
                       (majutsu-template--lookup-function-meta
                        (majutsu-template--normalize-call-name resolved-name)))))
        (if meta
            (apply (majutsu-template--fn-symbol meta)
                   (mapcar #'majutsu-template--sugar-transform (cdr args)))
          (let ((call-args (mapcar #'majutsu-template--sugar-transform (cdr args))))
            (apply #'majutsu-template-call resolved-name call-args)))))
     ((eq head 'join)
      (let ((sep (majutsu-template--sugar-transform (car args)))
            (coll (majutsu-template--sugar-transform (cadr args)))
            (var (nth 2 args))
            (body (majutsu-template--sugar-transform (nth 3 args))))
        (majutsu-template-join sep coll var body)))
     ((memq head '(map filter any all))
      (apply #'majutsu-template--sugar-map-like head args))
     ((eq head 'method)
      (apply #'majutsu-template--sugar-method args))
     ((memq head '(:+ + :sub - :* * :/ / :% % :>= >= :> > :<= <= :< <
                   :== == :!= != :and and :or or :not not :neg neg :concat-op concat-op))
      (majutsu-template--compile-op head (cons head args)))
     ((memq head '(coalesce fill indent pad_start pad_end pad_centered
                   truncate_start truncate_end hash stringify raw_escape_sequence config))
      (apply #'majutsu-template-call head
             (mapcar #'majutsu-template--sugar-transform args)))
     (t
      (user-error "majutsu-template: unknown operator %S" original)))))

(defun majutsu-template--sugar-map-like (op coll var body)
  "Helper for map/filter/any/all forms."
  (unless (symbolp var)
    (user-error "majutsu-template: %s expects a symbol variable" op))
  (let* ((coll-s (majutsu-template--compile (majutsu-template--sugar-transform coll)))
         (body-s (majutsu-template--compile (majutsu-template--sugar-transform body)))
         (fmt (cond
               ((eq op 'map) "%s.map(|%s| %s)")
               ((eq op 'filter) "%s.filter(|%s| %s)")
               ((eq op 'any) "%s.any(|%s| %s)")
               ((eq op 'all) "%s.all(|%s| %s)")
               (t (user-error "majutsu-template: unsupported map-like operator %S" op)))))
    (majutsu-template-raw (format fmt coll-s var body-s))))

(defun majutsu-template--sugar-method (obj name &rest args)
  "Helper for method chaining sugar."
  (let* ((obj-s (majutsu-template--compile (majutsu-template--sugar-transform obj)))
         (method (majutsu-template--normalize-call-name name))
         (arg-str (mapconcat (lambda (a)
                               (majutsu-template--compile (majutsu-template--sugar-transform a)))
                             args ", ")))
    (majutsu-template-raw
     (format "%s.%s(%s)" obj-s method arg-str))))
;;;###autoload
(defmacro tpl (form)
  "Expand a template program FORM (must be a vector literal) into the EDSL AST."
  (unless (vectorp form)
    (user-error "tpl: top-level form must be a vector, e.g., [:concat ...]"))
  (let ((majutsu-template--allow-eval t))
    (let ((node (majutsu-template--rewrite form)))
      `(quote ,node))))

;;;###autoload
(defmacro tpl-compile (form)
  "Expand and compile FORM to a jj template string.
Vector literals are compiled at macro-expansion time. Non-vector forms are
evaluated at runtime and normalized via `majutsu-template--normalize'."
  (if (vectorp form)
      (let ((majutsu-template--allow-eval t))
        (let ((node (majutsu-template--rewrite form)))
          `(majutsu-template-compile ',node)))
    `(let ((majutsu-template--allow-eval nil))
       (majutsu-template-compile (majutsu-template--normalize ,form)))))

(provide 'majutsu-template)
