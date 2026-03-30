;;; majutsu-template.el --- Embedded DSL for jj template  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library implements a small Elisp DSL for composing jj template
;; strings and normalizing types.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup majutsu-template nil
  "Elisp wrapper for composing jj templates."
  :group 'majutsu)

(defvar majutsu-template--allow-eval nil
  "When non-nil, cons forms encountered during sugar transformation may be eval'd.")

;;; Self binding context

(cl-defstruct (majutsu-template--self-binding
               (:constructor majutsu-template--make-self-binding))
  "Binding describing the current implicit `self' object."
  node
  type)

(defcustom majutsu-template-default-self-type 'Commit
  "Default top-level template type assumed for implicit `self' keywords.
This is only installed as a root self binding at public compile entry points.
Set it to nil to disable top-level keyword rewriting unless explicitly bound."
  :type '(choice (const :tag "None" nil) symbol)
  :group 'majutsu-template)

(defvar majutsu-template--self-stack nil
  "Dynamic stack describing the current implicit `self' binding.")

(defun majutsu-template--current-self ()
  "Return the current implicit `self' binding, or nil when none exists."
  (car majutsu-template--self-stack))

(defun majutsu-template--root-self-type (&optional explicit-type)
  "Return root self type to install for EXPLICIT-TYPE, or nil.
A synthetic root binding is only created when no more local self binding is
currently active."
  (when (null majutsu-template--self-stack)
    (when-let* ((type (or explicit-type majutsu-template-default-self-type)))
      (majutsu-template--normalize-type-symbol type))))

(defun majutsu-template--call-with-root-self-binding (explicit-type thunk)
  "Call THUNK with a synthetic root self binding when EXPLICIT-TYPE requires it."
  (if-let* ((type (majutsu-template--root-self-type explicit-type)))
      (majutsu-template--call-with-self-binding
       (majutsu-template--raw-node "self" type)
       type
       thunk)
    (funcall thunk)))

(defun majutsu-template--call-with-self-binding (node type thunk)
  "Call THUNK with NODE/TYPE temporarily bound as current `self'.
If NODE is nil, call THUNK without changing the current binding."
  (let ((majutsu-template--self-stack
         (if node
             (cons (majutsu-template--make-self-binding :node node :type type)
                   majutsu-template--self-stack)
           majutsu-template--self-stack)))
    (funcall thunk)))

;;; Lexical variable binding context

(cl-defstruct (majutsu-template--binding
               (:constructor majutsu-template--make-binding))
  "Lexical binding describing a lambda parameter."
  name
  type
  node)

(defvar majutsu-template--binding-stack nil
  "Dynamic stack of lexical template variable bindings.")

(defun majutsu-template--lookup-binding (name)
  "Return lexical binding for NAME, or nil if NAME is unbound."
  (cl-find-if (lambda (binding)
                (eq (majutsu-template--binding-name binding) name))
              majutsu-template--binding-stack))

(defun majutsu-template--call-with-bindings (bindings thunk)
  "Call THUNK with lexical BINDINGS temporarily active."
  (let ((majutsu-template--binding-stack
         (append bindings majutsu-template--binding-stack)))
    (funcall thunk)))

;;;; Syntax specials

(eval-and-compile
  (defconst majutsu-template--no-special-expansion
    (make-symbol "majutsu-template-no-special-expansion")
    "Sentinel returned when no syntax special matches an operator.")

  (defvar majutsu-template--special-registry (make-hash-table :test #'eq)
    "Registry mapping exact syntax operators to expansion functions.")

  (defvar majutsu-template--special-resolvers nil
    "List of pattern-based syntax special resolver functions.")

  (defun majutsu-template-define-special (name expander)
    "Register syntax special NAME handled by EXPANDER.
EXPANDER receives raw operator arguments and returns a lowered form or node."
    (unless (symbolp name)
      (user-error "majutsu-template: special name must be a symbol, got %S" name))
    (unless (or (functionp expander) (symbolp expander))
      (user-error "majutsu-template: special %S requires a callable expander, got %S"
                  name expander))
    (puthash name expander majutsu-template--special-registry)
    expander)

  (defun majutsu-template-define-special-resolver (resolver)
    "Register pattern-based syntax special RESOLVER.
RESOLVER is called with (OP ARGS) and should return either a lowered form/node
or `majutsu-template--no-special-expansion'."
    (unless (or (functionp resolver) (symbolp resolver))
      (user-error "majutsu-template: special resolver must be callable, got %S" resolver))
    (unless (memq resolver majutsu-template--special-resolvers)
      (setq majutsu-template--special-resolvers
            (append majutsu-template--special-resolvers (list resolver))))
    resolver)

  (defun majutsu-template--expand-special (op args)
    "Expand syntax special OP with raw ARGS, if one exists."
    (or (when-let* ((expander (gethash op majutsu-template--special-registry)))
          (apply expander args))
        (cl-loop for resolver in majutsu-template--special-resolvers
                 for expansion = (funcall resolver op args)
                 unless (eq expansion majutsu-template--no-special-expansion)
                 return expansion)
        majutsu-template--no-special-expansion)))

(defmacro majutsu-template-defspecial (name args &rest body)
  "Define syntax special NAME taking raw ARGS and expanding via BODY.
BODY should return a lowered form or a final template node."
  (declare (indent defun))
  (unless (symbolp name)
    (user-error "majutsu-template-defspecial: NAME must be a symbol, got %S" name))
  (let* ((base (if (keywordp name)
                   (substring (symbol-name name) 1)
                 (symbol-name name)))
         (fn-symbol (intern (format "majutsu-template--special-%s" base))))
    `(progn
       (defun ,fn-symbol ,args
         ,(format "Syntax special expander for %s." name)
         ,@body)
       (eval-and-compile
         (majutsu-template-define-special ',name #',fn-symbol)))))

;;;; Type and callable metadata

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

(defun majutsu-template--type-ref-normalize (type)
  "Normalize TYPE into a minimal internal type reference."
  (cond
   ((null type) 'Unknown)
   ((and (consp type) (memq (car type) '(:list :option :lambda)))
    (pcase (car type)
      (:list (list :list (majutsu-template--type-ref-normalize (cadr type))))
      (:option (list :option (majutsu-template--type-ref-normalize (cadr type))))
      (:lambda (list :lambda
                     (mapcar #'majutsu-template--type-ref-normalize (cadr type))
                     (majutsu-template--type-ref-normalize (caddr type))))))
   ((or (keywordp type) (symbolp type)) type)
   ((stringp type) (intern type))
   (t 'Unknown)))

(defun majutsu-template--type-ref-base-type (type)
  "Return nominal base type symbol for TYPE reference."
  (setq type (majutsu-template--type-ref-normalize type))
  (cond
   ((symbolp type)
    (pcase type
      (:self 'Template)
      (:element 'Template)
      (:option-value 'Template)
      (_ type)))
   ((consp type)
    (pcase (car type)
      (:list 'List)
      (:option 'Option)
      (:lambda 'Lambda)
      (_ 'Unknown)))
   (t 'Unknown)))

(defun majutsu-template--type-ref-dispatch-fragment (type)
  "Return canonical dispatch-name fragment derived from TYPE reference."
  (setq type (majutsu-template--type-ref-normalize type))
  (pcase type
    ((pred symbolp)
     (symbol-name (majutsu-template--type-ref-base-type type)))
    (`(:list ,inner)
     (concat (majutsu-template--type-ref-dispatch-fragment inner) "List"))
    (`(:option ,inner)
     (concat (majutsu-template--type-ref-dispatch-fragment inner) "Opt"))
    (`(:lambda . ,_)
     "Lambda")
    (_
     (symbol-name (majutsu-template--type-ref-base-type type)))))

(defun majutsu-template--type-ref-dispatch-kind (type)
  "Return internal dispatch kind symbol derived from TYPE reference."
  (intern (majutsu-template--type-ref-dispatch-fragment type)))

(defun majutsu-template--method-dispatch-candidates (receiver-type)
  "Return ordered `(SEMANTIC-TYPE . DISPATCH-KIND)' candidates for RECEIVER-TYPE."
  (setq receiver-type (majutsu-template--type-ref-normalize receiver-type))
  (cl-delete-duplicates
   (pcase receiver-type
     (`(:list ,_)
      (list (cons receiver-type
                  (majutsu-template--type-ref-dispatch-kind receiver-type))
            (cons receiver-type 'List)))
     (`(:option ,inner)
      (append (list (cons receiver-type
                          (majutsu-template--type-ref-dispatch-kind receiver-type))
                    (cons receiver-type 'Option))
              (majutsu-template--method-dispatch-candidates inner)))
     (`(:lambda . ,_)
      (list (cons receiver-type 'Lambda)))
     (_
      (list (cons receiver-type
                  (majutsu-template--type-ref-dispatch-kind receiver-type)))))
   :test #'equal))

(defun majutsu-template--type-ref-element-type (type)
  "Return element/value type carried by TYPE when available."
  (setq type (majutsu-template--type-ref-normalize type))
  (and (consp type)
       (pcase (car type)
         ((or :list :option) (cadr type))
         (_ nil))))

(defun majutsu-template--type-ref-lambda-arg-types (type)
  "Return lambda argument type list carried by TYPE, or nil."
  (setq type (majutsu-template--type-ref-normalize type))
  (and (consp type)
       (eq (car type) :lambda)
       (cadr type)))

(defun majutsu-template--type-ref-lambda-return-type (type)
  "Return lambda return type carried by TYPE, or nil."
  (setq type (majutsu-template--type-ref-normalize type))
  (and (consp type)
       (eq (car type) :lambda)
       (caddr type)))

(defun majutsu-template--node-type-ref (node)
  "Return normalized semantic type reference for NODE."
  (majutsu-template--type-ref-normalize
   (and (majutsu-template-node-p node)
        (majutsu-template-node-type node))))

(defun majutsu-template--type-explicitly-converts-to-p (actual-base expected-base)
  "Return non-nil if ACTUAL-BASE explicitly converts to EXPECTED-BASE."
  (and (symbolp actual-base)
       (symbolp expected-base)
       (memq (alist-get expected-base
                        (and (majutsu-template--lookup-type actual-base)
                             (majutsu-template--type-converts-to
                              (majutsu-template--lookup-type actual-base))))
             '(yes maybe))))

(defun majutsu-template--capability-type-p (type)
  "Return non-nil if TYPE denotes a capability-style target type."
  (memq (majutsu-template--type-ref-base-type type)
        '(Any Template Stringify Serialize)))

(defun majutsu-template--type-structurally-compatible-p (actual expected)
  "Return non-nil if ACTUAL and EXPECTED are structurally compatible."
  (setq actual (majutsu-template--type-ref-normalize actual)
        expected (majutsu-template--type-ref-normalize expected))
  (let ((actual-base (majutsu-template--type-ref-base-type actual))
        (expected-base (majutsu-template--type-ref-base-type expected)))
    (cond
     ((equal actual expected) t)
     ((and (consp actual) (consp expected) (eq (car actual) :list) (eq (car expected) :list))
      (majutsu-template--type-convertible-p (cadr actual) (cadr expected)))
     ((and (consp actual) (consp expected) (eq (car actual) :option) (eq (car expected) :option))
      (majutsu-template--type-convertible-p (cadr actual) (cadr expected)))
     ((and (consp actual) (consp expected) (eq (car actual) :lambda) (eq (car expected) :lambda)
           (= (length (cadr actual)) (length (cadr expected))))
      (and (cl-every #'identity
                     (cl-mapcar #'majutsu-template--type-convertible-p
                                (cadr actual) (cadr expected)))
           (majutsu-template--type-convertible-p (caddr actual) (caddr expected))))
     ((eq actual-base expected-base) t)
     (t nil))))

(defun majutsu-template--type-supports-capability-p (actual expected)
  "Return non-nil if ACTUAL satisfies capability-style EXPECTED."
  (setq actual (majutsu-template--type-ref-normalize actual)
        expected (majutsu-template--type-ref-normalize expected))
  (let ((actual-base (majutsu-template--type-ref-base-type actual))
        (expected-base (majutsu-template--type-ref-base-type expected)))
    (pcase expected-base
      ('Any t)
      ('Template
       (or (eq actual-base 'Template)
           (majutsu-template--type-explicitly-converts-to-p actual-base 'Template)))
      ('Stringify
       (or (eq actual-base 'Stringify)
           (majutsu-template--type-explicitly-converts-to-p actual-base 'Template)))
      ('Serialize
       (or (eq actual-base 'Serialize)
           (majutsu-template--type-explicitly-converts-to-p actual-base 'Serialize)))
      (_ nil))))

(defun majutsu-template--type-convertible-p (actual expected)
  "Return non-nil if ACTUAL type can be used where EXPECTED is required."
  (setq actual (majutsu-template--type-ref-normalize actual)
        expected (majutsu-template--type-ref-normalize expected))
  (let ((actual-base (majutsu-template--type-ref-base-type actual))
        (expected-base (majutsu-template--type-ref-base-type expected)))
    (cond
     ((eq expected 'Unknown) t)
     ((majutsu-template--type-structurally-compatible-p actual expected) t)
     ((majutsu-template--capability-type-p expected)
      (or (eq actual 'Unknown)
          (majutsu-template--type-supports-capability-p actual expected)))
     ((eq actual 'Unknown) nil)
     ((majutsu-template--type-explicitly-converts-to-p actual-base expected-base)
      t)
     (t nil))))

(defun majutsu-template--string-literal-node-p (node)
  "Return non-nil if NODE is a direct string literal node."
  (and (majutsu-template-node-p node)
       (eq (majutsu-template-node-kind node) :literal)
       (stringp (majutsu-template-node-value node))))

(defun majutsu-template--node-has-deferred-type-p (node)
  "Return non-nil if NODE carries a deferred semantic type placeholder."
  (and (majutsu-template-node-p node)
       (plist-get (majutsu-template-node-props node) :deferred-type)))

(defun majutsu-template--node-meets-type-constraints-p (node expected)
  "Return non-nil if NODE satisfies any extra constraints imposed by EXPECTED."
  (setq expected (majutsu-template--type-ref-normalize expected))
  (pcase expected
    ('StringLiteral
     (majutsu-template--string-literal-node-p node))
    (_ t)))

(defun majutsu-template--type-check-runtime-expected (expected)
  "Return semantic runtime type used to validate EXPECTED against a node.
Some surface-level type refs such as `StringLiteral' additionally impose
node-shape constraints, but semantically behave like a simpler runtime type."
  (setq expected (majutsu-template--type-ref-normalize expected))
  (pcase expected
    ('StringLiteral 'String)
    (_ expected)))

(defun majutsu-template--node-satisfies-type-p (node expected)
  "Return non-nil if NODE can be used where EXPECTED is required."
  (let* ((actual (majutsu-template--node-type-ref node))
         (expected (majutsu-template--type-ref-normalize expected))
         (runtime-expected (majutsu-template--type-check-runtime-expected expected)))
    (and (majutsu-template--type-convertible-p actual runtime-expected)
         (majutsu-template--node-meets-type-constraints-p node expected))))

(defun majutsu-template--assert-node-type (fn-name arg-name expected node)
  "Signal a user error if NODE does not satisfy EXPECTED for FN-NAME ARG-NAME."
  (let ((actual (majutsu-template--node-type-ref node))
        (expected (majutsu-template--type-ref-normalize expected)))
    (unless (or (majutsu-template--node-satisfies-type-p node expected)
                (and (eq actual 'Unknown)
                     (majutsu-template--node-has-deferred-type-p node)
                     (majutsu-template--node-meets-type-constraints-p node expected)))
      (user-error "majutsu-template: helper %s argument %s expects %S, got %S"
                  fn-name arg-name expected actual))))

(defun majutsu-template--resolve-return-type-ref (receiver-type return-type)
  "Resolve RETURN-TYPE in the context of RECEIVER-TYPE."
  (setq receiver-type (majutsu-template--type-ref-normalize receiver-type)
        return-type (majutsu-template--type-ref-normalize return-type))
  (cond
   ((eq return-type :self) receiver-type)
   ((eq return-type :element)
    (or (majutsu-template--type-ref-element-type receiver-type) 'Unknown))
   ((eq return-type :option-value)
    (or (majutsu-template--type-ref-element-type receiver-type) 'Unknown))
   ((and (consp return-type) (eq (car return-type) :list))
    (list :list
          (majutsu-template--resolve-return-type-ref receiver-type (cadr return-type))))
   ((and (consp return-type) (eq (car return-type) :option))
    (list :option
          (majutsu-template--resolve-return-type-ref receiver-type (cadr return-type))))
   ((and (consp return-type) (eq (car return-type) :lambda))
    (list :lambda
          (mapcar (lambda (arg-type)
                    (majutsu-template--resolve-return-type-ref receiver-type arg-type))
                  (cadr return-type))
          (majutsu-template--resolve-return-type-ref receiver-type (caddr return-type))))
   (t return-type)))

(eval-and-compile
  (cl-defstruct (majutsu-template--arg
                 (:constructor majutsu-template--make-arg))
    name type optional rest converts doc specialize)

  (cl-defstruct (majutsu-template--fn
                 (:constructor majutsu-template--make-fn))
    name
    symbol
    doc
    flavor
    args
    returns
    returns-via
    owner
    keyword
    bind-self))

(eval-and-compile
  (cl-defstruct (majutsu-template--fn-flavor
                 (:constructor majutsu-template--make-flavor))
    "Metadata describing a template helper flavor."
    name
    doc
    parent
    builder)

  (defvar majutsu-template--flavor-registry (make-hash-table :test #'eq)
    "Registry of function flavors keyed by keyword.")

  (defun majutsu-template-define-flavor (name &rest plist)
    "Register helper flavor NAME with optional metadata PLIST.
Recognised keys: :doc (string), :parent (keyword), :builder (function)."
    (cl-check-type name keyword)
    (let* ((doc (plist-get plist :doc))
           (parent (plist-get plist :parent))
           (builder (plist-get plist :builder)))
      (when (and parent (not (keywordp parent)))
        (user-error "majutsu-template: flavor %S expects :parent to be a keyword, got %S"
                    name parent))
      (let ((meta (majutsu-template--make-flavor
                   :name name
                   :doc doc
                   :parent parent
                   :builder builder)))
        (when (gethash name majutsu-template--flavor-registry)
          (message "majutsu-template: redefining flavor %S" name))
        (puthash name meta majutsu-template--flavor-registry)
        meta)))

  (defun majutsu-template--lookup-flavor (name)
    "Return flavor metadata registered for NAME (keyword) or signal error."
    (unless (keywordp name)
      (user-error "majutsu-template: flavor must be keyed by keyword, got %S" name))
    (or (gethash name majutsu-template--flavor-registry)
        (user-error "majutsu-template: unknown flavor %S" name)))

  (defun majutsu-template--resolve-flavor-builder (name)
    "Return builder function associated with flavor NAME, respecting parents."
    (when name
      (let ((flavor (majutsu-template--lookup-flavor name)))
        (or (majutsu-template--fn-flavor-builder flavor)
            (let ((parent (majutsu-template--fn-flavor-parent flavor)))
              (when parent
                (majutsu-template--resolve-flavor-builder parent)))))))

  ;; TODO majutsu-template--flavor-fn-body
  (defun majutsu-template--flavor-fn-body (context)
    "Generate default body for :fn flavor using CONTEXT plist."
    (let* ((template-name (plist-get context :template-name))
           (owner (plist-get context :owner))
           (args (plist-get context :args))
           (call-args (mapcar #'majutsu-template--arg-name args)))
      (if owner
          (let ((self-arg (car call-args)))
            (unless (and self-arg (symbolp self-arg))
              (user-error "majutsu-template: method on %S must have self argument" owner))
            (setq call-args (cdr call-args))
            (list `(majutsu-template-call ,template-name ,self-arg ,@call-args)))
        (list `(majutsu-template-call ,template-name ,@call-args)))))

  (defun majutsu-template--flavor-builtin-body (context)
    "Generate default body for :builtin flavor using CONTEXT plist."
    (if (plist-get context :owner)
        nil
      (let* ((template-name (plist-get context :template-name))
             (args (plist-get context :args))
             (required (cl-remove-if
                        (lambda (arg)
                          (or (majutsu-template--arg-optional arg)
                              (majutsu-template--arg-rest arg)))
                        args))
             (optional (cl-remove-if-not #'majutsu-template--arg-optional args))
             (rest-arg (cl-find-if #'majutsu-template--arg-rest args))
             (call-args-sym (gensym "majutsu-call-args")))
        (let ((base (if required
                        `(list ,@(mapcar #'majutsu-template--arg-name required))
                      nil))
              (optional-forms
               (mapcar
                (lambda (arg)
                  (let ((name (majutsu-template--arg-name arg)))
                    `(when ,name
                       (setq ,call-args-sym (append ,call-args-sym (list ,name))))))
                optional))
              (rest-form
               (when rest-arg
                 (let ((name (majutsu-template--arg-name rest-arg)))
                   `(when ,name
                      (setq ,call-args-sym (append ,call-args-sym ,name)))))))
          (list
           `(let ((,call-args-sym ,base))
              ,@optional-forms
              ,@(when rest-form (list rest-form))
              (apply #'majutsu-template-call ,template-name ,call-args-sym)))))))

  (defun majutsu-template--higher-order-body-form (method collection params bindings thunk)
    "Return COLLECTION.METHOD(lambda PARAMS . BODY) form with lexical BINDINGS.
THUNK is called to produce the lambda body form inside the binding scope."
    (let* ((self-binding (car bindings))
           (self-node (and self-binding
                           (majutsu-template--binding-node self-binding)))
           (body-node
            (majutsu-template--call-with-bindings
             bindings
             (lambda ()
               (majutsu-template--call-with-self-binding
                self-node nil
                (lambda ()
                  (majutsu-template--rewrite (funcall thunk)))))))
           (lambda-node (majutsu-template--lambda-node params body-node)))
      `[:method ,collection ,(intern (concat ":" method)) ,lambda-node]))

  (defun majutsu-template--flavor-body (flavor context)
    "Return auto-generated body forms for FLAVOR using CONTEXT plist."
    (let ((builder (majutsu-template--resolve-flavor-builder flavor)))
      (when builder
        (funcall builder context))))

  (majutsu-template-define-flavor :builtin
                                  :doc "Helpers that proxy jj built-in functions."
                                  :builder #'majutsu-template--flavor-builtin-body)

  (majutsu-template-define-flavor :fn
                                  :doc "Default flavor for custom helpers."
                                  :builder #'majutsu-template--flavor-fn-body)

  (majutsu-template-define-flavor :custom
                                  :doc "Blank flavor for custom helpers with no default body."))

(defvar majutsu-template--function-registry (make-hash-table :test #'equal)
  "Map template function names to `majutsu-template--fn' metadata.")

(defvar majutsu-template--function-name-map (make-hash-table :test #'eq)
  "Lookup table from symbols/keywords to template function names (strings).")

(defvar majutsu-template--method-registry (make-hash-table :test #'equal)
  "Map (DISPATCH-KIND . NAME) to template method metadata (keywords included).")

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
    (if (and (> (length type) 0) (eq (aref type 0) ?:))
        (intern (substring type 1))
      (intern type)))
   (t
    (user-error "majutsu-template: invalid type specifier %S" type))))

(defun majutsu-template--lookup-function-meta (key)
  "Return metadata struct for function identified by KEY, or nil."
  (let* ((name (cond
                ((stringp key) key)
                (t (gethash key majutsu-template--function-name-map)))))
    (or (and name (gethash name majutsu-template--function-registry))
        (when (stringp key)
          (let* ((sym (intern-soft key))
                 (keyword (intern-soft (concat ":" key)))
                 (canonical (or (and sym (gethash sym majutsu-template--function-name-map))
                                (and keyword (gethash keyword majutsu-template--function-name-map)))))
            (when canonical
              (gethash canonical majutsu-template--function-registry)))))))

(defun majutsu-template--lookup-function-name (key)
  "Return registered template name string for KEY, or nil if unknown."
  (let ((meta (majutsu-template--lookup-function-meta key)))
    (when meta
      (majutsu-template--fn-name meta))))

(defun majutsu-template--lookup-method-dispatch (receiver-type name)
  "Return `(SEMANTIC-TYPE . META)' for method NAME on RECEIVER-TYPE."
  (cl-loop for (semantic-type . dispatch-kind)
           in (majutsu-template--method-dispatch-candidates receiver-type)
           for meta = (gethash (cons dispatch-kind name)
                               majutsu-template--method-registry)
           when meta
           return (cons semantic-type meta)))

(defun majutsu-template--lookup-method (owner name)
  "Return metadata for method NAME on OWNER type.
OWNER may be a nominal type symbol or a richer normalized type ref."
  (cdr-safe (majutsu-template--lookup-method-dispatch owner name)))

(defun majutsu-template--lookup-keyword (owner name)
  "Return metadata for keyword NAME on OWNER type (alias of method lookup)."
  (let ((meta (majutsu-template--lookup-method owner name)))
    (when (and meta (majutsu-template--fn-keyword meta))
      meta)))

(defun majutsu-template--reserved-name-p (name)
  "Return non-nil if NAME (string) conflicts with reserved helpers."
  (ignore name)
  nil)

(defun majutsu-template--register-function (meta)
  "Register custom function described by META (a `majutsu-template--fn')."
  (let* ((name (majutsu-template--fn-name meta))
         (fn-symbol (majutsu-template--fn-symbol meta))
         (owner (majutsu-template--fn-owner meta))
         (keyword-flag (majutsu-template--fn-keyword meta))
         (sym (intern name))
         (keyword (intern (concat ":" name)))
         (pre-existing (gethash name majutsu-template--function-registry)))
    (if owner
        (progn
          (unless (majutsu-template--lookup-type
                   (majutsu-template--type-ref-base-type owner))
            (message "majutsu-template: warning – declaring %s for unknown type %S"
                     name owner))
          (when keyword-flag
            ;; keywords are 0-argument methods; ensure signature reflects it
            (let ((args (majutsu-template--fn-args meta)))
              (when (> (length args) 1)
                (message "majutsu-template: keyword %s on %S declares more than self argument"
                         name owner))))
          (puthash (cons (majutsu-template--type-ref-dispatch-kind owner) name)
                   meta
                   majutsu-template--method-registry))
      (when (majutsu-template--reserved-name-p name)
        (user-error "majutsu-template: %s is reserved" name))
      (when pre-existing
        (message "majutsu-template: redefining template helper %s" name))
      (puthash name meta majutsu-template--function-registry)
      (puthash sym name majutsu-template--function-name-map)
      (puthash keyword name majutsu-template--function-name-map))
    fn-symbol))

(eval-and-compile
  (defun majutsu-template--normalize-arg-specialize (fn-name arg-name arg-type specialize)
    "Validate SPECIALIZE metadata for FN-NAME ARG-NAME of ARG-TYPE."
    (when specialize
      (unless (memq specialize '(:element-lambda))
        (user-error "majutsu-template-defun %s: argument %s has unknown :specialize %S"
                    fn-name arg-name specialize))
      (unless (eq (majutsu-template--type-ref-base-type arg-type) 'Lambda)
        (user-error "majutsu-template-defun %s: argument %s uses :specialize %S but is not Lambda"
                    fn-name arg-name specialize))))

  (defun majutsu-template--parse-arg-options (name opts)
    "Internal helper to parse OPTS plist for argument NAME."
    (let ((optional nil)
          (rest nil)
          (converts nil)
          (doc nil)
          (specialize nil))
      (while opts
        (let ((key (pop opts))
              (val (pop opts)))
          (pcase key
            (:optional (setq optional (not (null val))))
            (:rest (setq rest (not (null val))))
            (:converts (setq converts val))
            (:doc (setq doc val))
            (:specialize (setq specialize val))
            (_ (user-error "majutsu-template-defun %s: unknown option %S" name key)))))
      (list optional rest converts doc specialize)))

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
               (_ (unless (or (symbolp type)
                              (keywordp type)
                              (and (consp type)
                                   (memq (car type) '(:list :option :lambda))))
                    (user-error "majutsu-template-defun %s: argument %s has invalid type %S"
                                fn-name arg-name type)))
               (opt-data (majutsu-template--parse-arg-options arg-name opts))
               (optional (nth 0 opt-data))
               (rest (nth 1 opt-data))
               (converts (nth 2 opt-data))
               (doc (nth 3 opt-data))
               (specialize (nth 4 opt-data)))
          (when (and rest (not (null (cdr (memq spec arg-specs)))))
            (user-error "majutsu-template-defun %s: :rest parameter must be last" fn-name))
          (when (and rest rest-seen)
            (user-error "majutsu-template-defun %s: only one :rest parameter allowed" fn-name))
          (when (and rest optional)
            (user-error "majutsu-template-defun %s: parameter %s cannot be both optional and :rest"
                        fn-name arg-name))
          (when (and optional rest-seen)
            (user-error "majutsu-template-defun %s: optional parameters must precede :rest" fn-name))
          (majutsu-template--normalize-arg-specialize fn-name arg-name type specialize)
          (when rest (setq rest-seen t))
          (push (majutsu-template--make-arg
                 :name arg-name
                 :type type
                 :optional optional
                 :rest rest
                 :converts converts
                 :doc doc
                 :specialize specialize)
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
                 `(setq ,name (mapcar #'majutsu-template--rewrite ,name)))
                ((majutsu-template--arg-optional arg)
                 `(when ,name (setq ,name (majutsu-template--rewrite ,name))))
                (t
                 `(setq ,name (majutsu-template--rewrite ,name)))))))

  (defun majutsu-template--build-arg-type-checks (fn-name args)
    "Return forms that validate normalized ARGS for helper FN-NAME."
    (cl-loop for arg in args
             collect
             (let ((name (majutsu-template--arg-name arg))
                   (type (majutsu-template--arg-type arg)))
               (cond
                ((majutsu-template--arg-rest arg)
                 `(dolist (majutsu-template--item ,name)
                    (majutsu-template--assert-node-type ',fn-name ',name ',type
                                                        majutsu-template--item)))
                ((majutsu-template--arg-optional arg)
                 `(when ,name
                    (majutsu-template--assert-node-type ',fn-name ',name ',type ,name)))
                (t
                 `(majutsu-template--assert-node-type ',fn-name ',name ',type ,name))))))

  (defun majutsu-template--lookup-arg (args name)
    "Return argument metadata from ARGS matching NAME, or nil."
    (cl-find-if (lambda (arg)
                  (eq (majutsu-template--arg-name arg) name))
                args))

  (defun majutsu-template--normalize-returns-via (fn-name returns-via)
    "Validate RETURNS-VIA metadata for FN-NAME and return normalized value."
    (pcase returns-via
      ('nil nil)
      (`(:list-of-lambda-return ,arg-name)
       (unless (symbolp arg-name)
         (user-error "majutsu-template-defun %s: :returns-via expects a symbol argument name, got %S"
                     fn-name arg-name))
       returns-via)
      (_
       (user-error "majutsu-template-defun %s: unsupported :returns-via %S"
                   fn-name returns-via))))

  (defun majutsu-template--validate-returns-via (fn-name returns-via args)
    "Validate RETURNS-VIA for FN-NAME against ARGS and return normalized value."
    (setq returns-via (majutsu-template--normalize-returns-via fn-name returns-via))
    (pcase returns-via
      ('nil nil)
      (`(:list-of-lambda-return ,arg-name)
       (let ((arg (majutsu-template--lookup-arg args arg-name)))
         (unless arg
           (user-error "majutsu-template-defun %s: :returns-via refers to unknown argument %S"
                       fn-name arg-name))
         (unless (eq (majutsu-template--type-ref-base-type
                      (majutsu-template--arg-type arg))
                     'Lambda)
           (user-error "majutsu-template-defun %s: :returns-via %S requires Lambda argument %S"
                       fn-name returns-via arg-name))
         returns-via))))

  (defun majutsu-template--validate-bind-self (fn-name bind-self args)
    "Validate BIND-SELF for FN-NAME against ARGS and return its arg metadata."
    (when bind-self
      (unless (symbolp bind-self)
        (user-error "majutsu-template-defun %s: :bind-self expects parameter name, got %S"
                    fn-name bind-self))
      (let ((arg (majutsu-template--lookup-arg args bind-self)))
        (unless arg
          (user-error "majutsu-template-defun %s: :bind-self parameter %S is not declared"
                      fn-name bind-self))
        (when (majutsu-template--arg-rest arg)
          (user-error "majutsu-template-defun %s: :bind-self parameter %S cannot be :rest"
                      fn-name bind-self))
        arg)))

  (defun majutsu-template--parse-signature (fn-name signature)
    "Parse SIGNATURE plist for FN-NAME, returning plist with metadata."
    (unless (and (consp signature) (eq (car signature) :returns) (cadr signature))
      (user-error "majutsu-template-defun %s: signature must start with (:returns TYPE ...)" fn-name))
    (let ((returns (cadr signature))
          (rest (cddr signature))
          (doc nil)
          (converts nil)
          (owner nil)
          (template-name nil)
          (flavor :fn)
          (keyword nil)
          (bind-self nil)
          (returns-via nil))
      (unless (or (symbolp returns)
                  (keywordp returns)
                  (and (consp returns)
                       (memq (car returns) '(:list :option :lambda))))
        (user-error "majutsu-template-defun %s: invalid return type %S" fn-name returns))
      (while rest
        (let ((key (pop rest))
              (value (pop rest)))
          (pcase key
            (:converts (setq converts value))
            (:doc (setq doc value))
            (:owner (setq owner value))
            (:template-name (setq template-name value))
            (:flavor (setq flavor value))
            (:keyword (setq keyword value))
            (:bind-self (setq bind-self value))
            (:returns-via (setq returns-via value))
            (_ (user-error "majutsu-template-defun %s: unknown signature key %S" fn-name key)))))
      (when (and owner
                 (not (or (symbolp owner)
                          (keywordp owner)
                          (and (consp owner)
                               (memq (car owner) '(:list :option :lambda))))))
        (user-error "majutsu-template-defun %s: :owner expects a type ref, got %S" fn-name owner))
      (when (and template-name (not (stringp template-name)))
        (user-error "majutsu-template-defun %s: :template-name expects string, got %S"
                    fn-name template-name))
      (setq flavor (or flavor :fn))
      (unless (keywordp flavor)
        (user-error "majutsu-template-defun %s: :flavor expects a keyword, got %S"
                    fn-name flavor))
      (setq keyword (and keyword (not (null keyword))))
      (when (and keyword (not owner))
        (user-error "majutsu-template-defun %s: :keyword requires :owner to be specified" fn-name))
      ;; Validate flavor early to surface typos during macro expansion.
      (majutsu-template--lookup-flavor flavor)
      (list :returns returns
            :converts converts
            :doc doc
            :owner owner
            :template-name template-name
            :flavor flavor
            :keyword keyword
            :bind-self bind-self
            :returns-via returns-via)))

  (defun majutsu-template--type-ref-string (type)
    "Return readable string form for TYPE reference."
    (setq type (majutsu-template--type-ref-normalize type))
    (cond
     ((symbolp type) (symbol-name type))
     ((and (consp type) (eq (car type) :list))
      (format "(:list %s)"
              (majutsu-template--type-ref-string (cadr type))))
     ((and (consp type) (eq (car type) :option))
      (format "(:option %s)"
              (majutsu-template--type-ref-string (cadr type))))
     ((and (consp type) (eq (car type) :lambda))
      (format "(:lambda (%s) %s)"
              (mapconcat #'majutsu-template--type-ref-string (cadr type) " ")
              (majutsu-template--type-ref-string (caddr type))))
     (t (format "%S" type))))

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
                          " (" (majutsu-template--type-ref-string type) ")"
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

  (defun majutsu-template--ensure-owner-type (owner fn-name)
    "Validate declared OWNER type for callable FN-NAME when provided."
    (when owner
      (setq owner (majutsu-template--type-ref-normalize owner))
      (unless (majutsu-template--lookup-type
               (majutsu-template--type-ref-base-type owner))
        (message "majutsu-template: warning – declaring %s for unknown type %S"
                 fn-name owner)))
    owner)

  (defun majutsu-template-def--inherit-signature (signature owner &optional keyword)
    (append signature
            (list :owner owner)
            (when keyword '(:keyword t)))))

;;;###autoload
(defmacro majutsu-template-defun (name args signature &rest body)
  "Define a majutsu template helper NAME with ARGS, SIGNATURE and BODY.
Generates `majutsu-template-NAME' and registers it for template DSL usage."
  (declare (indent defun))
  (unless (and (symbolp name) (not (keywordp name)))
    (user-error "majutsu-template-defun: NAME must be an unprefixed symbol"))
  (let* ((name-str (symbol-name name))
         (signature-info (majutsu-template--parse-signature name signature))
         (owner (majutsu-template--ensure-owner-type
                 (plist-get signature-info :owner)
                 name))
         (template-name (or (plist-get signature-info :template-name) name-str))
         (flavor (plist-get signature-info :flavor))
         (keyword-flag (plist-get signature-info :keyword))
         (fn-symbol (intern (format "majutsu-template-%s" name-str)))
         (parsed-args (majutsu-template--parse-args name args))
         (bind-self-arg (majutsu-template--validate-bind-self
                         name
                         (plist-get signature-info :bind-self)
                         parsed-args))
         (returns-via (majutsu-template--validate-returns-via
                       name
                       (plist-get signature-info :returns-via)
                       parsed-args))
         (lambda-list (majutsu-template--build-lambda-list parsed-args))
         (normalizers (majutsu-template--build-arg-normalizers parsed-args))
         (arg-checks (majutsu-template--build-arg-type-checks name parsed-args))
         (return-type (plist-get signature-info :returns))
         (docstring (majutsu-template--compose-docstring
                     template-name (plist-get signature-info :doc) parsed-args))
         (flavor-context (list :name name
                               :template-name template-name
                               :owner owner
                               :keyword keyword-flag
                               :args parsed-args
                               :returns return-type
                               :body-forms body))
         (auto-body (and (null body)
                         (majutsu-template--flavor-body flavor flavor-context)))
         (body-forms (or body auto-body))
         (meta `(majutsu-template--make-fn
                 :name ,template-name
                 :symbol ',fn-symbol
                 :args ',parsed-args
                 :returns ',return-type
                 :returns-via ',returns-via
                 :doc ,docstring
                 :owner ',owner
                 :flavor ',flavor
                 :keyword ',keyword-flag
                 :bind-self ',(and bind-self-arg
                                   (majutsu-template--arg-name bind-self-arg)))))
    (unless body-forms
      (user-error "majutsu-template-defun %s: flavor %S does not provide a default body"
                  name flavor))
    `(progn
       (majutsu-template--register-function ,meta)
       (defun ,fn-symbol ,lambda-list
         ,docstring
         ,@normalizers
         ,@arg-checks
         ,(if bind-self-arg
              `(majutsu-template--call-with-self-binding
                ,(majutsu-template--arg-name bind-self-arg)
                ',(majutsu-template--arg-type bind-self-arg)
                (lambda ()
                  (let ((majutsu-template--result (progn ,@body-forms)))
                    (majutsu-template--rewrite majutsu-template--result))))
            `(let ((majutsu-template--result (progn ,@body-forms)))
               (majutsu-template--rewrite majutsu-template--result)))))))
(defmacro majutsu-template-defmethod (name owner args signature &rest body)
  "Define template method NAME applicable to OWNER type.
ARGS describe parameters after the implicit SELF argument.

Methods defined with this macro are *not* treated as implicit keywords
unless the SIGNATURE explicitly provides :keyword t. Use
`majutsu-template-defkeyword' for zero-argument helpers intended to
participate in keyword sugar."
  (declare (indent defun))
  (let ((extended-args (cons `(self ,owner) args)))
    `(majutsu-template-defun ,name ,extended-args
       ,(majutsu-template-def--inherit-signature signature owner)
       ,@body)))

(defmacro majutsu-template-defkeyword (name owner signature &rest body)
  "Define keyword NAME (a 0-argument method) on OWNER type."
  (declare (indent defun))
  `(majutsu-template-defun ,name ((self ,owner))
     ,(majutsu-template-def--inherit-signature signature owner t)
     ,@body))

;;; Operator macro helpers

(defmacro majutsu-template--definfix (name token)
  "Define NAME as infix operator rendering TOKEN between two operands."
  `(majutsu-template-defun ,name ((lhs Template) (rhs Template))
     (:returns Template :doc ,(format "Infix %s operator." name))
     (majutsu-template--raw-node
      (format "(%s %s %s)"
              (majutsu-template--render-node lhs)
              ,token
              (majutsu-template--render-node rhs)))))

(defmacro majutsu-template--defprefix (name token)
  "Define NAME as prefix/unary operator rendering TOKEN before operand."
  `(majutsu-template-defun ,name ((value Template))
     (:returns Template :doc ,(format "Prefix %s operator." name))
     (majutsu-template--raw-node
      (format "(%s%s)"
              ,token
              (majutsu-template--render-node value)))))

(defmacro majutsu-template--defpassthrough (name &optional doc)
  "Define NAME as helper that simply emits NAME(ARGS...)."
  `(majutsu-template-defun ,name ((values Template :rest t))
     (:returns Template :doc ,doc :flavor :builtin)))

(defconst majutsu-template--operator-aliases
  '((sub . -))
  "Alias map from DSL operator names to actual function symbols.")

(defun majutsu-template--operator-symbol (op)
  "Return canonical operator symbol for OP (keyword or symbol)."
  (let* ((sym (cond
               ((keywordp op) (intern (substring (symbol-name op) 1)))
               (t op))))
    (or (alist-get sym majutsu-template--operator-aliases) sym)))

(majutsu-template--definfix + "+")
(majutsu-template--definfix - "-")
(majutsu-template--definfix * "*")
(majutsu-template--definfix / "/")
(majutsu-template--definfix % "%")
(majutsu-template--definfix > ">")
(majutsu-template--definfix >= ">=")
(majutsu-template--definfix < "<")
(majutsu-template--definfix <= "<=")
(majutsu-template--definfix == "==")
(majutsu-template--definfix != "!=")
(majutsu-template--definfix and "&&")
(majutsu-template--definfix or "||")
(majutsu-template--definfix concat-op "++")
(majutsu-template--definfix ++ "++")
(majutsu-template--defprefix not "!")
(majutsu-template--defprefix neg "-")

(defun majutsu-template--method-stub (&rest _args)
  "Placeholder for template methods/keywords that are not executable in Elisp."
  (error "majutsu-template: method stubs are not callable at runtime"))

(defun majutsu-template--parse-type-name (name)
  "Return normalized internal type reference corresponding to NAME."
  (cond
   ((or (symbolp name) (keywordp name))
    (majutsu-template--type-ref-normalize name))
   ((stringp name)
    (majutsu-template--type-ref-normalize
     (let ((base (if (string-match "\\`\\([^<]+\\)" name)
                     (match-string 1 name)
                   name)))
       (intern base))))
   ((and (consp name) (memq (car name) '(:list :option :lambda)))
    (majutsu-template--type-ref-normalize name))
   (t
    (error "majutsu-template: invalid type name %S" name))))

(defun majutsu-template--register-method-spec (owner method-spec)
  "Register method METADATA described by METHOD-SPEC for OWNER type."
  (let* ((method-name (car method-spec))
         (plist (cdr method-spec))
         (template-name (or (plist-get plist :template-name)
                            (symbol-name method-name)))
         (keyword-flag (and (plist-get plist :keyword) t))
         (flavor (or (plist-get plist :flavor) :builtin))
         (raw-args (plist-get plist :args))
         (returns (majutsu-template--parse-type-name
                   (or (plist-get plist :returns) 'Template)))
         (returns-via (plist-get plist :returns-via))
         (doc (plist-get plist :doc))
         (args-specs (cons `(self ,owner) (or raw-args '())))
         (parsed-args (majutsu-template--parse-args method-name args-specs))
         (returns-via (majutsu-template--validate-returns-via
                       method-name returns-via parsed-args))
         (owner (majutsu-template--ensure-owner-type owner method-name))
         (meta (majutsu-template--make-fn
                :name template-name
                :symbol 'majutsu-template--method-stub
                :args parsed-args
                :returns returns
                :returns-via returns-via
                :doc doc
                :owner owner
                :flavor flavor
                :keyword keyword-flag)))
    (majutsu-template--lookup-flavor flavor)
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
    (Any
     :doc "Generic expression value."
     :converts ((Boolean . no) (Serialize . maybe) (Template . maybe)))
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
    (DiffStatEntry
     :doc "Per-file diff stat entry."
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
     :converts ((Boolean . yes) (Serialize . yes) (Template . yes)
                (StringPattern . yes)))
    (StringLiteral
     :doc "Compile-time string literal."
     :converts ((Boolean . yes) (Serialize . yes) (Template . yes)
                (String . yes) (StringLiteral . yes) (StringPattern . yes)))
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
     (trailers :returns (:list Trailer) :keyword t)
     (change_id :returns ChangeId :keyword t)
     (commit_id :returns CommitId :keyword t)
     (parents :returns (:list Commit) :keyword t)
     (author :returns Signature :keyword t)
     (committer :returns Signature :keyword t)
     (signature :returns (:option CryptographicSignature) :keyword t)
     (mine :returns Boolean :keyword t)
     (working_copies :returns (:list WorkspaceRef) :keyword t)
     (current_working_copy :returns Boolean :keyword t)
     (bookmarks :returns (:list CommitRef) :keyword t)
     (local_bookmarks :returns (:list CommitRef) :keyword t)
     (remote_bookmarks :returns (:list CommitRef) :keyword t)
     (tags :returns (:list CommitRef) :keyword t)
     (local_tags :returns (:list CommitRef) :keyword t)
     (remote_tags :returns (:list CommitRef) :keyword t)
     (divergent :returns Boolean :keyword t)
     (hidden :returns Boolean :keyword t)
     (change_offset :returns (:option Integer) :keyword t)
     (immutable :returns Boolean :keyword t)
     (contained_in :args ((revset StringLiteral)) :returns Boolean)
     (conflict :returns Boolean :keyword t)
     (empty :returns Boolean :keyword t)
     (diff :args ((files StringLiteral :optional t)) :returns TreeDiff)
     (files :args ((files StringLiteral :optional t)) :returns (:list TreeEntry))
     (conflicted_files :returns (:list TreeEntry) :keyword t)
     (root :returns Boolean :keyword t))
    (CommitEvolutionEntry
     (commit :returns Commit :keyword t)
     (operation :returns Operation :keyword t)
     (predecessors :returns (:list Commit) :keyword t)
     (inter_diff :args ((files StringLiteral :optional t)) :returns TreeDiff))
    (ChangeId
     (normal_hex :returns String :keyword t)
     (short :args ((len Integer :optional t)) :returns String)
     (shortest :args ((min_len Integer :optional t)) :returns ShortestIdPrefix))
    (CommitId
     (short :args ((len Integer :optional t)) :returns String)
     (shortest :args ((min_len Integer :optional t)) :returns ShortestIdPrefix))
    (CommitRef
     (name :returns RefSymbol :keyword t)
     (remote :returns (:option RefSymbol) :keyword t)
     (present :returns Boolean :keyword t)
     (conflict :returns Boolean :keyword t)
     (normal_target :returns (:option Commit) :keyword t)
     (removed_targets :returns (:list Commit) :keyword t)
     (added_targets :returns (:list Commit) :keyword t)
     (tracked :returns Boolean :keyword t)
     (tracking_present :returns Boolean :keyword t)
     (tracking_ahead_count :returns SizeHint :keyword t)
     (tracking_behind_count :returns SizeHint :keyword t)
     (synced :returns Boolean :keyword t))
    (ConfigValue
     (as_boolean :returns Boolean :keyword t)
     (as_integer :returns Integer :keyword t)
     (as_string :returns String :keyword t)
     (as_string_list :returns (:list String) :keyword t))
    (CryptographicSignature
     (status :returns String :keyword t)
     (key :returns String :keyword t)
     (display :returns String :keyword t))
    (DiffStatEntry
     (bytes_delta :returns Integer :keyword t)
     (lines_added :returns Integer :keyword t)
     (lines_removed :returns Integer :keyword t)
     (path :returns RepoPath :keyword t)
     (display_diff_path :returns String :keyword t)
     (status :returns String :keyword t)
     (status_char :returns String :keyword t))
    (DiffStats
     (files :returns (:list DiffStatEntry) :keyword t)
     (total_added :returns Integer :keyword t)
     (total_removed :returns Integer :keyword t))
    (Email
     (local :returns String :keyword t)
     (domain :returns String :keyword t))
    (List
     (len :returns Integer :keyword t)
     (join :args ((separator Template)) :returns Template)
     (filter :args ((predicate (:lambda (Any) Boolean) :specialize :element-lambda))
             :returns :self)
     (map :args ((mapper (:lambda (Any) Any) :specialize :element-lambda))
          :returns (:list Template)
          :returns-via (:list-of-lambda-return mapper))
     (any :args ((predicate (:lambda (Any) Boolean) :specialize :element-lambda))
          :returns Boolean)
     (all :args ((predicate (:lambda (Any) Boolean) :specialize :element-lambda))
          :returns Boolean)
     (first :returns :element :keyword t)
     (last :returns :element :keyword t)
     (get :args ((index Integer)) :returns :element)
     (reverse :returns :self :keyword t)
     (skip :args ((count Integer)) :returns :self)
     (take :args ((count Integer)) :returns :self))
    ((:list Trailer)
     (contains_key :args ((key Stringify)) :returns Boolean))
    (Operation
     (current_operation :returns Boolean :keyword t)
     (description :returns String :keyword t)
     (id :returns OperationId :keyword t)
     (tags :returns String :keyword t)
     (time :returns TimestampRange :keyword t)
     (user :returns String :keyword t)
     (snapshot :returns Boolean :keyword t)
     (workspace_name :returns String :keyword t)
     (root :returns Boolean :keyword t)
     (parents :returns (:list Operation) :keyword t))
    (OperationId
     (short :args ((len Integer :optional t)) :returns String))
    (RepoPath
     (absolute :returns String :keyword t)
     (display :returns String :keyword t)
     (parent :returns (:option RepoPath) :keyword t))
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
     (upper :returns (:option Integer) :keyword t)
     (exact :returns (:option Integer) :keyword t)
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
     (lines :returns (:list String) :keyword t)
     (upper :returns String :keyword t)
     (lower :returns String :keyword t)
     (starts_with :args ((needle Stringify)) :returns Boolean)
     (ends_with :args ((needle Stringify)) :returns Boolean)
     (remove_prefix :args ((needle Stringify)) :returns String)
     (remove_suffix :args ((needle Stringify)) :returns String)
     (trim :returns String :keyword t)
     (trim_start :returns String :keyword t)
     (trim_end :returns String :keyword t)
     (split :args ((separator StringPattern)
                   (limit Integer :optional t))
            :returns (:list String))
     (substr :args ((start Integer) (end Integer :optional t)) :returns String)
     (escape_json :returns String :keyword t))
    (Timestamp
     (ago :returns String :keyword t)
     (format :args ((format StringLiteral)) :returns String)
     (utc :returns Timestamp :keyword t)
     (local :returns Timestamp :keyword t)
     (after :args ((date StringLiteral)) :returns Boolean)
     (before :args ((date StringLiteral)) :returns Boolean)
     (since :args ((start Timestamp)) :returns TimestampRange))
    (TimestampRange
     (start :returns Timestamp :keyword t)
     (end :returns Timestamp :keyword t)
     (duration :returns String :keyword t))
    (Trailer
     (key :returns String :keyword t)
     (value :returns String :keyword t))
    (TreeDiff
     (files :returns (:list TreeDiffEntry) :keyword t)
     (color_words :args ((context Integer :optional t)) :returns Template)
     (git :args ((context Integer :optional t)) :returns Template)
     (stat :args ((width Integer :optional t)) :returns DiffStats)
     (summary :returns Template :keyword t))
    (TreeDiffEntry
     (path :returns RepoPath :keyword t)
     (display_diff_path :returns String :keyword t)
     (status :returns String :keyword t)
     (status_char :returns String :keyword t)
     (source :returns TreeEntry :keyword t)
     (target :returns TreeEntry :keyword t))
    (TreeEntry
     (path :returns RepoPath :keyword t)
     (conflict :returns Boolean :keyword t)
     (conflict_side_count :returns Integer :keyword t)
     (file_type :returns String :keyword t)
     (executable :returns Boolean :keyword t))
    (WorkspaceRef
     (name :returns RefSymbol :keyword t)
     (target :returns Commit :keyword t)
     (root :returns Template :keyword t)))
  "Built-in template method metadata.")

(majutsu-template--register-methods majutsu-template--builtin-method-specs)

;;;; AST representation

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
  "Return raw template node for verbatim TEXT.
Untyped raw snippets default to `Unknown' so semantic typing only becomes
precise when callers provide an explicit annotation or later inference refines
it."
  (majutsu-template-node-create
   :kind :raw
   :type (or type 'Unknown)
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

(defun majutsu-template--var-node (name &optional type props)
  "Return lexical variable node for NAME."
  (majutsu-template-node-create
   :kind :var
   :type (or type 'Unknown)
   :value name
   :props props))

(defun majutsu-template--lambda-node (params body &optional props type)
  "Return lambda node binding PARAMS around BODY.
PARAMS is a list of symbols. BODY must already be a template node.
TYPE defaults to `Lambda' and may be a richer internal lambda type ref."
  (majutsu-template-node-create
   :kind :lambda
   :type (or type 'Lambda)
   :value params
   :args (list body)
   :props props))

(defun majutsu-template--lambda-params (node)
  "Return parameter list stored in lambda NODE."
  (unless (and (majutsu-template-node-p node)
               (eq (majutsu-template-node-kind node) :lambda))
    (user-error "majutsu-template: expected lambda node, got %S" node))
  (majutsu-template-node-value node))

(defun majutsu-template--lambda-body (node)
  "Return body node stored in lambda NODE."
  (unless (and (majutsu-template-node-p node)
               (eq (majutsu-template-node-kind node) :lambda))
    (user-error "majutsu-template: expected lambda node, got %S" node))
  (car (majutsu-template-node-args node)))

(defun majutsu-template--expand-elisp (form)
  "Expand embedded Elisp expressions inside FORM when allowed."
  (cond
   ((majutsu-template-node-p form) form)
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

(defun majutsu-template--parse-lambda-params (params)
  "Parse PARAMS form into a validated list of lambda parameter symbols."
  (let ((items (cond
                ((vectorp params) (append params nil))
                ((listp params) params)
                (t (user-error "majutsu-template: lambda parameters must be a vector or list, got %S"
                               params)))))
    (unless (= (length items) 1)
      (user-error "majutsu-template: only single-argument lambdas are supported, got %S"
                  params))
    (dolist (item items)
      (unless (and (symbolp item) (not (keywordp item)))
        (user-error "majutsu-template: invalid lambda parameter %S" item)))
    items))

(defun majutsu-template--lambda-from-form (params body &optional param-type)
  "Return lambda node from PARAMS and BODY forms.
When PARAM-TYPE is non-nil, bind the single lambda parameter to that type."
  (let* ((parsed-params (majutsu-template--parse-lambda-params params))
         (self-param (car parsed-params))
         (param-type (majutsu-template--type-ref-normalize param-type))
         (bound-type (unless (memq (majutsu-template--type-ref-base-type param-type)
                                   '(Unknown Template))
                       param-type))
         (self-node (and self-param
                         (majutsu-template--var-node
                          self-param bound-type
                          (unless bound-type '(:deferred-type t)))))
         (bindings (mapcar (lambda (param)
                             (majutsu-template--make-binding
                              :name param
                              :type (and (eq param self-param) bound-type)
                              :node (when (eq param self-param) self-node)))
                           parsed-params))
         (body-node (majutsu-template--call-with-bindings
                     bindings
                     (lambda ()
                       (majutsu-template--call-with-self-binding
                        self-node bound-type
                        (lambda ()
                          (majutsu-template--sugar-transform body))))))
         (lambda-type (list :lambda
                            (list (or bound-type 'Unknown))
                            (or (majutsu-template-node-type body-node) 'Unknown))))
    (majutsu-template--lambda-node parsed-params body-node
                                   (list :body-form body)
                                   lambda-type)))

(defun majutsu-template--lambda-sugar-params (op)
  "Return lambda parameter list encoded by keyword OP like `:|x|'.
Return nil when OP is not lambda shorthand syntax."
  (when (keywordp op)
    (let ((name (symbol-name op)))
      (when (and (string-prefix-p ":|" name)
                 (string-suffix-p "|" name))
        (let ((items (split-string (string-trim (substring name 2 -1))
                                   "[[:space:]]+" t)))
          (unless items
            (user-error "majutsu-template: lambda shorthand %S requires at least one parameter" op))
          (mapcar #'intern items))))))

(majutsu-template-defspecial :lambda (params body)
                             (majutsu-template--lambda-from-form params body))

(majutsu-template-defspecial :--map (body collection)
                             `[:-map [:|it| ,body] ,collection])

(majutsu-template-defspecial :--filter (body collection)
                             `[:-filter [:|it| ,body] ,collection])

(majutsu-template-defspecial :--any (body collection)
                             `[:-any [:|it| ,body] ,collection])

(majutsu-template-defspecial :--all-p (body collection)
                             `[:-all-p [:|it| ,body] ,collection])

(majutsu-template-defspecial :map (collection var body)
                             (majutsu-template--higher-order-form "map" collection var body))

(majutsu-template-defspecial :filter (collection var body)
                             (majutsu-template--higher-order-form "filter" collection var body))

(majutsu-template-defspecial :any (collection var body)
                             (majutsu-template--higher-order-form "any" collection var body))

(majutsu-template-defspecial :all (collection var body)
                             (majutsu-template--higher-order-form "all" collection var body))

(majutsu-template-defspecial :map-join (separator collection var body)
                             `[:method [:map ,collection ,var ,body] :join ,separator])

(defun majutsu-template--expand-lambda-shorthand-special (op args)
  "Expand lambda shorthand operator OP with raw ARGS.
Return `majutsu-template--no-special-expansion' when OP is not shorthand."
  (if-let* ((params (majutsu-template--lambda-sugar-params op)))
      (pcase args
        (`(,body)
         (majutsu-template--lambda-from-form params body))
        (_
         (user-error "majutsu-template: %S expects BODY, got %S" op args)))
    majutsu-template--no-special-expansion))

(eval-and-compile
  (majutsu-template-define-special-resolver
   #'majutsu-template--expand-lambda-shorthand-special))

(defun majutsu-template--ensure-lambda-node (value &optional context)
  "Return VALUE normalized as a lambda node.
CONTEXT is used in error messages."
  (let ((node (majutsu-template--rewrite value)))
    (if (eq (majutsu-template-node-kind node) :lambda)
        node
      (user-error "majutsu-template: expected lambda%s, got %S"
                  (if context (format " for %s" context) "")
                  value))))

(defun majutsu-template--substitute-var (node name replacement)
  "Replace lexical variable NAME in NODE with REPLACEMENT node."
  (pcase (majutsu-template-node-kind node)
    (:var
     (if (eq (majutsu-template-node-value node) name)
         replacement
       node))
    (:lambda
     (if (memq name (majutsu-template--lambda-params node))
         node
       (majutsu-template--lambda-node
        (majutsu-template--lambda-params node)
        (majutsu-template--substitute-var
         (majutsu-template--lambda-body node)
         name replacement)
        (majutsu-template-node-props node))))
    (:call
     (majutsu-template-node-create
      :kind :call
      :type (majutsu-template-node-type node)
      :value (majutsu-template-node-value node)
      :args (mapcar (lambda (arg)
                      (majutsu-template--substitute-var arg name replacement))
                    (majutsu-template-node-args node))
      :props (majutsu-template-node-props node)))
    (_ node)))

(defun majutsu-template--specialize-lambda-node (lambda-node arg-type)
  "Return LAMBDA-NODE specialized for a single parameter of ARG-TYPE."
  (let ((params (majutsu-template--lambda-params lambda-node)))
    (unless (= (length params) 1)
      (user-error "majutsu-template: only single-argument lambdas can be specialized"))
    (if-let* ((body-form (plist-get (majutsu-template-node-props lambda-node) :body-form)))
        (majutsu-template--lambda-from-form params body-form arg-type)
      lambda-node)))

(defun majutsu-template--apply-lambda (lambda-node arg-nodes)
  "Apply LAMBDA-NODE to ARG-NODES by compile-time substitution."
  (let ((params (majutsu-template--lambda-params lambda-node)))
    (unless (= (length params) (length arg-nodes))
      (user-error "majutsu-template: lambda expects %d arguments, got %d"
                  (length params) (length arg-nodes)))
    (when-let* ((expected-types (majutsu-template--type-ref-lambda-arg-types
                                 (majutsu-template-node-type lambda-node))))
      (cl-loop for expected in expected-types
               for arg in arg-nodes
               do (unless (majutsu-template--type-convertible-p
                           (majutsu-template-node-type arg) expected)
                    (user-error "majutsu-template: lambda expects %S, got %S"
                                expected (majutsu-template-node-type arg)))))
    (if-let* ((body-form (plist-get (majutsu-template-node-props lambda-node) :body-form)))
        (let* ((arg-type (and (= (length arg-nodes) 1)
                              (majutsu-template-node-type (car arg-nodes))))
               (specialized (and arg-type
                                 (majutsu-template--specialize-lambda-node lambda-node arg-type)))
               (specialized-body-form (and specialized
                                           (plist-get (majutsu-template-node-props specialized) :body-form))))
          (if specialized-body-form
              (let* ((self-type (car (or (majutsu-template--type-ref-lambda-arg-types
                                          (majutsu-template-node-type specialized))
                                         '(Unknown))))
                     (self-type (unless (memq (majutsu-template--type-ref-base-type self-type)
                                              '(Unknown Template))
                                  self-type))
                     (bindings (cl-mapcar (lambda (param arg)
                                            (majutsu-template--make-binding
                                             :name param
                                             :type (majutsu-template-node-type arg)
                                             :node arg))
                                          params arg-nodes)))
                (majutsu-template--call-with-bindings
                 bindings
                 (lambda ()
                   (majutsu-template--call-with-self-binding
                    (car arg-nodes)
                    self-type
                    (lambda ()
                      (majutsu-template--sugar-transform specialized-body-form))))))
            (cl-loop with result = (majutsu-template--lambda-body lambda-node)
                     for param in params
                     for arg in arg-nodes
                     do (setq result (majutsu-template--substitute-var result param arg))
                     finally return result)))
      (cl-loop with result = (majutsu-template--lambda-body lambda-node)
               for param in params
               for arg in arg-nodes
               do (setq result (majutsu-template--substitute-var result param arg))
               finally return result))))

(defun majutsu-template--rewrite (form)
  "Rewrite literal FORM into normalized AST nodes.
Further passes (type-checking, rendering) operate on these nodes."
  (let ((expanded (if majutsu-template--allow-eval
                      (majutsu-template--expand-elisp form)
                    form)))
    (majutsu-template--sugar-transform expanded)))

(majutsu-template-defun concat ((forms Template :rest t))
  (:returns Template :doc "concat(FORMS...)." :flavor :builtin))

(majutsu-template-defun if ((condition Template)
                            (then Template)
                            (else Template :optional t))
  (:returns Template :doc "if(COND, THEN [, ELSE])." :flavor :builtin))

(majutsu-template-defun separate ((separator Template)
                                  (forms Template :rest t))
  (:returns Template :doc "separate(SEP, FORMS...)." :flavor :builtin))

(majutsu-template-defun surround ((pre Template)
                                  (post Template)
                                  (body Template))
  (:returns Template :doc "surround(PRE, POST, BODY)." :flavor :builtin))

(majutsu-template-defun label ((label Template)
                               (content Template))
  (:returns Template :doc "label helper." :flavor :builtin))

(majutsu-template-defun json ((value Any))
  (:returns String :doc "json(VALUE)." :flavor :builtin))

(majutsu-template-defun join ((separator Template)
                              (content Template :rest t))
  (:returns Template :doc "join(SEP, CONTENT...)." :flavor :builtin))

;; str/raw are defined manually to avoid recursion or for custom logic.
;; We register their metadata here.

(majutsu-template--register-function
 (majutsu-template--make-fn
  :name "str" :symbol 'majutsu-template-str
  :args (list (majutsu-template--make-arg :name 'value :type 'Any))
  :returns 'Template :doc "String literal helper." :flavor :custom))

(majutsu-template--register-function
 (majutsu-template--make-fn
  :name "raw" :symbol 'majutsu-template-raw
  :args (list (majutsu-template--make-arg :name 'value :type 'Any)
              (majutsu-template--make-arg :name 'type :type 'Any :optional t))
  :returns 'Template :doc "Raw literal helper." :flavor :custom))

(defun majutsu-template--higher-order-form (method collection var body)
  "Return COLLECTION.METHOD(|VAR| BODY) as a template form."
  (let ((var-name (intern (majutsu-template--node->identifier var method))))
    `[:method ,collection ,(intern (concat ":" method)) [:lambda [,var-name] ,body]]))

(majutsu-template-defun -map ((function Lambda)
                              (collection Any))
  (:returns (:list Template) :doc "Dash-style map taking an explicit lambda." :flavor :custom)
  `[:method ,collection :map ,function])

(majutsu-template-defun -filter ((function Lambda)
                                 (collection Any))
  (:returns (:list Template) :doc "Dash-style filter taking an explicit lambda." :flavor :custom)
  `[:method ,collection :filter ,function])

(majutsu-template-defun -any ((function Lambda)
                              (collection Any))
  (:returns Boolean :doc "Dash-style any taking an explicit lambda." :flavor :custom)
  `[:method ,collection :any ,function])

(majutsu-template-defun -all-p ((function Lambda)
                                (collection Any))
  (:returns Boolean :doc "Dash-style all taking an explicit lambda." :flavor :custom)
  `[:method ,collection :all ,function])

(majutsu-template-defun call ((name Any)
                              (args Any :rest t))
  (:returns Template :doc "funcall helper, with builtin function support.")
  (let* ((name-node (majutsu-template--rewrite name))
         (arg-nodes (mapcar #'majutsu-template--rewrite args)))
    (if (eq (majutsu-template-node-kind name-node) :lambda)
        (majutsu-template--apply-lambda name-node arg-nodes)
      (let* ((identifier (majutsu-template--node->identifier name-node "call name"))
             (meta (majutsu-template--lookup-function-meta identifier)))
        (cond
         ((and meta (eq (majutsu-template--fn-flavor meta) :builtin))
          (majutsu-template--call-node (majutsu-template--fn-name meta)
                                       arg-nodes
                                       (majutsu-template--fn-returns meta)))
         (meta
          (apply (majutsu-template--fn-symbol meta) arg-nodes))
         (t
          (majutsu-template--call-node
           (majutsu-template--normalize-call-name identifier)
           arg-nodes)))))))

(defun majutsu-template--method-node-name (node)
  "Return normalized method name string from NODE."
  (let* ((raw (majutsu-template--node->identifier node "method name")))
    (if (and (> (length raw) 0) (eq (aref raw 0) ?:))
        (substring raw 1)
      raw)))

(defun majutsu-template--method-name-node-p (node)
  "Return non-nil if NODE denotes a chained method name marker."
  (and (majutsu-template-node-p node)
       (memq (majutsu-template-node-kind node) '(:raw :literal))
       (let ((value (majutsu-template-node-value node)))
         (and (stringp value)
              (> (length value) 0)
              (eq (aref value 0) ?:)))))

(defun majutsu-template--method-name-from-node (node)
  "Return normalized method name string from NODE."
  (let ((raw (majutsu-template--node->identifier node "method name")))
    (if (and (> (length raw) 0) (eq (aref raw 0) ?:))
        (substring raw 1)
      raw)))

(defun majutsu-template--method-split-segments (initial-name nodes)
  "Split method chain into segments starting with INITIAL-NAME (string).
NODES are the remaining argument nodes. Returns list of (NAME . ARGS)."
  (let ((segments '())
        (current-name initial-name)
        (current-args '()))
    (dolist (node nodes)
      (if (majutsu-template--method-name-node-p node)
          (progn
            (push (cons current-name (nreverse current-args)) segments)
            (setq current-name (majutsu-template--method-name-from-node node)
                  current-args '()))
        (push node current-args)))
    (push (cons current-name (nreverse current-args)) segments)
    (nreverse segments)))

(defun majutsu-template--method-dispatch-info (receiver-type method-name)
  "Return `(DISPATCH-TYPE . META)' for METHOD-NAME on RECEIVER-TYPE."
  (majutsu-template--lookup-method-dispatch receiver-type method-name))

(defun majutsu-template--lookup-method-for-type (receiver-type method-name)
  "Return metadata for METHOD-NAME on RECEIVER-TYPE, or nil."
  (cdr-safe (majutsu-template--method-dispatch-info receiver-type method-name)))

(defun majutsu-template--specialize-method-arg (receiver-type arg-spec arg)
  "Return ARG prepared according to ARG-SPEC in context of RECEIVER-TYPE."
  (pcase (and arg-spec (majutsu-template--arg-specialize arg-spec))
    (:element-lambda
     (if (and (eq (majutsu-template-node-kind arg) :lambda)
              (majutsu-template--type-ref-element-type receiver-type))
         (majutsu-template--specialize-lambda-node
          arg (majutsu-template--type-ref-element-type receiver-type))
       arg))
    (_ arg)))

(defun majutsu-template--specialize-method-arg-type (receiver-type arg-spec)
  "Return expected argument type for ARG-SPEC specialized to RECEIVER-TYPE."
  (let ((arg-type (majutsu-template--arg-type arg-spec)))
    (pcase (and arg-spec (majutsu-template--arg-specialize arg-spec))
      (:element-lambda
       (if-let* ((element-type (majutsu-template--type-ref-element-type receiver-type))
                 (lambda-args (majutsu-template--type-ref-lambda-arg-types arg-type)))
           (list :lambda
                 (cons element-type (cdr lambda-args))
                 (or (majutsu-template--type-ref-lambda-return-type arg-type)
                     'Unknown))
         arg-type))
      (_ arg-type))))

(defun majutsu-template--check-method-segment-args (receiver-type method-name args)
  "Validate ARGS for METHOD-NAME on RECEIVER-TYPE and return prepared args."
  (let* ((dispatch (majutsu-template--method-dispatch-info receiver-type method-name))
         (dispatch-type (or (car-safe dispatch) receiver-type))
         (meta (cdr-safe dispatch))
         (specs (and meta (cdr (majutsu-template--fn-args meta))))
         (prepared (cl-loop for arg in args
                            for index from 0
                            for spec = (nth index specs)
                            collect (majutsu-template--specialize-method-arg
                                     dispatch-type spec arg))))
    (when meta
      (cl-loop for arg in prepared
               for spec in specs
               for expected = (majutsu-template--specialize-method-arg-type
                               dispatch-type spec)
               do (majutsu-template--assert-node-type
                   method-name
                   (majutsu-template--arg-name spec)
                   expected
                   arg)))
    prepared))

(defun majutsu-template--resolve-method-return-via (meta args fallback)
  "Resolve META's `:returns-via' against ARGS, defaulting to FALLBACK."
  (pcase (majutsu-template--fn-returns-via meta)
    (`(:list-of-lambda-return ,arg-name)
     (let* ((specs (cdr (majutsu-template--fn-args meta)))
            (index (cl-position arg-name specs
                                :key #'majutsu-template--arg-name
                                :test #'eq))
            (arg-node (and index (nth index args)))
            (mapped-type (and arg-node
                              (majutsu-template--type-ref-lambda-return-type
                               (majutsu-template-node-type arg-node)))))
       (list :list (or mapped-type 'Template))))
    (_ fallback)))

(defun majutsu-template--method-segment-result-type (receiver-type method-name args)
  "Infer result type of calling METHOD-NAME on RECEIVER-TYPE with ARGS.
Return nil if the result type is unknown."
  (when-let* ((dispatch (majutsu-template--method-dispatch-info receiver-type method-name))
              (dispatch-type (car dispatch))
              (meta (cdr dispatch)))
    (majutsu-template--resolve-method-return-via
     meta args
     (majutsu-template--resolve-return-type-ref
      dispatch-type
      (majutsu-template--fn-returns meta)))))

(majutsu-template-defun method ((object Any)
                                (name Any)
                                (args Any :rest t))
  (:returns Template :doc "Method chaining helper." :flavor :custom)
  (let* ((object-node (majutsu-template--rewrite object))
         (object-str (majutsu-template--render-node object-node))
         (name-node (majutsu-template--rewrite name))
         (start-name (majutsu-template--method-name-from-node name-node))
         (arg-nodes (mapcar #'majutsu-template--rewrite args))
         (segments (majutsu-template--method-split-segments start-name arg-nodes))
         (result object-str)
         (current-type (majutsu-template-node-type object-node)))
    (dolist (segment segments)
      (let* ((seg-name (car segment))
             (seg-args (majutsu-template--check-method-segment-args
                        current-type seg-name (cdr segment)))
             (arg-str (mapconcat #'majutsu-template--render-node seg-args ", ")))
        (setq result (if (= (length arg-str) 0)
                         (format "%s.%s()" result seg-name)
                       (format "%s.%s(%s)" result seg-name arg-str)))
        (setq current-type (majutsu-template--method-segment-result-type
                            current-type seg-name seg-args))))
    (majutsu-template--raw-node result current-type)))

(majutsu-template--defpassthrough coalesce "coalesce helper.")
(majutsu-template--defpassthrough fill "fill helper.")
(majutsu-template--defpassthrough indent "indent helper.")
(majutsu-template--defpassthrough pad_start "pad_start helper.")
(majutsu-template--defpassthrough pad_end "pad_end helper.")
(majutsu-template--defpassthrough pad_centered "pad_centered helper.")
(majutsu-template--defpassthrough truncate_start "truncate_start helper.")
(majutsu-template--defpassthrough truncate_end "truncate_end helper.")
(majutsu-template--defpassthrough hash "hash helper.")
(majutsu-template--defpassthrough stringify "stringify helper.")
(majutsu-template--defpassthrough raw_escape_sequence "raw_escape_sequence helper.")
(majutsu-template--defpassthrough config "config helper.")
(majutsu-template--defpassthrough hyperlink "hyperlink helper.")
(majutsu-template--defpassthrough git_web_url "git_web_url helper.")
;; Internal node representation: (:tag ...)

(defun majutsu-template--str-escape (s)
  "Escape S into a jj double-quoted string literal content.

Supported escapes (aligned with jj docs):
  \\\" \\\\ \\t \\r \\n \\0 \\e and generic control bytes as \\xHH.
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
  (let ((text (if (majutsu-template-node-p s)
                  (majutsu-template--literal-string-from-node s)
                s)))
    (majutsu-template--literal-node text 'String)))

(defun majutsu-template-raw (value &optional type)
  "Raw snippet injected verbatim. Use sparingly.
VALUE may be a string or template node. TYPE is an optional semantic
annotation; otherwise the raw snippet remains `Unknown'."
  (let* ((string (if (majutsu-template-node-p value)
                     (majutsu-template--literal-string-from-node value)
                   (format "%s" value)))
         (declared (when type (majutsu-template--node->type-symbol type)))
         (props (when declared (list :declared declared))))
    (majutsu-template--raw-node string declared props)))

(defun majutsu-template--emit-call (name &rest args)
  "Low-level helper: build call node NAME with normalized ARGS.
NAME may be symbol, keyword, string, or template node."
  (let* ((identifier (majutsu-template--node->identifier name "call name"))
         (resolved (majutsu-template--normalize-call-name identifier))
         (arg-nodes (mapcar #'majutsu-template--rewrite args)))
    (majutsu-template--call-node resolved arg-nodes)))

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

(defun majutsu-template--node->identifier (node &optional context)
  "Extract identifier-like string from NODE.
CONTEXT is used in error messages."
  (if (majutsu-template-node-p node)
      (pcase (majutsu-template-node-kind node)
        (:raw (majutsu-template-node-value node))
        (:literal (majutsu-template-node-value node))
        (_ (user-error "majutsu-template: expected identifier%s, got %S"
                       (if context (format " for %s" context) "")
                       node)))
    (majutsu-template--node->identifier (majutsu-template--rewrite node) context)))
(defun majutsu-template--node->type-symbol (node)
  "Return normalized type symbol described by NODE."
  (majutsu-template--normalize-type-symbol
   (majutsu-template--node->identifier node "type")))

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

(defun majutsu-template--render-node (node)
  "Render AST NODE to jj template string."
  (pcase (majutsu-template-node-kind node)
    (:literal
     (format "\"%s\"" (majutsu-template--str-escape (majutsu-template-node-value node))))
    (:raw
     (majutsu-template-node-value node))
    (:var
     (symbol-name (majutsu-template-node-value node)))
    (:lambda
     (let* ((params (majutsu-template--lambda-params node))
            (body (majutsu-template--lambda-body node)))
       (format "|%s| %s"
               (mapconcat #'symbol-name params ", " )
               (majutsu-template--render-node body))))
    (:call
     (let* ((name (majutsu-template-node-value node))
            (args (majutsu-template-node-args node))
            (compiled-args (mapconcat #'majutsu-template--render-node args ", ")))
       (format "%s(%s)" name compiled-args)))
    (_
     (user-error "majutsu-template: unknown AST node kind %S" (majutsu-template-node-kind node)))))

;;;###autoload
(defun majutsu-template-compile (form &optional self-type)
  "Public entry point: compile FORM into a jj template string.
Optional SELF-TYPE overrides `majutsu-template-default-self-type' for the root
implicit self context installed during this compilation."
  (let* ((effective-default (or self-type majutsu-template-default-self-type))
         (majutsu-template-default-self-type
          (and effective-default
               (majutsu-template--normalize-type-symbol effective-default))))
    (majutsu-template--call-with-root-self-binding
     nil
     (lambda ()
       (majutsu-template--render-node (majutsu-template--rewrite form))))))
(defun majutsu-template--sugar-transform (form)
  "Transform compact FORM into the template AST."
  (cond
   ((majutsu-template-node-p form) form)
   ((and (consp form) (keywordp (car form)))
    (majutsu-template--sugar-apply (car form) (cdr form)))
   ((numberp form) (majutsu-template-raw (number-to-string form) 'Integer))
   ((eq form t) (majutsu-template-raw "true" 'Boolean))
   ((eq form nil) (majutsu-template-raw "false" 'Boolean))
   ((stringp form) (majutsu-template-str form))
   ((symbolp form)
    (if-let* ((binding (majutsu-template--lookup-binding form)))
        (or (majutsu-template--binding-node binding)
            (majutsu-template--var-node form (majutsu-template--binding-type binding)))
      (majutsu-template-raw (symbol-name form))))
   ((vectorp form)
    (let* ((list-form (append form nil)))
      (cond
       ((null list-form)
        (majutsu-template--sugar-transform '(:concat)))
       ((symbolp (car list-form))
        (majutsu-template--sugar-transform list-form))
       ((cdr list-form)
        (let ((head (majutsu-template--sugar-transform (car list-form))))
          (if (eq (majutsu-template-node-kind head) :lambda)
              (majutsu-template--sugar-transform (cons :call (cons head (cdr list-form))))
            (majutsu-template--sugar-transform (cons :concat list-form)))))
       (t
        (majutsu-template--sugar-transform (cons :concat list-form))))))
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

(defun majutsu-template--maybe-self-dispatch (op args)
  "Return method node if OP is a self keyword applicable in current context."
  (let ((self-binding (majutsu-template--current-self)))
    (when (and self-binding (or (symbolp op) (stringp op)))
      (let* ((object (majutsu-template--self-binding-node self-binding))
             (owner (majutsu-template--self-binding-type self-binding)))
        (when object
          (let* ((name (if (symbolp op)
                           (majutsu-template--symbol->template-name op)
                         op))
                 (meta (and owner
                            (majutsu-template--lookup-method-for-type owner name)))
                 (normalized-args (mapcar #'majutsu-template--rewrite args)))
            (cond
             ((and meta (not (majutsu-template--fn-keyword meta)))
              nil)
             ((and meta
                   normalized-args
                   (not (majutsu-template--method-name-node-p (car normalized-args))))
              (user-error "majutsu-template: keyword %s on %S does not accept arguments"
                          name owner))
             (meta
              (let ((name-node (majutsu-template--rewrite op)))
                (apply #'majutsu-template-method object name-node normalized-args)))
             ((and (null owner) (keywordp op))
              (let ((name-node (majutsu-template--rewrite op)))
                (apply #'majutsu-template-method object name-node normalized-args))))))))))

(defun majutsu-template--sugar-apply (op args)
  "Dispatch helper applying OP to ARGS within sugar transformation."
  (let ((special (majutsu-template--expand-special op args)))
    (if (not (eq special majutsu-template--no-special-expansion))
        (majutsu-template--rewrite special)
      (let* ((normalized (majutsu-template--operator-symbol op))
             (meta (or (majutsu-template--lookup-function-meta op)
                       (majutsu-template--lookup-function-meta normalized)))
             (self-node (majutsu-template--maybe-self-dispatch op args)))
        (cond
         (meta
          (apply (majutsu-template--fn-symbol meta)
                 (mapcar #'majutsu-template--sugar-transform args)))
         (self-node self-node)
         (t
          (user-error "majutsu-template: unknown operator %S" op)))))))

;;;###autoload
(defmacro majutsu-tpl (form &optional self-type)
  "Expand and compile FORM to a jj template string.
Vector literals are compiled at macro-expansion time IF SELF-TYPE is provided
as a constant.

If SELF-TYPE is not provided, compilation is deferred to runtime to respect
dynamic bindings of `majutsu-template-default-self-type'."
  (cond
   ((and (vectorp form) (or (keywordp self-type) (and (consp self-type) (eq (car self-type) 'quote))))
    (let ((majutsu-template-default-self-type (majutsu-template--normalize-type-symbol (eval self-type)))
          (majutsu-template--allow-eval t))
      (let ((node (majutsu-template--call-with-root-self-binding
                   nil
                   (lambda ()
                     (majutsu-template--rewrite form)))))
        `(majutsu-template-compile ',node))))
   ((vectorp form)
    `(let ((majutsu-template--allow-eval t))
       (majutsu-template-compile ,form ,self-type)))
   (t
    `(let ((majutsu-template--allow-eval t))
       (majutsu-template-compile ,form ,self-type)))))

;;; _
(provide 'majutsu-template)
;;; majutsu-template.el ends here
