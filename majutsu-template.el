;;; majutsu-template.el -*- lexical-binding: t; -*-

;; A small Elisp EDSL to build jj template language strings.
;; v0 focuses on safe string construction and a few core combinators.

(require 'cl-lib)

(defgroup majutsu-template nil
  "Elisp wrapper for composing jj templates."
  :group 'majutsu)

(defcustom majutsu-template-compact-output t
  "If non-nil, compile without extra whitespace beyond commas and spaces."
  :type 'boolean
  :group 'majutsu-template)

(defvar majutsu-template--allow-eval nil
  "When non-nil, cons forms encountered during sugar transformation may be eval'd.")

;;;; Registry and metadata for custom template helpers

(cl-defstruct (majutsu-template--arg
               (:constructor majutsu-template--make-arg))
  name type optional rest converts doc)

(cl-defstruct (majutsu-template--fn
               (:constructor majutsu-template--make-fn))
  name symbol args returns return-converts doc)

(defvar majutsu-template--function-registry (make-hash-table :test #'equal)
  "Map template function names to `majutsu-template--fn' metadata.")

(defvar majutsu-template--function-name-map (make-hash-table :test #'eq)
  "Lookup table from symbols/keywords to template function names (strings).")

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
         (sym (intern name))
         (keyword (intern (concat ":" name)))
         (pre-existing (gethash name majutsu-template--function-registry)))
    (when (majutsu-template--reserved-name-p name)
      (user-error "majutsu-template: %s is reserved" name))
    (when pre-existing
      (message "majutsu-template: redefining template helper %s" name))
    (puthash name meta majutsu-template--function-registry)
    (puthash sym name majutsu-template--function-name-map)
    (puthash keyword name majutsu-template--function-name-map)
    fn-symbol))

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
      (cond
       ((majutsu-template--arg-rest arg)
        (setq rest (majutsu-template--arg-name arg)))
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
  "Parse SIGNATURE plist for FN-NAME, returning plist with :returns etc."
  (unless (and (consp signature) (eq (car signature) :returns) (cadr signature))
    (user-error "majutsu-template-defun %s: signature must start with (:returns TYPE ...)" fn-name))
  (let ((returns (cadr signature))
        (rest (cddr signature))
        (doc nil)
        (converts nil))
    (unless (symbolp returns)
      (user-error "majutsu-template-defun %s: invalid return type %S" fn-name returns))
    (while rest
      (let ((key (pop rest))
            (value (pop rest)))
        (pcase key
          (:converts (setq converts value))
          (:doc (setq doc value))
          (_ (user-error "majutsu-template-defun %s: unknown signature key %S" fn-name key)))))
    (list :returns returns :converts converts :doc doc)))

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
         (fn-symbol (intern (format "majutsu-template-%s" name-str)))
         (parsed-args (majutsu-template--parse-args name args))
         (lambda-list (majutsu-template--build-lambda-list parsed-args))
         (normalizers (majutsu-template--build-arg-normalizers parsed-args))
         (signature-info (majutsu-template--parse-signature name signature))
         (return-type (plist-get signature-info :returns))
         (return-converts (plist-get signature-info :converts))
         (docstring (majutsu-template--compose-docstring
                     name-str (plist-get signature-info :doc) parsed-args))
         (arg-metadata (mapcar #'majutsu-template--arg->metadata parsed-args))
         (meta `(majutsu-template--make-fn
                 :name ,name-str
                 :symbol ',fn-symbol
                 :args ',arg-metadata
                 :returns ',return-type
                 :return-converts ',return-converts
                 :doc ,docstring)))
    `(progn
       (majutsu-template--register-function ,meta)
       (defun ,fn-symbol ,lambda-list
         ,docstring
         ,@normalizers
         (let ((majutsu-template--result (progn ,@body)))
           (majutsu-template--normalize majutsu-template--result))))))

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

(defun majutsu-template--ast-p (x)
  "Return non-nil if X is a well-formed majutsu template AST node."
  (when (consp x)
    (pcase (car x)
      (:str (and (stringp (cadr x)) (null (cddr x))))
      (:raw (and (stringp (cadr x)) (null (cddr x))))
      (:call (and (stringp (cadr x)) (listp (caddr x)) (null (cdddr x))))
      (_ nil))))

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
  (list :str s))

(defun majutsu-template-raw (text)
  "Raw snippet injected verbatim. Use sparingly."
  (list :raw text))

(defun majutsu-template-call (name &rest args)
  "Call a jj template function NAME with ARGS (already normalized forms)."
  (let ((name-str (majutsu-template--normalize-call-name name)))
    (list :call name-str args)))

(defun majutsu-template--literal-string-from-node (node)
  "Return literal string content from NODE if it is :str or :raw."
  (pcase node
    (`(:str ,text) text)
    (`(:raw ,text) text)
    (_ (user-error "majutsu-template: expected literal string node, got %S" node))))

(defun majutsu-template--resolve-call-name (expr)
  "Resolve EXPR into a call name (string or symbol)."
  (cond
   ((or (stringp expr) (symbolp expr) (keywordp expr)) expr)
   ((majutsu-template--ast-p expr)
    (majutsu-template--literal-string-from-node expr))
   ((vectorp expr)
    (let ((node (majutsu-template--sugar-transform expr)))
      (majutsu-template--literal-string-from-node node)))
   ((and majutsu-template--allow-eval (consp expr))
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
  "Normalize X into a node. Strings become :str; nodes pass-through; raw left as-is."
  (cond
   ((majutsu-template--ast-p x) x)
   ((vectorp x) (majutsu-template--sugar-transform x))
   ((stringp x) (majutsu-template-str x))
   (t (user-error "majutsu-template: unsupported form %S" x))))

(defun majutsu-template--compile-list (xs)
  (mapconcat #'majutsu-template--compile xs ", "))

(defun majutsu-template--compile (node)
  "Compile NODE to jj template string."
  (let* ((n (majutsu-template--normalize node))
         (tag (car n)))
    (pcase tag
      (:str (format "\"%s\"" (majutsu-template--str-escape (plist-get n :str))))
      (:raw (format "%s" (plist-get n :raw)))
      (:call (let ((name (cadr n))
                   (args (caddr n)))
               (format "%s(%s)" name (majutsu-template--compile-list args))))
      (_ (user-error "majutsu-template: unknown node %S" tag)))))

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
   ((majutsu-template--ast-p form) form)
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
      (unless (= (length args) 1)
        (user-error "majutsu-template: :raw expects 1 argument"))
      (majutsu-template-raw (car args)))
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
    (let ((node (majutsu-template--sugar-transform form)))
      `(quote ,node))))

;;;###autoload
(defmacro tpl-compile (form)
  "Expand and compile FORM to a jj template string.
Vector literals are compiled at macro-expansion time. Non-vector forms are
evaluated at runtime and normalized via `majutsu-template--normalize'."
  (if (vectorp form)
      (let ((majutsu-template--allow-eval t))
        (let ((node (majutsu-template--sugar-transform form)))
          `(majutsu-template-compile ',node)))
    `(let ((majutsu-template--allow-eval nil))
       (majutsu-template-compile (majutsu-template--normalize ,form)))))

(provide 'majutsu-template)
