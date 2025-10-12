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

(defun majutsu-template--lookup-function-name (sym)
  "Return registered template name string for SYM, or nil if unknown."
  (gethash sym majutsu-template--function-name-map))

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
  "Call a jj template function NAME with ARGS (forms or strings)."
  (list :call (format "%s" name) (cl-mapcar #'identity args)))

(defun majutsu-template-concat (&rest forms)
  "concat(FORMS...)."
  (apply #'majutsu-template-call 'concat forms))

(defun majutsu-template-if (cond then &optional else)
  "if(COND, THEN [, ELSE])."
  (if (null else)
      (majutsu-template-call 'if cond then)
    (majutsu-template-call 'if cond then else)))

(defun majutsu-template-label (name value)
  "label(NAME, VALUE). NAME is auto-quoted."
  (majutsu-template-call 'label (majutsu-template-str (format "%s" name)) value))

(defun majutsu-template-separate (sep &rest forms)
  "separate(SEP, FORMS...)."
  (apply #'majutsu-template-call 'separate sep forms))

(defun majutsu-template-surround (pre post body)
  "surround(PRE, POST, BODY)."
  (majutsu-template-call 'surround pre post body))

(defun majutsu-template-json (form)
  "json(FORM)."
  (majutsu-template-call 'json form))

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
  '((:str . majutsu-template-str) (str . majutsu-template-str)
    (:raw . majutsu-template-raw) (raw . majutsu-template-raw)
    (:call . majutsu-template-call) (call . majutsu-template-call)
    (:concat . majutsu-template-concat) (concat . majutsu-template-concat)
    (:if . majutsu-template-if) (if . majutsu-template-if)
    (:label . majutsu-template-label) (label . majutsu-template-label)
    (:separate . majutsu-template-separate) (separate . majutsu-template-separate)
    (:surround . majutsu-template-surround) (surround . majutsu-template-surround)
    (:json . majutsu-template-json) (json . majutsu-template-json)
    (:join . majutsu-template-join) (join . majutsu-template-join)
    (:method . majutsu-template-call) (method . majutsu-template-call)
    (:. . majutsu-template-call)
    (:map . majutsu-template-join) (map . majutsu-template-join)
    (:filter . majutsu-template-join) (filter . majutsu-template-join)
    (:any . majutsu-template-join) (any . majutsu-template-join)
    (:all . majutsu-template-join) (all . majutsu-template-join))
  "Operator mapping for the `tpl` macro sugar.")

(defun majutsu-template--sugar-transform (form)
  "Transform compact FORM (list or vector) into EDSL constructor calls."
  (let ((was-vector (vectorp form)))
    (when was-vector
      (setq form (append form nil))
      ;; If vector head is not an operator, treat as implicit concat
      (when (and (consp form)
                 (not (symbolp (car form))))
        (let* ((items (mapcar #'majutsu-template--sugar-transform form)))
          (setq form (apply #'majutsu-template-concat items)
                was-vector nil))))
    (cond
     ;; Allow pre-built AST nodes to pass through (:str/:raw/:call) with proper shape
     ((majutsu-template--ast-p form) form)
     ;; Numbers and booleans: inject as raw tokens (true/false, 123)
     ((numberp form) (majutsu-template-raw (number-to-string form)))
     ((eq form t) (majutsu-template-raw "true"))
     ((eq form nil) (majutsu-template-raw "false"))
     ((stringp form) (majutsu-template-str form))
     ;; Disallow list-based operators; require vectors like [:concat ...]
     ((and (consp form) (symbolp (car form)) (not was-vector))
      (user-error "majutsu-template: use vector syntax [:op ...], lists are not accepted: %S" form))
     ((and (consp form) (symbolp (car form)))
      (let* ((op (car form))
             (builtin (alist-get op majutsu-template--sugar-ops))
             (custom (majutsu-template--lookup-function-name op)))
        (cond
         (builtin
          (pcase op
            ;; Explicit call: [:call 'name arg1 arg2]
            ((or 'call :call)
            (let* ((raw-name (cadr form))
                   (name-str (majutsu-template--normalize-call-name raw-name))
                   (args (mapcar #'majutsu-template--sugar-transform (cddr form))))
              (apply #'majutsu-template-call name-str args)))
            ;; Special cases for arity that mix raw values and forms
            ((or 'label :label)
            (let ((name (cadr form))
                  (value (caddr form)))
              (funcall builtin name (majutsu-template--sugar-transform value))))
            ((or 'join :join)
            (let ((sep (cadr form)) (coll (caddr form)) (var (cadddr form)) (body (car (cddddr form))))
              (funcall builtin (majutsu-template--sugar-transform sep)
                       (majutsu-template--sugar-transform coll)
                       var
                       (majutsu-template--sugar-transform body))))
            ;; Map shorthand: [:map coll var body] -> coll.map(|var| body)
          ((or 'map :map)
            (let ((coll (cadr form)) (var (caddr form)) (body (cadddr form)))
              (let ((coll-s (majutsu-template--compile (majutsu-template--sugar-transform coll)))
                    (body-s (majutsu-template--compile (majutsu-template--sugar-transform body))))
                (majutsu-template-raw (format "%s.map(|%s| %s)" coll-s var body-s)))))
            ;; Filter/any/all shorthand
          ((or 'filter :filter)
            (let ((coll (cadr form)) (var (caddr form)) (pred (cadddr form)))
              (let ((coll-s (majutsu-template--compile (majutsu-template--sugar-transform coll)))
                    (pred-s (majutsu-template--compile (majutsu-template--sugar-transform pred))))
                (majutsu-template-raw (format "%s.filter(|%s| %s)" coll-s var pred-s)))))
          ((or 'any :any)
            (let ((coll (cadr form)) (var (caddr form)) (pred (cadddr form)))
              (let ((coll-s (majutsu-template--compile (majutsu-template--sugar-transform coll)))
                    (pred-s (majutsu-template--compile (majutsu-template--sugar-transform pred))))
                (majutsu-template-raw (format "%s.any(|%s| %s)" coll-s var pred-s)))))
          ((or 'all :all)
            (let ((coll (cadr form)) (var (caddr form)) (pred (cadddr form)))
              (let ((coll-s (majutsu-template--compile (majutsu-template--sugar-transform coll)))
                    (pred-s (majutsu-template--compile (majutsu-template--sugar-transform pred))))
                (majutsu-template-raw (format "%s.all(|%s| %s)" coll-s var pred-s)))))
            ;; Method sugar: [:method OBJ name arg1 arg2]
          ((or 'method :method ':.))
            (let* ((obj (cadr form))
                   (name (caddr form))
                   (args (cdddr form))
                   (obj-s (majutsu-template--compile (majutsu-template--sugar-transform obj)))
                   (arg-s (mapconcat (lambda (a)
                                       (majutsu-template--compile (majutsu-template--sugar-transform a)))
                                     args ", ")))
              (majutsu-template-raw (format "%s.%s(%s)" obj-s name arg-s))))
            ;; Global function sugar: coalesce/fill/indent/pad/truncate/hash/json/stringify/raw_escape_sequence/config
            ((or 'coalesce :coalesce 'fill :fill 'indent :indent 'pad_start :pad_start 'pad_end :pad_end
                'pad_centered :pad_centered 'truncate_start :truncate_start 'truncate_end :truncate_end
                'hash :hash 'stringify :stringify 'raw_escape_sequence :raw_escape_sequence 'config :config)
            (let* ((name (symbol-name op))
                   (args (mapcar #'majutsu-template--sugar-transform (cdr form))))
              (apply #'majutsu-template-call name args)))
            ;; Simple operators
            ((or :+ '+ :sub '- :* '* :/ '/ :% '% :>= '>= :> '> :<= '<= :< '< :== '== :!= '!= :and 'and :or 'or :not 'not :neg 'neg :concat-op 'concat-op)
            (majutsu-template--compile-op op form))
            (_
            (apply builtin (mapcar #'majutsu-template--sugar-transform (cdr form)))))
         (custom
          (let ((args (mapcar #'majutsu-template--sugar-transform (cdr form))))
            (apply #'majutsu-template-call custom args)))
         (t
          (user-error "majutsu-template: unknown operator %S" op)))))
     (t (user-error "majutsu-template: unsupported literal in sugar %S" form)))))

;;;###autoload
(defmacro tpl (form)
  "Expand a template program FORM (must be a vector literal) into the EDSL AST."
  (unless (vectorp form)
    (user-error "tpl: top-level form must be a vector, e.g., [:concat ...]"))
  (let ((node (majutsu-template--sugar-transform form)))
    `(quote ,node)))

;;;###autoload
(defmacro tpl-compile (form)
  "Expand and compile a template program FORM (must be a vector) to a jj template string."
  (unless (vectorp form)
    (user-error "tpl-compile: top-level form must be a vector, e.g., [:concat ...]"))
  (let ((node (majutsu-template--sugar-transform form)))
    `(majutsu-template-compile ',node)))

(provide 'majutsu-template)
