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
  "Escape S for a jj double-quoted string literal."
  (when (not (stringp s))
    (user-error "majutsu-template: expected string, got %S" s))
  (let ((out (replace-regexp-in-string "\\\\" "\\\\\\\\" s))) ; backslash first
    (setq out (replace-regexp-in-string "\"" "\\\"" out))
    (setq out (replace-regexp-in-string "\n" "\\n" out))
    (setq out (replace-regexp-in-string "\t" "\\t" out))
    out))

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
    (:join . majutsu-template-join) (join . majutsu-template-join))
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
             (fn (alist-get op majutsu-template--sugar-ops)))
        (unless fn
          (user-error "majutsu-template: unknown operator %S" op))
        (pcase op
          ;; Explicit call: [:call name arg1 arg2]
          ((or 'call :call)
           (let* ((name (cadr form))
                  (name-str (if (symbolp name) (symbol-name name) name))
                  (args (mapcar #'majutsu-template--sugar-transform (cddr form))))
             (apply #'majutsu-template-call name-str args)))
          ;; Special cases for arity that mix raw values and forms
          ((or 'label :label)
           (let ((name (cadr form))
                 (value (caddr form)))
             (funcall fn name (majutsu-template--sugar-transform value))))
          ((or 'join :join)
           (let ((sep (cadr form)) (coll (caddr form)) (var (cadddr form)) (body (car (cddddr form))))
             (funcall fn (majutsu-template--sugar-transform sep)
                      (majutsu-template--sugar-transform coll)
                      var
                      (majutsu-template--sugar-transform body))))
          ;; Literal elisp value as jj string literal
          ((or 'lit :lit)
           (let ((val (cadr form)))
             (majutsu-template-str (format "%s" val))))
          ;; Raw elisp expression result injected verbatim
          ((or 'raw-e :raw-e)
           (let ((val (eval (cadr form))))
             (majutsu-template-raw (format "%s" val))))
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
          ((or 'method :method)
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
           (apply fn (mapcar #'majutsu-template--sugar-transform (cdr form)))))))
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
