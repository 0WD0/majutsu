;;; majutsu-template-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'majutsu-template)

(defmacro mt--is (form expected)
  `(let ((got ,form))
     (should (stringp got))
     (should (equal got ,expected))))

(majutsu-template-defun test-helper ((label Template)
                                     (value Template :optional t))
  (:returns Template :doc "Small helper used in tests.")
  `[:concat ,label [:str ": "] ,(or value [:str ""])])

(ert-deftest test-majutsu-template-compile-basic ()
  (mt--is (tpl-compile [:concat [:str "Hello "] [:raw "self.author().name()"]])
          "concat(\"Hello \", self.author().name())")
  ;; Bare strings inside vector are treated as :str
  (mt--is (tpl-compile [:concat "A" "B"]) "concat(\"A\", \"B\")")
  ;; Vector not starting with operator defaults to concat
  (mt--is (tpl-compile ["A" "B"]) "concat(\"A\", \"B\")")
  (mt--is (tpl-compile ["A" [:raw "self.commit_id()"]])
          "concat(\"A\", self.commit_id())")
  (mt--is (tpl-compile [:if (:raw "self.root()") (:str "(root)") (:raw "format_short_commit_id(self.commit_id())")])
          "if(self.root(), \"(root)\", format_short_commit_id(self.commit_id()))")
  (mt--is (tpl-compile [:if t "A" "B"]) "if(true, \"A\", \"B\")")
  ;; Optional else
  (mt--is (tpl-compile [:if t "A"]) "if(true, \"A\")")
  (mt--is (tpl-compile [:separate [:str " "] [:label "immutable" [:str "immutable"]] [:label "conflict" [:str "conflict"]]])
          "separate(\" \", label(\"immutable\", \"immutable\"), label(\"conflict\", \"conflict\"))")
  (mt--is (tpl-compile [:raw (if t "ac" "wa")])
          "ac")
  )

(ert-deftest test-majutsu-template-compile-vector-required ()
  (should-error (macroexpand-1 '(tpl (concat (str "A") (str "B")))) :type 'error)
  (should-error (eval '(tpl-compile (concat (str "A") (str "B")))) :type 'error)
  ;; Nested list is also rejected
  (should-error (tpl-compile [:concat (str "A") [:str "B"]]) :type 'error))

(ert-deftest test-majutsu-template-compile-numbers-booleans ()
  (mt--is (tpl-compile [:call 'pad_end 8 [:str "abc"]])
          "pad_end(8, \"abc\")")
  (mt--is (tpl-compile [:if t [:str "yes"] [:str "no"]])
          "if(true, \"yes\", \"no\")")
  (mt--is (tpl-compile [:if nil [:str "yes"] [:str "no"]])
          "if(false, \"yes\", \"no\")"))

(ert-deftest test-majutsu-template-compile-map-join ()
  (mt--is (tpl-compile [:map [:raw "self.bookmarks()"] b [:raw "b.name()"]])
          "self.bookmarks().map(|b| b.name())")
  (mt--is (tpl-compile [:join [:str ", "] [:raw "self.bookmarks()"] b [:raw "b.name()"]])
          "self.bookmarks().map(|b| b.name()).join(\", \")"))

(ert-deftest test-majutsu-template-compile-filter-any-all ()
  (mt--is (tpl-compile [:filter [:raw "parents"] c [:raw "c.mine()"]])
          "parents.filter(|c| c.mine())")
  (mt--is (tpl-compile [:any [:raw "parents"] c [:raw "c.conflict()"]])
          "parents.any(|c| c.conflict())")
  (mt--is (tpl-compile [:all [:raw "parents"] c [:raw "c.mine()"]])
          "parents.all(|c| c.mine())"))

(ert-deftest test-majutsu-template-compile-method-and-call ()
  (mt--is (tpl-compile [:method (:raw "self") commit_id])
          "self.commit_id()")
  (mt--is (tpl-compile [:method (:raw "self") diff (:str "src")])
          "self.diff(\"src\")")
  (mt--is (tpl-compile [:call 'coalesce [:str ""] [:str "X"]])
          "coalesce(\"\", \"X\")")
  ;; :call with symbol name and bare string arg
  (mt--is (tpl-compile [:call 'json " "]) "json(\" \")")
  ;; :call with raw arg
  (mt--is (tpl-compile [:call 'json [:raw "test"]]) "json(test)")
  (mt--is (tpl-compile [:call 'a 'b]) "a(b)")
  (mt--is (tpl-compile [:call (if t 'a [:raw "hh"]) 'h]) "a(h)")
  (mt--is (tpl-compile [:call (if t [:raw (if t "hh" "wa")] 'bbb) 'x])
          "hh(x)")
  ;; dynamic decision in :call name
  (mt--is (tpl-compile [:call (if t 'json 'coalesce) [:str "ok"]])
          "json(\"ok\")")
  (mt--is (tpl-compile [:call (if nil 'json 'coalesce) [:str ""] [:str "x"]])
          "coalesce(\"\", \"x\")"))

(ert-deftest test-majutsu-template-compile-operators ()
  (mt--is (tpl-compile [:+ 1 2])
          "(1 + 2)")
  (mt--is (tpl-compile [:and (:> 3 1) (:<= 2 2)])
          "((3 > 1) && (2 <= 2))")
  (mt--is (tpl-compile [:concat-op (:str "a") (:str "b")])
          "(\"a\" ++ \"b\")")
  (mt--is (tpl-compile [:not t])
          "(!true)")
  (mt--is (tpl-compile [:neg 5])
          "(-5)"))

(ert-deftest test-majutsu-template-compile-json-line-sample ()
  (mt--is (tpl-compile [:concat
                        [:str "{"]
                        [:str "\"root\":"]
                        [:raw "if(self.root(), true, false)"]
                        [:str ",\"commit_id\":"]
                        [:json [:raw "self.commit_id()"]]
                        [:str "}"]])
          "concat(\"{\", \"\\\"root\\\":\", if(self.root(), true, false), \",\\\"commit_id\\\":\", json(self.commit_id()), \"}\")"))

(ert-deftest test-majutsu-template-string-escape ()
  ;; Quote and backslash
  (mt--is (tpl-compile [:str "A \"B\" \\"]) "\"A \\\"B\\\" \\\\\"")
  ;; Newline, tab, carriage return
  (mt--is (tpl-compile [:str "a\nb\tc\r"]) "\"a\\nb\\tc\\r\"")
  ;; NUL and ESC
  (let ((s (concat "x" (string 0) "y" (string 27) "z")))
    (mt--is (tpl-compile (vector :str s)) "\"x\\0y\\ez\""))
  ;; Other control: 0x01 -> \\x01, DEL -> \\x7F
  (let* ((s2 (concat (string 1) "-" (string 127)))
         (exp "\"\\x01-\\x7F\""))
    (mt--is (tpl-compile (vector :str s2)) exp))
  ;; Unicode stays verbatim
  (mt--is (tpl-compile [:str "雪"]) "\"雪\""))

(ert-deftest test-majutsu-template-defun-basic ()
  ;; Direct call produces AST and compiles
  (let ((node (majutsu-template-test-helper (majutsu-template-str "ID")
                                            (majutsu-template-str "VAL"))))
    (should (majutsu-template-node-p node))
    (should (equal (majutsu-template-compile node)
                   "concat(\"ID\", \": \", \"VAL\")")))
  ;; Optional argument omitted
  (let ((node (majutsu-template-test-helper (majutsu-template-str "ID"))))
    (should (equal (majutsu-template-compile node)
                   "concat(\"ID\", \": \", \"\")")))
  ;; DSL sugar
  (mt--is (tpl-compile [:test-helper [:str "ID"] [:str "X"]])
          "concat(\"ID\", \": \", \"X\")")
  (mt--is (tpl-compile [:call 'test-helper [:str "ID"] [:str "Y"]])
          "concat(\"ID\", \": \", \"Y\")")
  ;; Dynamic helper selection in :call
  (mt--is (tpl-compile [:call (if t 'test-helper 'json) [:str "ID"] [:str "Z"]])
          "concat(\"ID\", \": \", \"Z\")")
  ;; Dynamic helper selection in :call
  (mt--is (tpl-compile [:call (if t 'test-helper [:raw "json"]) [:str "ID"] [:str "Z"]])
          "concat(\"ID\", \": \", \"Z\")")
  (mt--is (tpl-compile [:call (if t [:raw "json"] 'a) [:str "ID"]])
          "json(\"ID\")")
  (mt--is (tpl-compile [:call (if t [:raw "json"] 'a) 'ID])
          "json(ID)")
  ;; Non-vector call name resolved at runtime
  (mt--is (majutsu-template-compile
           (majutsu-template-test-helper (majutsu-template-str "A")))
          "concat(\"A\", \": \", \"\")")
  ;; :raw expression evaluated to string
  (mt--is (tpl-compile [:concat [:raw (if t "foo" "bar")]])
          "concat(foo)")
  ;; Registry lookup via keyword/symbol
  (should (string= (majutsu-template--lookup-function-name :test-helper) "test-helper"))
  (should (string= (majutsu-template--lookup-function-name 'test-helper) "test-helper")))

(ert-deftest test-majutsu-template-ast-basic-nodes ()
  (let ((literal (majutsu-template-ast '[:str "X"]))
        (raw (majutsu-template-ast '[:raw "foo()"]))
        (call (majutsu-template-ast '[:concat [:str "A"] [:raw "bar"]])))
    (should (majutsu-template-node-p literal))
    (should (eq (majutsu-template-node-kind literal) :literal))
    (should (equal (majutsu-template-node-value literal) "X"))
    (should (majutsu-template-node-p raw))
    (should (eq (majutsu-template-node-kind raw) :raw))
    (should (equal (majutsu-template-node-value raw) "foo()"))
    (should (majutsu-template-node-p call))
    (should (eq (majutsu-template-node-kind call) :call))
    (should (equal (majutsu-template-node-value call) "concat"))
    (should (= (length (majutsu-template-node-args call)) 2))
    (should (eq (majutsu-template-node-kind (car (majutsu-template-node-args call))) :literal))
    (should (eq (majutsu-template-node-kind (cadr (majutsu-template-node-args call))) :raw))))

(ert-deftest test-majutsu-template-builtin-type-registry ()
  (let ((commit (majutsu-template--lookup-type 'Commit))
        (string-type (majutsu-template--lookup-type 'String))
        (option (majutsu-template--lookup-type 'Option)))
    (should commit)
    (should (equal (majutsu-template--type-name commit) 'Commit))
    (should (eq (alist-get 'Serialize (majutsu-template--type-converts-to commit)) 'yes))
    (should (eq (alist-get 'Template (majutsu-template--type-converts-to commit)) 'no))
    (should string-type)
    (should (eq (alist-get 'Template (majutsu-template--type-converts-to string-type)) 'yes))
    (should option)
    (should (eq (alist-get 'Boolean (majutsu-template--type-converts-to option)) 'yes))
    (should (eq (alist-get 'Serialize (majutsu-template--type-converts-to option)) 'maybe))))

(ert-deftest test-majutsu-template-builtin-method-registry ()
  ;; Keyword method: Commit.description()
  (let* ((meta (majutsu-template--lookup-method 'Commit "description"))
         (args (and meta (majutsu-template--fn-args meta))))
    (should meta)
    (should (equal (majutsu-template--fn-name meta) "description"))
    (should (eq (majutsu-template--fn-scope meta) :keyword))
    (should (eq (majutsu-template--fn-owner meta) 'Commit))
    (should (eq (majutsu-template--fn-returns meta) 'String))
    (should (= (length args) 1))
    (should (eq (majutsu-template--arg-name (car args)) 'self))
    (should (eq (majutsu-template--arg-type (car args)) 'Commit))
    (should (eq (majutsu-template--fn-symbol meta) 'majutsu-template--method-stub)))
  ;; Method with additional argument: List.map
  (let* ((meta (majutsu-template--lookup-method 'List "map"))
         (args (and meta (majutsu-template--fn-args meta))))
    (should meta)
    (should (eq (majutsu-template--fn-scope meta) :method))
    (should (eq (majutsu-template--fn-returns meta) 'ListTemplate))
    (should (= (length args) 2))
    (should (eq (majutsu-template--arg-name (car args)) 'self))
    (should (eq (majutsu-template--arg-type (car args)) 'List))
    (should (eq (majutsu-template--arg-type (cadr args)) 'Lambda))))

;;; majutsu-template-test.el ends here
