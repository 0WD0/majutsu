;;; majutsu-template-test.el --- Tests for majutsu-template DSL  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for majutsu-template DSL.

;;; Code:

(require 'ert)
(require 'majutsu-template)

(defvar mt--runtime-tmp nil)

(defmacro mt--is (form expected)
  `(let ((got ,form))
     (should (stringp got))
     (should (equal got ,expected))))

(eval-and-compile
  (majutsu-template-defun test-helper ((label Template)
                                       (value Template :optional t))
    (:returns Template :doc "Small helper used in tests.")
    `[:concat ,label [:str ": "] ,(or value [:str ""])])

  (majutsu-template-defun test-builtin-wrapper ((primary Template)
                                                (secondary Template :optional t)
                                                (rest Template :rest t))
    (:returns Template :doc "Auto-generated builtin wrapper for tests." :flavor :builtin))

  (majutsu-template-defun test-lambda-helper ()
    (:returns Lambda :doc "Reusable helper returning a native lambda.")
    [:lambda [c] [:method 'c :description]])

  (majutsu-template-defun test-lambda-helper-extra ((suffix Template))
    (:returns Lambda :doc "Native lambda helper capturing outer arguments.")
    `[:lambda [c] [:concat [:method 'c :description] ,suffix]])

  (majutsu-template-defkeyword test-commitref-keyword CommitRef
    (:returns Template :doc "Synthetic CommitRef keyword for tests.")
    `[:raw "commitref_keyword"])

  (majutsu-template-defkeyword test-commit-keyword Commit
    (:returns Template :doc "Synthetic Commit keyword for tests.")
    `[:raw "commit_keyword"])

  (majutsu-template-defmethod test-list-method List ((suffix Template))
    (:returns Template :doc "Synthetic List method for tests.")
    `[:raw "list_method_stub"])

  (majutsu-template-defmethod test-commit-flag Commit ()
    (:returns Template :doc "Non-keyword Commit method for tests.")
    `[:raw "commit_flag"])

  (majutsu-template-defmethod test-commit-optflag Commit ()
    (:returns Template :keyword t :doc "Opt-in keyword Commit method for tests.")
    `[:raw "commit_optflag"])

  (majutsu-template-defun test-bind-self ((object Commit :optional t))
    (:returns Template :doc "Helper using :bind-self for Commit objects." :bind-self object)
    [:if [:hidden]
        [:commit_id :shortest 8]
      [:change_id :shortest 8]])

  (majutsu-template-defun test-bind-self-inner ((object Commit :optional t))
    (:returns Template :doc "Nested helper using :bind-self." :bind-self object)
    [:description])

  (majutsu-template-defun test-bind-self-outer ((outer Commit)
                                                (inner Commit))
    (:returns Template :doc "Outer helper that restores nested :bind-self state." :bind-self outer)
    `[:concat [:description] " -> " [:test-bind-self-inner ,inner] " -> " [:description]])
  )

(ert-deftest test-majutsu-template-compile-basic ()
  (mt--is (majutsu-tpl [:concat [:str "Hello "] [:raw "self.author().name()"]])
          "concat(\"Hello \", self.author().name())")
  ;; Bare strings inside vector are treated as :str
  (mt--is (majutsu-tpl [:concat "A" "B"]) "concat(\"A\", \"B\")")
  ;; Vector not starting with operator defaults to concat
  (mt--is (majutsu-tpl ["A" "B"]) "concat(\"A\", \"B\")")
  (mt--is (majutsu-tpl ["A" [:raw "self.commit_id()"]])
          "concat(\"A\", self.commit_id())")
  (mt--is (majutsu-tpl [:if [:raw "self.root()"] [:str "(root)"] [:raw "format_short_commit_id(self.commit_id())"]])
          "if(self.root(), \"(root)\", format_short_commit_id(self.commit_id()))")
  (mt--is (majutsu-tpl [:if t "A" "B"]) "if(true, \"A\", \"B\")")
  ;; Optional else
  (mt--is (majutsu-tpl [:if t "A"]) "if(true, \"A\")")
  (mt--is (majutsu-tpl [:separate [:str " "] [:label "immutable" [:str "immutable"]] [:label "conflict" [:str "conflict"]]])
          "separate(\" \", label(\"immutable\", \"immutable\"), label(\"conflict\", \"conflict\"))")
  (mt--is (majutsu-tpl [:raw (if t "ac" "wa")])
          "ac")
  )

(ert-deftest test-majutsu-template-compile-vector-required ()
  ;; Runtime error for non-vector forms
  (should-error (eval '(majutsu-tpl (concat (str "A") (str "B")))) :type 'error)
  ;; Nested list is also rejected
  (should-error (majutsu-tpl [:concat (str "A") [:str "B"]]) :type 'error))

(ert-deftest test-majutsu-template-compile-numbers-booleans ()
  (mt--is (majutsu-tpl [:call 'pad_end 8 [:str "abc"]])
          "pad_end(8, \"abc\")")
  (mt--is (majutsu-tpl [:if t [:str "yes"] [:str "no"]])
          "if(true, \"yes\", \"no\")")
  (mt--is (majutsu-tpl [:if nil [:str "yes"] [:str "no"]])
          "if(false, \"yes\", \"no\")"))

(ert-deftest test-majutsu-template-compile-map-join ()
  (mt--is (majutsu-tpl [:map [:raw "self.bookmarks()"] b [:raw "b.name()"]])
          "self.bookmarks().map(|b| b.name())")
  (mt--is (majutsu-tpl [:map-join [:str ", "] [:raw "self.bookmarks()"] b [:raw "b.name()"]])
          "self.bookmarks().map(|b| b.name()).join(\", \")"))

(ert-deftest test-majutsu-template-compile-filter-any-all ()
  (mt--is (majutsu-tpl [:filter [:raw "parents"] c [:raw "c.mine()"]])
          "parents.filter(|c| c.mine())")
  (mt--is (majutsu-tpl [:any [:raw "parents"] c [:raw "c.conflict()"]])
          "parents.any(|c| c.conflict())")
  (mt--is (majutsu-tpl [:all [:raw "parents"] c [:raw "c.mine()"]])
          "parents.all(|c| c.mine())"))

(ert-deftest test-majutsu-template-compile-method-and-call ()
  (mt--is (majutsu-tpl [:method [:raw "self" :Commit] :commit_id])
          "self.commit_id()")
  (mt--is (majutsu-tpl [:method [:raw "self" :Commit] :diff "src"])
          "self.diff(\"src\")")
  (mt--is (majutsu-tpl [:method [:raw "self" :Commit] :parents :len])
          "self.parents().len()")
  (mt--is (majutsu-tpl [:call 'coalesce [:str ""] [:str "X"]])
          "coalesce(\"\", \"X\")")
  ;; :call with symbol name and bare string arg
  (mt--is (majutsu-tpl [:call 'json " "]) "json(\" \")")
  ;; :call with raw arg
  (mt--is (majutsu-tpl [:call 'json [:raw "test"]]) "json(test)")
  (mt--is (majutsu-tpl [:call 'a 'b]) "a(b)")
  (mt--is (majutsu-tpl [:call (if t 'a [:raw "hh"]) 'h]) "a(h)")
  (mt--is (majutsu-tpl [:call (if t [:raw (if t "hh" "wa")] 'bbb) 'x])
          "hh(x)")
  ;; dynamic decision in :call name
  (mt--is (majutsu-tpl [:call (if t 'json 'coalesce) [:str "ok"]])
          "json(\"ok\")")
  (mt--is (majutsu-tpl [:call (if nil 'json 'coalesce) [:str ""] [:str "x"]])
          "coalesce(\"\", \"x\")"))

(ert-deftest test-majutsu-template-compile-lambda ()
  (mt--is (majutsu-tpl [:lambda [c] [:method 'c :description]])
          "|c| c.description()")
  (mt--is (majutsu-tpl [:|c| [:method 'c :description]])
          "|c| c.description()")
  (mt--is (majutsu-tpl [:method [:raw "refs"] :map [:lambda [c] [:method 'c :description]]])
          "refs.map(|c| c.description())")
  (mt--is (majutsu-tpl [:method [:raw "refs"] :map [:|c| [:method 'c :description]]])
          "refs.map(|c| c.description())")
  (mt--is (majutsu-tpl [:lambda [c] c])
          "|c| c"))

(ert-deftest test-majutsu-template-call-lambda ()
  (mt--is (majutsu-tpl [:call [:lambda [c] [:method 'c :description]] [:raw "item"]])
          "item.description()")
  (mt--is (majutsu-tpl [:call [:|c| [:method 'c :description]] [:raw "item"]])
          "item.description()")
  (mt--is (majutsu-tpl [[:lambda [c] [:method 'c :description]] [:raw "item"]])
          "item.description()")
  (mt--is (majutsu-tpl [[:|c| [:method 'c :description]] [:raw "item"]])
          "item.description()")
  (mt--is (majutsu-tpl [:call [:lambda [c] [[:lambda [c] c] [:raw "inner"]]] [:raw "outer"]])
          "inner"))

(ert-deftest test-majutsu-template-dash-map-syntax ()
  (mt--is (majutsu-tpl [:-map [:lambda [c] [:method 'c :description]] [:raw "refs"]])
          "refs.map(|c| c.description())")
  (mt--is (majutsu-tpl [:--map [:method 'it :description] [:raw "refs"]])
          "refs.map(|it| it.description())")
  (mt--is (majutsu-template-compile '[:--map [:description] [:raw "refs"]] 'Commit)
          "refs.map(|it| self.description())")
  (mt--is (majutsu-tpl [:--all-p [:method 'it :present] [:raw "refs"]])
          "refs.all(|it| it.present())"))

(ert-deftest test-majutsu-template-lambda-invalid-syntax ()
  (should-error (majutsu-template-compile '[:lambda [a b] a]) :type 'error)
  (should-error (majutsu-template-compile '[:|c|]) :type 'error)
  (should-error (majutsu-template-compile '[:|c| a b]) :type 'error)
  (should-error (majutsu-template-compile '[:-map [:raw "not_a_lambda"] [:raw "refs"]])
                :type 'error))

(ert-deftest test-majutsu-template-compile-operators ()
  (mt--is (majutsu-tpl [:+ 1 2])
          "(1 + 2)")
  (mt--is (majutsu-tpl [:and [:> 3 1] [:<= 2 2]])
          "((3 > 1) && (2 <= 2))")
  (mt--is (majutsu-tpl [:concat-op [:str "a"] [:str "b"]])
          "(\"a\" ++ \"b\")")
  (mt--is (majutsu-tpl [:++ "L" "R"])
          "(\"L\" ++ \"R\")")
  (mt--is (majutsu-tpl [:not t])
          "(!true)")
  (mt--is (majutsu-tpl [:neg 5])
          "(-5)"))

(ert-deftest test-majutsu-template-compile-json-line-sample ()
  (mt--is (majutsu-tpl [:concat
                        [:str "{"]
                        [:str "\"root\":"]
                        [:raw "if(self.root(), true, false)"]
                        [:str ",\"commit_id\":"]
                        [:json [:raw "self.commit_id()"]]
                        [:str "}"]])
          "concat(\"{\", \"\\\"root\\\":\", if(self.root(), true, false), \",\\\"commit_id\\\":\", json(self.commit_id()), \"}\")"))

(ert-deftest test-majutsu-template-string-escape ()
  ;; Quote and backslash
  (mt--is (majutsu-tpl [:str "A \"B\" \\"]) "\"A \\\"B\\\" \\\\\"")
  ;; Newline, tab, carriage return
  (mt--is (majutsu-tpl [:str "a\nb\tc\r"]) "\"a\\nb\\tc\\r\"")
  ;; NUL and ESC
  (let ((s (concat "x" (string 0) "y" (string 27) "z")))
    (mt--is (majutsu-tpl (vector :str s)) "\"x\\0y\\ez\""))
  ;; Other control: 0x01 -> \\x01, DEL -> \\x7F
  (let* ((s2 (concat (string 1) "-" (string 127)))
         (exp "\"\\x01-\\x7F\""))
    (mt--is (majutsu-tpl (vector :str s2)) exp))
  ;; Unicode stays verbatim
  (mt--is (majutsu-tpl [:str "雪"]) "\"雪\""))

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
  (mt--is (majutsu-tpl [:test-helper [:str "ID"] [:str "X"]])
          "concat(\"ID\", \": \", \"X\")")
  (mt--is (majutsu-tpl [:call 'test-helper [:str "ID"] [:str "Y"]])
          "concat(\"ID\", \": \", \"Y\")")
  ;; Dynamic helper selection in :call
  (mt--is (majutsu-tpl [:call (if t 'test-helper 'json) [:str "ID"] [:str "Z"]])
          "concat(\"ID\", \": \", \"Z\")")
  ;; Dynamic helper selection in :call
  (mt--is (majutsu-tpl [:call (if t 'test-helper [:raw "json"]) [:str "ID"] [:str "Z"]])
          "concat(\"ID\", \": \", \"Z\")")
  (mt--is (majutsu-tpl [:call (if t [:raw "json"] 'a) [:str "ID"]])
          "json(\"ID\")")
  (mt--is (majutsu-tpl [:call (if t [:raw "json"] 'a) 'ID])
          "json(ID)")
  ;; Non-vector call name resolved at runtime
  (mt--is (majutsu-template-compile
           (majutsu-template-test-helper (majutsu-template-str "A")))
          "concat(\"A\", \": \", \"\")")
  ;; :raw expression evaluated to string
  (mt--is (majutsu-tpl [:concat [:raw (if t "foo" "bar")]])
          "concat(foo)")
  ;; Embedded condition evaluated prior to rewrite
  (mt--is (majutsu-tpl [:concat (if (> 2 1) [:str "T"] [:str "F"]) [:str "!"]])
          "concat(\"T\", \"!\")")
  ;; Registry lookup via keyword/symbol
  (should (string= (majutsu-template--lookup-function-name :test-helper) "test-helper"))
  (should (string= (majutsu-template--lookup-function-name 'test-helper) "test-helper")))

(ert-deftest test-majutsu-template-bind-self-explicit-object ()
  (mt--is (majutsu-tpl [:test-bind-self [:raw "p" :Commit]])
          "if(p.hidden(), p.commit_id().shortest(8), p.change_id().shortest(8))"))

(ert-deftest test-majutsu-template-bind-self-inherits-outer-self ()
  (mt--is (majutsu-template-compile '[:test-bind-self] 'Commit)
          "if(self.hidden(), self.commit_id().shortest(8), self.change_id().shortest(8))"))

(ert-deftest test-majutsu-template-bind-self-restores-outer-binding ()
  (mt--is (majutsu-tpl [:test-bind-self-outer [:raw "lhs" :Commit] [:raw "rhs" :Commit]])
          "concat(lhs.description(), \" -> \", rhs.description(), \" -> \", lhs.description())"))

(ert-deftest test-majutsu-template-bind-self-invalid-parameter ()
  (should-error
   (eval '(majutsu-template-defun test-bind-self-missing ((object Commit))
            (:returns Template :bind-self missing)
            [:description]))
   :type 'error))

(ert-deftest test-majutsu-template-bind-self-rest-parameter-rejected ()
  (should-error
   (eval '(majutsu-template-defun test-bind-self-rest ((objects Commit :rest t))
            (:returns Template :bind-self objects)
            [:description]))
   :type 'error))

(ert-deftest test-majutsu-template-builtin-flavor-auto-body ()
  (mt--is (majutsu-tpl [:call 'test-builtin-wrapper [:str "L"]])
          "test-builtin-wrapper(\"L\")")
  ;; Optional argument present and multiple rest args.
  (mt--is (majutsu-tpl [:call 'test-builtin-wrapper [:str "L"] [:str "R"] [:str "X"] [:str "Y"]])
          "test-builtin-wrapper(\"L\", \"R\", \"X\", \"Y\")"))

(ert-deftest test-majutsu-template-map-sugar-lowers-to-native-lambda ()
  (mt--is (majutsu-tpl [:map [:raw "xs"] item [:raw "item.value()"]])
          "xs.map(|item| item.value())")
  (mt--is (majutsu-tpl [:filter [:raw "xs"] item [:raw "item.present()"]])
          "xs.filter(|item| item.present())"))

(ert-deftest test-majutsu-template-native-lambda-helper ()
  (mt--is (majutsu-tpl [:test-lambda-helper])
          "|c| c.description()")
  (mt--is (majutsu-tpl [:test-lambda-helper-extra [:str "!"]])
          "|c| concat(c.description(), \"!\")")
  (mt--is (majutsu-tpl [:method [:raw "refs"] :map [:test-lambda-helper]])
          "refs.map(|c| c.description())")
  (mt--is (majutsu-tpl [:method [:raw "refs"] :map [:test-lambda-helper-extra [:str "!"]]])
          "refs.map(|c| concat(c.description(), \"!\"))"))

(ert-deftest test-majutsu-template-call-dispatch ()
  ;; Built-in flavor should emit direct jj call.
  (mt--is (majutsu-tpl [:call 'concat [:str "L"] [:str "R"]])
          "concat(\"L\", \"R\")")
  ;; Custom helper keeps macro-generated body.
  (mt--is (majutsu-tpl [:call 'test-helper [:str "ID"] [:str "V"]])
          "concat(\"ID\", \": \", \"V\")")
  ;; Falling back to raw name works for unknown helper.
  (mt--is (majutsu-tpl [:call 'unknown [:str "X"]])
          "unknown(\"X\")")
  ;; Lookup by string literal reuses existing metadata.
  (mt--is (majutsu-tpl [:call "concat" [:str "P"] [:str "Q"]])
          "concat(\"P\", \"Q\")"))

(ert-deftest test-majutsu-template-ast-basic-nodes ()
  (let ((literal (majutsu-template--rewrite '[:str "X"]))
        (raw (majutsu-template--rewrite '[:raw "foo()"]))
        (call (majutsu-template--rewrite '[:concat [:str "A"] [:raw "bar"]])))
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

(ert-deftest test-majutsu-template-ast-evaluates-elisp ()
  (let* ((majutsu-template--allow-eval t)
         (node (majutsu-template--rewrite `[:concat ,(majutsu-template-str "X") [:str "Y"]])))
    (should (majutsu-template-node-p node))
    (should (equal (majutsu-template-compile node)
                   "concat(\"X\", \"Y\")")))
  (should (equal (majutsu-tpl [:concat (if nil [:str "no"] [:str "yes"]) [:str "!"]])
                 "concat(\"yes\", \"!\")"))
  (mt--is (majutsu-tpl [:call '+ 3 4]) "(3 + 4)")
  (mt--is (majutsu-tpl [:call '+ [:str "A"] [:str "B"]]) "(\"A\" + \"B\")"))

(ert-deftest test-majutsu-template-runtime-var-eval ()
  (let* ((mt--runtime-tmp 1)
         (s [:concat (if (> 2 mt--runtime-tmp) [:str "T"] [:str "F"]) [:str "!"]])
         (mt--runtime-tmp 3))
    (mt--is (majutsu-tpl s) "concat(\"F\", \"!\")")))

(ert-deftest test-majutsu-template-raw-type-annotation ()
  (let ((node (majutsu-template--rewrite '[:raw "foo" :Template])))
    (should (majutsu-template-node-p node))
    (should (eq (majutsu-template-node-kind node) :raw))
    (should (equal (majutsu-template-node-value node) "foo"))
    (should (eq (majutsu-template-node-type node) 'Template))
    (should (equal (plist-get (majutsu-template-node-props node) :declared) 'Template)))
  (should (equal (majutsu-tpl [:raw "foo" :Template]) "foo")))

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
    (should (eq (majutsu-template--fn-owner meta) 'Commit))
    (should (majutsu-template--fn-keyword meta))
    (should (eq (majutsu-template--fn-returns meta) 'String))
    (should (= (length args) 1))
    (should (eq (majutsu-template--arg-name (car args)) 'self))
    (should (eq (majutsu-template--arg-type (car args)) 'Commit))
    (should (eq (majutsu-template--fn-symbol meta) 'majutsu-template--method-stub)))
  ;; Method with additional argument: List.map
  (let* ((meta (majutsu-template--lookup-method 'List "map"))
         (args (and meta (majutsu-template--fn-args meta))))
    (should meta)
    (should (eq (majutsu-template--fn-owner meta) 'List))
    (should-not (majutsu-template--fn-keyword meta))
    (should (eq (majutsu-template--fn-returns meta) 'ListTemplate))
    (should (= (length args) 2))
    (should (eq (majutsu-template--arg-name (car args)) 'self))
    (should (eq (majutsu-template--arg-type (car args)) 'List))
    (should (eq (majutsu-template--arg-type (cadr args)) 'Lambda))))

(ert-deftest test-majutsu-template-custom-callable-metadata ()
  (let ((kw (majutsu-template--lookup-keyword 'CommitRef "test-commitref-keyword")))
    (should kw)
    (should (majutsu-template--fn-keyword kw))
    (should (eq (majutsu-template--fn-owner kw) 'CommitRef))
    (should (= (length (majutsu-template--fn-args kw)) 1)))
  (let ((method (majutsu-template--lookup-method 'List "test-list-method")))
    (should method)
    (should (not (majutsu-template--fn-keyword method)))
    (should (eq (majutsu-template--fn-owner method) 'List))
    (should (= (length (majutsu-template--fn-args method)) 2)))
  (let ((helper (majutsu-template--lookup-function-meta 'test-bind-self)))
    (should helper)
    (should (eq (majutsu-template--fn-bind-self helper) 'object)))
  (let ((helper (majutsu-template--lookup-function-meta 'test-lambda-helper)))
    (should helper)
    (should (eq (majutsu-template--fn-returns helper) 'Lambda))))

(ert-deftest test-majutsu-template-label-helper ()
  (let ((node (majutsu-template-label "status" (majutsu-template-str "ok"))))
    (should (majutsu-template-node-p node))
    (should (equal (majutsu-template-compile node)
                   "label(\"status\", \"ok\")"))))

(ert-deftest test-majutsu-template-map-join-helper ()
  (let ((node (majutsu-template-map-join [:str ", "]
                                         [:raw "self.parents()"]
                                         'p
                                         [:raw "p.commit_id()"])))
    (should (majutsu-template-node-p node))
    (should (equal (majutsu-template-compile node)
                   "self.parents().map(|p| p.commit_id()).join(\", \")"))))

(ert-deftest test-majutsu-template-self-keyword-basic ()
  (mt--is (majutsu-tpl [:description] 'Commit)
          "self.description()"))

(ert-deftest test-majutsu-template-self-keyword-chain ()
  (mt--is (majutsu-tpl [:parents :len] 'Commit)
          "self.parents().len()"))

(ert-deftest test-majutsu-template-self-keyword-custom-defkeyword ()
  (mt--is (majutsu-tpl [:test-commit-keyword] 'Commit)
          "self.test-commit-keyword()"))

(ert-deftest test-majutsu-template-self-keyword-custom-defmethod-opt-in ()
  (mt--is (majutsu-tpl [:test-commit-optflag] 'Commit)
          "self.test-commit-optflag()"))

(ert-deftest test-majutsu-template-with-self-binding ()
  (let ((majutsu-template-default-self-type nil)
        (majutsu-template--self-stack
         (list (majutsu-template--make-self-binding
                :node (majutsu-template-raw "op" 'Operation)
                :type 'Operation))))
    (mt--is (majutsu-template-compile '[:id])
            "op.id()")))

(ert-deftest test-majutsu-template-self-keyword-missing-context ()
  (should-error (majutsu-template-compile '[:description] '_)
                :type 'error))

(ert-deftest test-majutsu-template-self-keyword-unknown-method ()
  (should-error (majutsu-template-compile '[:change_id] 'Operation)
                :type 'error))

(ert-deftest test-majutsu-template-self-keyword-arguments-rejected ()
  (should-error (majutsu-template-compile '[:description "X"])
                :type 'error))

(ert-deftest test-majutsu-template-self-nonkeyword-not-dispatched ()
  (should-error (majutsu-template-compile '[:test-commit-flag])
                :type 'error))

(ert-deftest test-majutsu-template-self-nonkeyword-explicit-call ()
  (mt--is (majutsu-tpl [:method [:raw "self" :Commit] :test-commit-flag])
          "self.test-commit-flag()"))

(ert-deftest test-majutsu-template-compile-with-explicit-self-type ()
  (mt--is (majutsu-tpl [:user] 'Operation)
          "self.user()")
  (mt--is (majutsu-template-compile '[:user] 'Operation)
          "self.user()"))
;;; majutsu-template-test.el ends here
