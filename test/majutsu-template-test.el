;;; majutsu-template-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'majutsu-template)

(defmacro mt--is (form expected)
  `(let ((got ,form))
     (should (stringp got))
     (should (equal got ,expected))))

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
          "separate(\" \", label(\"immutable\", \"immutable\"), label(\"conflict\", \"conflict\"))"))

(ert-deftest test-majutsu-template-compile-vector-required ()
  (should-error (macroexpand-1 '(tpl (concat (str "A") (str "B")))) :type 'error)
  (should-error (eval '(tpl-compile (concat (str "A") (str "B")))) :type 'error)
  ;; Nested list is also rejected
  (should-error (tpl-compile [:concat (str "A") [:str "B"]]) :type 'error))

(ert-deftest test-majutsu-template-compile-numbers-booleans ()
  (mt--is (tpl-compile [:call pad_end 8 [:str "abc"]])
          "pad_end(8, \"abc\")")
  (mt--is (tpl-compile [:if t [:str "yes"] [:str "no"]])
          "if(true, \"yes\", \"no\")")
  (mt--is (tpl-compile [:if nil [:str "yes"] [:str "no"]])
          "if(false, \"yes\", \"no\")"))

(ert-deftest test-majutsu-template-compile-lit-raw-e ()
  (let ((user "Alice"))
    (mt--is (tpl-compile [:concat [:str "Author:"] [:str " "] [:lit user]])
            "concat(\"Author:\", \" \", \"Alice\")"))
  (mt--is (tpl-compile [:raw-e (format "format_timestamp(%s)" "commit_timestamp(self)")])
          "format_timestamp(commit_timestamp(self))"))

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
  (mt--is (tpl-compile [:call coalesce [:str ""] [:str "X"]])
          "coalesce(\"\", \"X\")")
  ;; :call with symbol name and bare string arg
  (mt--is (tpl-compile [:call json " "]) "json(\" \")")
  ;; :call with raw arg
  (mt--is (tpl-compile [:call json [:raw "test"]]) "json(test)"))

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

;;; majutsu-template-test.el ends here
