;;; majutsu-jt.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup majutsu-jt nil
  "Elisp wrapper for composing jj templates."
  :group 'majutsu)


(defun majutsu-jt--symbol->template-name (sym)
  "Return the template name string corresponding to SYM.
Accepts symbols and keywords."
  (cond
   ((keywordp sym) (substring (symbol-name sym) 1))
   ((symbolp sym) (symbol-name sym))
   (t (user-error "majutsu-template: invalid function name %S" sym))))
