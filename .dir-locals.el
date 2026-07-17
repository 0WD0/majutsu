;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

;; Indent specs mirror (declare (indent ...)) so TAB works even when the
;; defining feature is not loaded yet.  These are reindent hints only;
;; they do not reflow forms.  Prefer Doom's lisp-indent +format for
;; reindent-only buffer formatting, not deterministic autofmt.
((nil . ((indent-tabs-mode . nil)))
 (emacs-lisp-mode
  . ((lisp-indent-local-overrides
      . (;; majutsu-jj
         (majutsu-with-editor . 0)
         (majutsu--with-safe-default-directory . 1)
         (majutsu--with-no-color . 0)
         (majutsu-with-toplevel . defun)
         (majutsu-jj-wash . 2)
         ;; buffers / selection / UI
         (majutsu-setup-buffer . 2)
         (majutsu-selection--with-session-buffer . 1)
         (majutsu-forge--with-section-errors . 1)
         (majutsu-evil--define-keys . 2)
         ;; definition macros
         (majutsu-log-define-column . 1)
         (majutsu-bookmark-define-template . 1)
         (majutsu-template-defspecial . defun)
         (majutsu-template-defun . defun)
         (majutsu-template-defmethod . defun)
         (majutsu-template-defkeyword . defun)
         ;; test helpers (also used when editing under test/)
         (majutsu-jj-integration-with-sandbox . 1)
         (majutsu-jj-integration-with-repo . 1)
         ;; prefix + suffix distinguished; rest is body
         (majutsu-diff-test--with-transient-context . 2)
         (majutsu-diff-test--with-washed-git-hunks . 0)
         (majutsu-process-test--with-sh . 0)
         (majutsu-remote-test--with-root . 0))))))
