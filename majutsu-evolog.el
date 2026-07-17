;;; majutsu-evolog.el --- JJ evolution log view for Majutsu  -*- lexical-binding: t; -*-

;;; Commentary:

;; This library renders `jj evolog' using a Majutsu-owned template.  Entries
;; use the shared row protocol, so jj keeps rendering the graph while Majutsu
;; controls the visible style and parses metadata back into Magit sections.

;;; Code:

(require 'majutsu)
(require 'majutsu-diff)
(require 'seq)
(require 'subr-x)
(require 'transient)

(declare-function majutsu-jj-wash "majutsu-jj" (washer keep-error &rest args))

(defconst majutsu-evolog--change-offset-template
  '[:if [:commit :change_offset]
       [:concat [:label "change_offset" "/"]
                [:commit :change_offset]]]
  "Template fragment equivalent to jj's format_change_offset(commit).")

(defconst majutsu-evolog--short-change-id-template
  `[:coalesce
    [:if [:commit :hidden]
        [:label "hidden"
                [:concat [:commit :change_id :shortest 8]
                         ,majutsu-evolog--change-offset-template]]]
    [:if [:commit :divergent]
        [:label "divergent"
                [:concat [:commit :change_id :shortest 8]
                         ,majutsu-evolog--change-offset-template]]]
    [:commit :change_id :shortest 8]]
  "Template fragment equivalent to jj's short change-id formatter.")

(defconst majutsu-evolog--commit-labels-template
  '[:separate " "
    [:coalesce
     [:if [:commit :hidden]
         [:label "hidden" "(hidden)"]]
     [:if [:commit :divergent]
         [:label "divergent" "(divergent)"]]]
    [:if [:commit :conflict]
        [:label "conflict" "(conflict)"]]]
  "Template fragment equivalent to jj's format_commit_labels(commit).")

(defconst majutsu-evolog--signature-template
  '[:if [:method [:call 'config "ui.show-cryptographic-signatures"]
         :as_boolean]
       [:call 'format_short_cryptographic_signature
              [:commit :signature]]]
  "Template fragment for jj's optional compact signature marker.")

(defconst majutsu-evolog--commit-header-template
  `[:separate " "
    ,majutsu-evolog--short-change-id-template
    [:coalesce [:commit :author :email]
               [:label "email placeholder" "(no email set)"]]
    [:commit :committer :timestamp :local :format "%Y-%m-%d %H:%M:%S"]
    [:commit :bookmarks]
    [:commit :tags]
    [:commit :working_copies]
    [:commit :commit_id :shortest 8]
    ,majutsu-evolog--commit-labels-template
    ,majutsu-evolog--signature-template]
  "Template fragment equivalent to jj's format_short_commit_header(commit).")

(defconst majutsu-evolog--description-template
  '[:separate " "
    [:if [:commit :empty]
        [:label "empty" "(empty)"]]
    [:if [:commit :description]
        [:commit :description :first_line]
      [:label [:if [:commit :empty] "empty"]
              [:label "description placeholder"
                      "(no description set)"]]]]
  "Template fragment equivalent to builtin_log_compact's description line.")

(defconst majutsu-evolog--commit-compact-template
  `[:if [:commit :root]
       [:label "immutable"
               [:concat
                [:separate " "
                           [:commit :change_id :shortest 8]
                           [:label "root" "root()"]
                           [:commit :commit_id :shortest 8]
                           [:commit :bookmarks]]
                "\n"]]
     [:label
      [:separate " "
                 [:if [:commit :current_working_copy] "working_copy"]
                 [:if [:commit :immutable] "immutable" "mutable"]
                 [:if [:commit :conflict] "conflicted"]]
      [:concat ,majutsu-evolog--commit-header-template
               "\n"
               ,majutsu-evolog--description-template
               "\n"]]]
  "Template fragment equivalent to compact log for one evolog commit.")

(defconst majutsu-evolog--operation-line-template
  '[:if [:operation]
       [:concat
        [:separate " "
                   [:label "separator" "--"]
                   "operation"
                   [:operation :id :short]
                   [:operation :description :first_line]]
        "\n"]]
  "Template fragment for the builtin_evolog_compact operation line.")

(defconst majutsu-evolog--entry-display-template
  `[:concat
    ,majutsu-evolog--commit-compact-template
    ,majutsu-evolog--operation-line-template]
  "Majutsu-owned visible template for one evolog entry.")

(defconst majutsu-evolog-entry-columns
  `((:field display :module heading
     :template ,majutsu-evolog--entry-display-template :face t)
    (:field change-id :module metadata
     :template [:commit :change_id] :face nil)
    (:field commit-id :module metadata
     :template [:commit :commit_id] :face nil)
    (:field operation-id :module metadata
     :template [:if [:operation] [:operation :id] ""] :face nil))
  "Flat row columns for `majutsu-evolog'.")

(defun majutsu-evolog--entry-id (entry)
  "Return stable section id string from evolog ENTRY."
  (or (let ((commit-id (majutsu-row-column entry 'commit-id)))
        (and (stringp commit-id)
             (not (string-empty-p (string-trim commit-id)))
             (substring-no-properties commit-id)))
      (let ((change-id (majutsu-row-column entry 'change-id)))
        (and (stringp change-id)
             (not (string-empty-p (string-trim change-id)))
             (substring-no-properties change-id)))
      "unknown"))

(defun majutsu-evolog--row-profile ()
  "Return the row profile for `majutsu-evolog'."
  (majutsu-row-make-profile
   :name 'evolog
   :self-type 'CommitEvolutionEntry
   :columns-var 'majutsu-evolog-entry-columns
   :entry-id-function 'majutsu-evolog--entry-id
   :section-class 'jj-evolog-entry
   :section-value-function 'majutsu-evolog--entry-id))

(defconst majutsu-evolog--entry-compiled
  (majutsu-row-compile (majutsu-evolog--row-profile))
  "Compiled row layout metadata for `majutsu-evolog'.")

(defconst majutsu-evolog--entry-template
  (plist-get majutsu-evolog--entry-compiled :template)
  "Template used by `majutsu-evolog'.")

(defconst majutsu-evolog--read-only-global-args
  '("--at-op=@" "--ignore-working-copy")
  "Top-level jj arguments for read-only evolog queries.")

(defvar-local majutsu-evolog--revset nil
  "Revset used for the current evolog buffer.")

(defvar-local majutsu-evolog--args nil
  "Arguments used for the current evolog buffer.")

(defun majutsu-evolog--validate-args (args)
  "Return normalized safe evolog list ARGS or signal `user-error'.
Majutsu owns the revision and template arguments because they frame the row
protocol.  Only one limit, reverse-order switch, and graph switch are allowed."
  (unless (listp args)
    (user-error "Evolog arguments must be a list, got %S" args))
  (let ((rest args)
        (seen (make-hash-table :test #'eq))
        normalized)
    (while rest
      (let ((arg (pop rest)))
        (unless (stringp arg)
          (user-error "Unsupported evolog argument: %S" arg))
        (pcase-let*
            ((`(,kind ,value)
              (cond
               ((string-match "\\`--limit=\\([0-9]+\\)\\'" arg)
                (list 'limit
                      (format "--limit=%s" (match-string 1 arg))))
               ((member arg '("-n" "--limit"))
                (let ((limit (pop rest)))
                  (unless (and (stringp limit)
                               (string-match-p "\\`[0-9]+\\'" limit))
                    (user-error "Invalid evolog limit: %S" limit))
                  (list 'limit (format "--limit=%s" limit))))
               ((equal arg "--reversed")
                '(reversed "--reversed"))
               ((member arg '("-G" "--no-graph"))
                '(no-graph "--no-graph"))
               (t
                (user-error "Unsupported evolog argument: %s" arg)))))
          (when (gethash kind seen)
            (user-error "Duplicate evolog argument: %s" arg))
          (puthash kind t seen)
          (push value normalized))))
    (nreverse normalized)))

(defun majutsu-evolog--command-args (revset &optional args)
  "Return jj arguments for evolog REVSET with ARGS."
  (let ((args (majutsu-evolog--validate-args
               (or args majutsu-evolog--args))))
    (append majutsu-evolog--read-only-global-args
            majutsu-row-protocol-global-args
            '("evolog")
            args
            (list "-r" revset "-T" majutsu-evolog--entry-template))))

(defun majutsu-evolog--wash-output (_args)
  "Wash raw `jj evolog` output in the current narrowed region."
  (majutsu-row-wash-buffer majutsu-evolog--entry-compiled))

(defun majutsu-evolog--insert-entries ()
  "Insert evolog output for the current buffer."
  (magit-insert-section (jj-evolog majutsu-evolog--revset)
    (magit-insert-heading (format "Evolution Log %s" majutsu-evolog--revset))
    (apply #'majutsu-jj-wash
           #'majutsu-evolog--wash-output
           nil
           (majutsu-evolog--command-args
            (or majutsu-evolog--revset "@")
            majutsu-evolog--args))))

(defun majutsu-evolog-refresh-buffer ()
  "Refresh the current evolog buffer."
  (interactive)
  (majutsu--assert-mode 'majutsu-evolog-mode)
  (majutsu-row-clear-buffer-data)
  (magit-insert-section (evologbuf)
    (majutsu-evolog--insert-entries)))

;;;###autoload(autoload 'majutsu-evolog-copy-transient "majutsu-evolog" nil t)
(majutsu-row-define-copy-transient
 majutsu-evolog-copy-transient
 "Transient for semantic copy commands in `majutsu-evolog-mode'."
 ("h" "Commit hash" majutsu-row-copy-commit-id))

(defclass majutsu-evolog-prefix (transient-prefix) ())

(cl-defmethod transient-init-value ((obj majutsu-evolog-prefix))
  (oset obj value
        (and (derived-mode-p 'majutsu-evolog-mode)
             majutsu-evolog--args)))

(transient-define-argument majutsu-evolog:--limit ()
  :description "Limit"
  :class 'transient-option
  :shortarg "-n"
  :argument "--limit="
  :reader #'transient-read-number-N0)

(transient-define-argument majutsu-evolog:--reversed ()
  :description "Reverse order"
  :class 'transient-switch
  :key "-v"
  :argument "--reversed")

(transient-define-argument majutsu-evolog:--no-graph ()
  :description "Hide graph"
  :class 'transient-switch
  :shortarg "-G"
  :argument "--no-graph")

(defun majutsu-evolog-transient-reset ()
  "Reset options in the current evolog buffer."
  (interactive)
  (unless (derived-mode-p 'majutsu-evolog-mode)
    (user-error "Not in a Majutsu evolog buffer"))
  (setq-local majutsu-evolog--args nil)
  (transient-reset))

;;;###autoload(autoload 'majutsu-evolog-transient "majutsu-evolog" nil t)
(transient-define-prefix majutsu-evolog-transient ()
  "Transient interface for adjusting jj evolog options."
  :man-page "jj-evolog"
  :class 'majutsu-evolog-prefix
  :transient-non-suffix t
  [["Options"
    (majutsu-evolog:--limit)
    (majutsu-evolog:--reversed)
    (majutsu-evolog:--no-graph)]
   ["Actions"
    ("g" "Apply" majutsu-evolog-transient)
    ("0" "Reset options" majutsu-evolog-transient-reset :transient t)]]
  (interactive)
  (if (not (eq transient-current-command 'majutsu-evolog-transient))
      (transient-setup 'majutsu-evolog-transient)
    (unless (derived-mode-p 'majutsu-evolog-mode)
      (user-error "Not in a Majutsu evolog buffer"))
    (setq-local majutsu-evolog--args
                (majutsu-evolog--validate-args
                 (transient-args transient-current-command)))
    (majutsu-refresh-buffer)))

(defvar-local majutsu-evolog-diff--commit-id nil
  "Full commit id selected for the current evolog inter-diff buffer.")

(defun majutsu-evolog--validate-commit-id (commit-id)
  "Return COMMIT-ID when it is a hexadecimal commit id."
  (unless (and (stringp commit-id)
               (string-match-p "\\`[0-9a-f]+\\'" commit-id))
    (user-error "Invalid evolog commit id: %S" commit-id))
  (substring-no-properties commit-id))

(defun majutsu-evolog--inter-diff-command-args (commit-id)
  "Return jj arguments for the native inter-diff of COMMIT-ID."
  (setq commit-id (majutsu-evolog--validate-commit-id commit-id))
  (append majutsu-evolog--read-only-global-args
          '("evolog" "--no-graph" "--limit=1" "--patch" "--git")
          (list "-r" (format "commit_id(%s)" commit-id)
                "-T" "")))

(defun majutsu-evolog-diff-at-point ()
  "Show the native inter-diff for the evolution entry at point."
  (interactive)
  (let* ((entry (majutsu-row-current-entry "No evolution entry at point"))
         (commit-id (majutsu-row-column entry 'commit-id)))
    (majutsu-evolog-inter-diff commit-id)))

(defvar-keymap majutsu-evolog-diff-section-map
  :doc "Restricted keymap for sections in an evolog inter-diff."
  "<remap> <majutsu-visit-thing>" #'undefined
  "C-j" #'magit-section-forward
  "C-<return>" #'undefined)

(defclass majutsu-evolog-file-section (majutsu-file-section)
  ((keymap :initform 'majutsu-evolog-diff-section-map)))

(defclass majutsu-evolog-hunk-section (majutsu-hunk-section)
  ((keymap :initform 'majutsu-evolog-diff-section-map)))

(defun majutsu-evolog--wash-inter-diff (args)
  "Wash an evolog inter-diff as a git patch."
  (let ((magit--section-type-alist
         (append '((jj-file . majutsu-evolog-file-section)
                   (jj-hunk . majutsu-evolog-hunk-section))
                 magit--section-type-alist)))
    (majutsu-diff-wash-diffs args)))

(defun majutsu-evolog-diff-refresh-buffer ()
  "Refresh the current evolog inter-diff buffer."
  (interactive)
  (majutsu--assert-mode 'majutsu-evolog-diff-mode)
  (let* ((commit-id (majutsu-evolog--validate-commit-id
                     majutsu-evolog-diff--commit-id))
         (majutsu-jj-global-arguments
          (cons "--color=never"
                (seq-remove (lambda (arg)
                              (string-prefix-p "--color" arg))
                            majutsu-jj-global-arguments)))
         (majutsu-process-apply-ansi-colors nil))
    (majutsu-diff--set-left-margin 0)
    (magit-insert-section (jj-evolog-diff commit-id)
      (magit-insert-heading (format "Evolution Diff %s" commit-id))
      (let ((beg (point)))
        (magit-insert-section (diff-root)
          (apply #'majutsu-jj-wash
                 #'majutsu-evolog--wash-inter-diff
                 nil
                 (majutsu-evolog--inter-diff-command-args commit-id)))
        (when (= beg (point))
          (insert "No patch\n"))))))

(defvar-keymap majutsu-evolog-diff-mode-map
  :doc "Keymap for read-only evolog inter-diff buffers."
  :parent magit-section-mode-map
  "g" #'majutsu-refresh
  "q" #'majutsu-mode-bury-buffer
  "$" #'majutsu-process-buffer
  "t" #'majutsu-diff-toggle-refine-hunk
  "C-j" #'magit-section-forward
  "C-k" #'magit-section-backward
  "RET" #'undefined
  "d" #'undefined
  "e" #'undefined
  "E" #'undefined
  "T" #'undefined
  "+" #'undefined
  "-" #'undefined
  "0" #'undefined
  "j" #'undefined)

(define-derived-mode majutsu-evolog-diff-mode majutsu-diff-mode
  "Majutsu Evolog Diff"
  "Major mode for a native jj evolution inter-diff.
An inter-diff can combine multiple predecessors, so revision-based visit and
edit actions inherited from ordinary diff buffers are intentionally disabled."
  :group 'majutsu
  (setq-local majutsu-buffer-diff-args '("--git"))
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

(defun majutsu-evolog-diff--buffer-name (commit-id)
  "Return the inter-diff buffer name for COMMIT-ID."
  (let* ((root (majutsu--toplevel-safe))
         (repo (file-name-nondirectory (directory-file-name root))))
    (format "*majutsu-evolog-diff: %s:%s*" repo commit-id)))

;;;###autoload
(defun majutsu-evolog-inter-diff (commit-id)
  "Show the native evolution inter-diff for full COMMIT-ID."
  (interactive (list (majutsu-row-column
                      (majutsu-row-current-entry "No evolution entry at point")
                      'commit-id)))
  (setq commit-id (majutsu-evolog--validate-commit-id commit-id))
  (let ((root (majutsu--toplevel-safe)))
    (majutsu-setup-buffer #'majutsu-evolog-diff-mode t
      :buffer (majutsu-evolog-diff--buffer-name commit-id)
      :directory root
      (majutsu-evolog-diff--commit-id commit-id))))

(defvar-keymap majutsu-evolog-mode-map
  :doc "Keymap for `majutsu-evolog-mode'."
  :parent majutsu-mode-map
  "RET" #'majutsu-evolog-diff-at-point
  "l" #'majutsu-evolog-transient)

(define-derived-mode majutsu-evolog-mode majutsu-mode "Majutsu Evolog"
  "Major mode for viewing jj change evolution history."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer)
  (setq-local filter-buffer-substring-function
              #'majutsu-row-filter-buffer-substring))

(defun majutsu-evolog--buffer-name (revset)
  "Return buffer name for evolog REVSET."
  (let* ((root (majutsu--toplevel-safe))
         (repo (file-name-nondirectory (directory-file-name root))))
    (format "*majutsu-evolog: %s:%s*" repo revset)))

;;;###autoload
(defun majutsu-evolog (revset &optional args)
  "Show the evolution history for REVSET.
Optional ARGS may contain safe list-formatting options for `jj evolog`."
  (interactive
   (list (majutsu-read-single-revset
          "Evolution log for revision"
          (or (majutsu-revision-at-point) "@"))))
  (setq args (majutsu-evolog--validate-args args))
  (let ((root (majutsu--toplevel-safe)))
    (majutsu-setup-buffer #'majutsu-evolog-mode nil
      :buffer (majutsu-evolog--buffer-name revset)
      :directory root
      (majutsu-evolog--revset revset)
      (majutsu-evolog--args args))))

(provide 'majutsu-evolog)
;;; majutsu-evolog.el ends here
