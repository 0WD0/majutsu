;;; majutsu-evolog.el --- JJ evolution log view for Majutsu  -*- lexical-binding: t; -*-

;;; Commentary:

;; This library renders `jj evolog' using a Majutsu-owned template equivalent
;; to jj's default compact evolog template.  Entries use the shared
;; row protocol, so jj keeps rendering the graph while Majutsu parses
;; heading and metadata modules back into real Magit sections.

;;; Code:

(require 'majutsu)
(require 'subr-x)

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
    ,majutsu-evolog--commit-labels-template]
  "Template fragment equivalent to jj's format_short_commit_header(commit).")

(defconst majutsu-evolog--description-template
  '[:separate " "
    [:if [:commit :empty]
        [:label "empty" "(empty)"]]
    [:if [:commit :description]
        [:commit :description :first_line]
      [:label [:if [:commit :empty]
                  "empty"
                "description placeholder"]
              "(no description set)"]]]
  "Template fragment equivalent to builtin_log_compact's description line.")

(defconst majutsu-evolog--commit-compact-template
  `[:if [:commit :root]
       [:label "immutable"
               [:separate " "
                          [:commit :change_id :shortest 8]
                          [:label "root" "root()"]
                          [:commit :commit_id :shortest 8]
                          [:commit :bookmarks]]]
     [:label
      [:separate " "
                 [:if [:commit :current_working_copy] "working_copy"]
                 [:if [:commit :immutable] "immutable" "mutable"]
                 [:if [:commit :conflict] "conflicted"]]
      [:concat ,majutsu-evolog--commit-header-template
               "\n"
               ,majutsu-evolog--description-template]]]
  "Template fragment equivalent to compact log for one evolog commit.")

(defconst majutsu-evolog--operation-line-template
  '[:if [:operation]
       [:concat
        "\n"
        [:separate " "
                   [:label "separator" "--"]
                   "operation"
                   [:operation :id :short]
                   [:operation :description :first_line]]]]
  "Template fragment for the builtin_evolog_compact operation line.")

(defconst majutsu-evolog--entry-display-template
  `[:concat
    ,majutsu-evolog--commit-compact-template
    ,majutsu-evolog--operation-line-template]
  "Visible evolog entry template, equivalent to jj's builtin_evolog_compact.")

(defconst majutsu-evolog-entry-layout
  `(:schema
    ((display :module heading :face t)
     (change-id :module metadata :face nil)
     (commit-id :module metadata :face nil)
     (operation-id :module metadata :face nil))
    :columns
    ((display ,majutsu-evolog--entry-display-template)
     (change-id [:commit :change_id])
     (commit-id [:commit :commit_id])
     (operation-id [:if [:operation] [:operation :id] ""])))
  "Declarative row layout for `majutsu-evolog'.")

(defun majutsu-evolog--record-field (entry field value)
  "Record canonical evolog FIELD VALUE onto ENTRY."
  (pcase field
    ('display
     (setq entry (plist-put entry :display value)))
    ('change-id
     (setq entry (plist-put entry :change-id value)))
    ('commit-id
     (setq entry (plist-put entry :commit-id value)))
    ('operation-id
     (setq entry (plist-put entry :operation-id value))))
  (majutsu-row-record-canonical-field entry field value))

(defun majutsu-evolog--entry-id (entry)
  "Return stable section id string from evolog ENTRY."
  (or (let ((commit-id (plist-get entry :commit-id)))
        (and (stringp commit-id)
             (not (string-empty-p (string-trim commit-id)))
             (substring-no-properties commit-id)))
      (let ((change-id (plist-get entry :change-id)))
        (and (stringp change-id)
             (not (string-empty-p (string-trim change-id)))
             (substring-no-properties change-id)))
      "unknown"))

(defun majutsu-evolog--row-profile ()
  "Return the row profile for `majutsu-evolog'."
  (majutsu-row-make-profile
   :name 'evolog
   :self-type 'CommitEvolutionEntry
   :layout-var 'majutsu-evolog-entry-layout
   :record-field-function 'majutsu-evolog--record-field
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

(defun majutsu-evolog--command-args (revset &optional args)
  "Return jj arguments for evolog REVSET with ARGS."
  (append majutsu-evolog--read-only-global-args
          '("evolog")
          (or args majutsu-evolog--args)
          (list "-r" revset "-T" majutsu-evolog--entry-template)))

(defun majutsu-evolog--wash-output (_args)
  "Wash raw `jj evolog` output in the current narrowed region."
  (majutsu-row-wash-buffer majutsu-evolog--entry-compiled))

(defun majutsu-evolog--insert-entries ()
  "Insert evolog output for the current buffer."
  (magit-insert-section (jj-evolog majutsu-evolog--revset)
    (magit-insert-heading (format "Evolution Log %s" majutsu-evolog--revset))
    (apply #'majutsu-jj-wash
           #'majutsu-evolog--wash-output
           'wash-anyway
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

(defvar-keymap majutsu-evolog-mode-map
  :doc "Keymap for `majutsu-evolog-mode'."
  :parent majutsu-mode-map)

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
Optional ARGS are passed to `jj evolog`."
  (interactive
   (list (majutsu-read-single-revset
          "Evolution log for revision"
          (or (majutsu-revision-at-point) "@"))))
  (let ((root (majutsu--toplevel-safe)))
    (majutsu-setup-buffer #'majutsu-evolog-mode nil
      :buffer (majutsu-evolog--buffer-name revset)
      :directory root
      (majutsu-evolog--revset revset)
      (majutsu-evolog--args args))))

(provide 'majutsu-evolog)
;;; majutsu-evolog.el ends here
