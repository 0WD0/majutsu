;;; majutsu-evolog.el --- JJ evolution log view for Majutsu  -*- lexical-binding: t; -*-

;;; Commentary:

;; This library renders `jj evolog' using a Majutsu-owned template equivalent
;; to jj's default compact evolog template, wrapped in graph-record tokens so
;; the graph stays intact while record boundaries remain machine-readable.

;;; Code:

(require 'majutsu)
(require 'majutsu-graph-record)
(require 'subr-x)

(declare-function majutsu-jj-wash "majutsu-jj" (washer keep-error &rest args))

(defconst majutsu-evolog--record-name "evolog"
  "Graph record protocol name for evolog entries.")

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
  "Template fragment equivalent to jj's format_short_change_id_with_change_offset(commit).")

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
  "Template fragment equivalent to builtin_log_compact(commit), without final newline.")

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

(defconst majutsu-evolog--entry-template
  (majutsu-graph-record-template
   majutsu-evolog--record-name
   '([:commit :change_id]
     [:commit :commit_id]
     [:if [:operation] [:operation :id] ""])
   'CommitEvolutionEntry
   majutsu-evolog--entry-display-template)
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

(defun majutsu-evolog--wash-record ()
  "Wash one graph-record wrapped evolog entry at point."
  (when-let* ((record (majutsu-graph-record-parse-at-point
                       majutsu-evolog--record-name)))
    (let ((beg (plist-get record :beg))
          (end (plist-get record :end))
          (display (plist-get record :display)))
      (delete-region beg end)
      (goto-char beg)
      (unless (string-empty-p display)
        (insert display)
        (unless (string-suffix-p "\n" display)
          (insert "\n")))
      t)))

(defun majutsu-evolog--wash-output (_args)
  "Wash raw `jj evolog` output in the current narrowed region."
  (goto-char (point-min))
  (while (not (eobp))
    (unless (majutsu-evolog--wash-record)
      (forward-line 1))))

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
  (magit-insert-section (evologbuf)
    (majutsu-evolog--insert-entries)))

(defvar-keymap majutsu-evolog-mode-map
  :doc "Keymap for `majutsu-evolog-mode'."
  :parent majutsu-mode-map)

(define-derived-mode majutsu-evolog-mode majutsu-mode "Majutsu Evolog"
  "Major mode for viewing jj change evolution history."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

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
