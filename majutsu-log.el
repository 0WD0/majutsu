;;; majutsu-log.el --- Log view for majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Brandon Olivier
;; Copyright (C) 2025 0WD0

;; Author: Brandon Olivier
;;         0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Log view, graph rendering, and navigation for Majutsu.

;;; Code:

(require 'majutsu-core)
(require 'majutsu-process)
(require 'majutsu-template)
(require 'magit-section)
(require 'transient)
(require 'json)

;;; Classes

(defclass majutsu-commits-section (magit-section) ())
(defclass majutsu-log-graph-section (magit-section) ())
(defclass majutsu-log-entry-section (magit-section)
  ((commit-id :initarg :commit-id)
   (change-id :initarg :change-id)
   (description :initarg :description)
   (bookmarks :initarg :bookmarks)))

(defun majutsu--section-change-id (section)
  "Return the change id recorded in SECTION, if available."
  (when (and section (object-of-class-p section 'majutsu-log-entry-section))
    (or (when (and (slot-exists-p section 'change-id)
                   (slot-boundp section 'change-id))
          (majutsu--normalize-id-value (oref section change-id)))
        (let ((entry (oref section value)))
          (when (listp entry)
            (majutsu--normalize-id-value
             (or (plist-get entry :change-id)
                 (plist-get entry :id))))))))

(defun majutsu--section-commit-id (section)
  "Return the commit id recorded in SECTION, if available."
  (when section
    (when (and (slot-exists-p section 'commit-id)
               (slot-boundp section 'commit-id))
      (majutsu--normalize-id-value (oref section commit-id)))))

(cl-defmethod magit-section-ident-value ((section majutsu-log-entry-section))
  "Identify log entry sections by their change id."
  (or (majutsu--section-change-id section)
      (majutsu--section-commit-id section)
      (let ((entry (oref section value)))
        (when (listp entry)
          (or (plist-get entry :change-id)
              (plist-get entry :commit_id)
              (plist-get entry :id))))))

;;; Log State

(defconst majutsu-log--state-template
  '(:revisions nil
    :limit nil
    :reversed nil
    :no-graph nil
    :filesets nil)
  "Default plist template describing log view options.")

(defvar majutsu-log-state (copy-sequence majutsu-log--state-template)
  "Plist capturing the current jj log view options.")

(defcustom majutsu-log-sections-hook '(majutsu-log-insert-logs
                                       majutsu-log-insert-status
                                       majutsu-log-insert-diff)
  "Hook run to insert sections in the log buffer."
  :type 'hook
  :group 'majutsu)

(defun majutsu-log--reset-state ()
  "Reset log view options to defaults."
  (setq majutsu-log-state (copy-sequence majutsu-log--state-template)))

(defun majutsu-log--state-get (key)
  "Return value for KEY in `majutsu-log-state'."
  (plist-get majutsu-log-state key))

(defun majutsu-log--state-set (key value)
  "Set KEY in `majutsu-log-state' to VALUE."
  (setq majutsu-log-state (plist-put majutsu-log-state key value)))

(defun majutsu-log--summary-parts ()
  "Return a list of human-readable fragments describing current log state."
  (let ((parts '()))
    (when-let* ((rev (majutsu-log--state-get :revisions)))
      (push (format "rev=%s" rev) parts))
    (when-let* ((limit (majutsu-log--state-get :limit)))
      (push (format "limit=%s" limit) parts))
    (when (majutsu-log--state-get :reversed)
      (push "reversed" parts))
    (when (majutsu-log--state-get :no-graph)
      (push "no-graph" parts))
    (let ((paths (majutsu-log--state-get :filesets)))
      (when paths
        (push (if (= (length paths) 1)
                  (format "path=%s" (car paths))
                (format "paths=%d" (length paths)))
              parts)))
    (nreverse parts)))

(defun majutsu-log--format-summary (prefix)
  "Return PREFIX annotated with active log state summary."
  (let ((parts (majutsu-log--summary-parts)))
    (if parts
        (format "%s (%s)" prefix (string-join parts ", "))
      prefix)))

(defun majutsu-log--heading-string ()
  "Return heading string for the log section."
  (majutsu-log--format-summary "Log Graph"))

(defun majutsu-log--transient-description ()
  "Return description string for the log transient."
  (majutsu-log--format-summary "JJ Log"))

;;; Log Template

(defconst majutsu--log-template
  (tpl-compile
   ["\x1e"
    [:if [:root]
        [:separate "\x1e"
                   [:call 'format_short_change_id [:change_id]]
                   " "
                   [" " [:bookmarks] [:tags] [:working_copies]]
                   " "
                   " "
                   " "
                   [:label "root" "root()"]
                   "root"
                   [:call 'format_short_commit_id [:commit_id]]
                   " "
                   [:json " "]]
      [[:label
        [:separate "\x1e"
                   [:if [:current_working_copy] "working_copy"]
                   [:if [:immutable] "immutable" "mutable"]
                   [:if [:conflict] "conflicted"]]
        [:separate "\x1e"
                   [:call 'format_short_change_id_with_hidden_and_divergent_info [:raw "self" :Commit]]
                   [:call 'format_short_signature_oneline [:author]]
                   [" " [:bookmarks] [:tags] [:working_copies]]
                   [:if [:git_head]
                       [:label "git_head" "git_head()"]
                     " "]
                   [:if [:conflict]
                       [:label "conflict" "conflict"]
                     " "]
                   [:if [:method [:call 'config "ui.show-cryptographic-signatures"] :as_boolean]
                       [:call 'format_short_cryptographic_signature [:signature]]
                     " "]
                   [:if [:empty]
                       [:label "empty" "(empty)"]
                     " "]
                   [:if [:description]
                       [:method [:description] :first_line]
                     [:label
                      [:if [:empty] "empty"]
                      'description_placeholder]]
                   [:call 'format_short_commit_id [:commit_id]]
                   [:call 'format_timestamp
                          [:call 'commit_timestamp [:raw "self" :Commit]]]
                   [:if [:description]
                       [:json [:description]]
                     [:json " "]]]]
       "\n"]]])
  "Template for formatting log entries.

The trailing newline keeps each entry on its own line even when
`jj log' is invoked with `--no-graph'.")

(defun majutsu-log--build-args ()
  "Build argument list for `jj log' using current state."
  (let ((args '("log")))
    (when-let* ((rev (majutsu-log--state-get :revisions)))
      (setq args (append args (list "-r" rev))))
    (when-let* ((limit (majutsu-log--state-get :limit)))
      (setq args (append args (list "-n" limit))))
    (when (majutsu-log--state-get :reversed)
      (setq args (append args '("--reversed"))))
    (when (majutsu-log--state-get :no-graph)
      (setq args (append args '("--no-graph"))))
    (setq args (append args (list "-T" majutsu--log-template)))
    (setq args (append args (majutsu-log--state-get :filesets)))
    args))

;;; Log Parsing

(defun majutsu-parse-log-entries (&optional buf)
  "Parse jj log output from BUF (defaults to `current-buffer').

The parser keeps graph spacer lines (blank or connector rows) with the
preceding revision so `magit-section-forward' can jump between commits
instead of stopping on visual padding."
  (with-current-buffer (or buf (current-buffer))
    (let* ((args (majutsu-log--build-args))
           (log-output (apply #'majutsu--run-command-color args)))
      (when (and log-output (not (string-empty-p log-output)))
        (let ((lines (split-string log-output "\n"))
              (entries '())
              (current nil)
              (pending nil))
          (dolist (line lines)
            (let* ((raw-elems (split-string line "\x1e"))
                   (trimmed-elems (mapcar #'string-trim-right raw-elems))
                   (clean-elems (seq-remove (lambda (l) (or (not l) (string-blank-p l)))
                                            trimmed-elems)))
              (if (> (length clean-elems) 1)
                  (progn
                    (when current
                      (when pending
                        (setq current (plist-put current :suffix-lines (nreverse pending)))
                        (setq pending nil))
                      (push current entries))
                    (setq current
                          (seq-let (prefix change-id author bookmarks _git-head _conflict _signature _empty short-desc commit-id timestamp long-desc)
                              trimmed-elems
                            (let* ((cid (if (stringp change-id) (substring-no-properties change-id) ""))
                                   (full (if (stringp commit-id) (substring-no-properties commit-id) ""))
                                   (id8  (if (> (length cid) 8) (substring cid 0 8) cid))
                                   (idv  (unless (string-empty-p id8) id8)))
                              (list :id idv
                                    :prefix prefix
                                    :line line
                                    :elems clean-elems
                                    :author author
                                    :change-id cid
                                    :commit_id full
                                    :short-desc short-desc
                                    :long-desc (when long-desc (json-parse-string long-desc))
                                    :timestamp timestamp
                                    :bookmarks bookmarks))))
                    (setq pending nil))
                (push line pending))))
          (when current
            (when pending
              (setq current (plist-put current :suffix-lines (nreverse pending))))
            (push current entries))
          (nreverse entries))))))

(defun majutsu--indent-string (s column)
  "Insert STRING into the current buffer, indenting each line to COLUMN."
  (let ((indentation (make-string column ?\s))) ; Create a string of spaces for indentation
    (mapconcat (lambda (line)
                 (concat indentation line))
               (split-string s "\n")
               "\n"))) ; Join lines with newline, prefixed by indentation

(defun majutsu-log-insert-logs ()
  "Insert jj log graph into current buffer."
  (magit-insert-section (majutsu-log-graph-section)
    (magit-insert-heading (majutsu-log--heading-string))
    (dolist (entry (majutsu-parse-log-entries))
      (magit-insert-section section (majutsu-log-entry-section entry t)
                            (oset section commit-id (or (plist-get entry :commit_id)
                                                        (plist-get entry :id)))
                            (oset section change-id (or (plist-get entry :change-id)
                                                        (plist-get entry :id)))
                            (oset section description (plist-get entry :short-desc))
                            (oset section bookmarks (plist-get entry :bookmarks))
                            (magit-insert-heading
                              (insert (string-join (butlast (plist-get entry :elems)) " ")))
                            (when-let* ((long-desc (plist-get entry :long-desc))
                                        (indented (majutsu--indent-string long-desc
                                                                          (+ 10 (length (plist-get entry :prefix))))))
                              (magit-insert-section-body
                                (insert indented)
                                (insert "\n")))
                            (when-let* ((suffix-lines (plist-get entry :suffix-lines)))
                              (dolist (suffix-line suffix-lines)
                                (insert suffix-line)
                                (insert "\n")))))
    (insert "\n")))

;;; Log Navigation

(defun majutsu--find-log-entry-section (change-id commit-id)
  "Return the log entry section matching CHANGE-ID or COMMIT-ID, or nil."
  (when magit-root-section
    (let (found)
      (cl-labels ((walk (section)
                    (when section
                      (when (and (object-of-class-p section 'majutsu-log-entry-section)
                                 (or (and change-id
                                          (let ((section-change (majutsu--section-change-id section)))
                                            (and section-change
                                                 (equal section-change change-id))))
                                     (and commit-id
                                          (slot-boundp section 'commit-id)
                                          (equal (oref section commit-id) commit-id))))
                        (setq found section))
                      (dolist (child (oref section children))
                        (unless found
                          (walk child))))))
        (walk magit-root-section))
      found)))

(defun majutsu-log--commit-only-at-point ()
  "Return the raw commit id at point, or nil if unavailable."
  (when-let* ((section (magit-current-section)))
    (majutsu--section-commit-id section)))

(defun majutsu-log--ids-at-point ()
  "Return a plist (:change .. :commit .. :section ..) describing ids at point."
  (when-let* ((section (magit-current-section)))
    (let ((change (majutsu--section-change-id section))
          (commit (majutsu--section-commit-id section)))
      (when (or change commit)
        (list :change change :commit commit :section section)))))

(defun majutsu-log--revset-at-point ()
  "Return the preferred revset (change id if possible) at point."
  (when-let* ((ids (majutsu-log--ids-at-point)))
    (let ((change (plist-get ids :change))
          (commit (plist-get ids :commit)))
      (if (and change (string-suffix-p "?" change))
          (or commit change)
        (or change commit)))))

(defun majutsu-log--change-id-at-point ()
  "Return change id for the log entry at point, or nil otherwise."
  (majutsu--section-change-id (magit-current-section)))

(defun majutsu-log--commit-id-at-point ()
  "Get the changeset ID at point as a plain string (no text properties)."
  (or (majutsu-log--commit-only-at-point)
      (majutsu-log--change-id-at-point)))

;;; Log Mode

(define-derived-mode majutsu-log-mode magit-section-mode "Majutsu Log"
  "Major mode for interacting with jj version control system."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function 'majutsu-log-refresh)
  ;; Clear rebase selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-rebase-clear-selections nil t)
  ;; Clear new selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-new-clear-selections nil t)
  ;; Clear squash selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-squash-clear-selections nil t)
  ;; Clear duplicate selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-duplicate-clear-selections nil t))

(defun majutsu-log-refresh ()
  "Refresh the current majutsu log buffer."
  (interactive)
  (let ((root (majutsu--root)))
    (setq-local majutsu--repo-root root)
    (setq default-directory root)
    (magit-insert-section (magit-root-section)
      (run-hooks 'majutsu-log-sections-hook))))

(defun majutsu-log ()
  "Open the majutsu log buffer."
  (interactive)
  (let* ((root (majutsu--root))
         (buffer (get-buffer-create (format "*majutsu: %s*" (file-name-nondirectory (directory-file-name root))))))
    (with-current-buffer buffer
      (majutsu-log-mode)
      (setq-local majutsu--repo-root root)
      (setq default-directory root)
      (majutsu-log-refresh))
    (majutsu--display-buffer-for-editor buffer)))

(defun majutsu-log--refresh-view ()
  "Refresh current log buffer or open a new one."
  (if (derived-mode-p 'majutsu-log-mode)
      (majutsu-log-refresh)
    (majutsu-log)))

(provide 'majutsu-log)
;;; majutsu-log.el ends here
