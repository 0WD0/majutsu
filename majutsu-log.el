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
(defclass majutsu-status-section (magit-section) ())
(defclass majutsu-conflict-section (magit-section) ())
(defclass majutsu-conflict-file-section (magit-section)
  ((file :initarg :file)))

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
        [:separate " "
                   [:if [:current_working_copy] "working_copy"]
                   [:if [:immutable] "immutable" "mutable"]
                   [:if [:conflict] "conflicted"]]
        [:separate "\x1e"
                   [:call 'format_short_change_id_with_hidden_and_divergent_info [:raw "self" :Commit]]
                   [:call 'format_short_signature_oneline [:author]]
                   [:separate " "
                              [:bookmarks]
                              [:tags]
                              [:working_copies]]
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

(defvar-local majutsu-log--cached-entries nil
  "Cached log entries for the current buffer.")

(defun majutsu-parse-log-entries (&optional buf log-output)
  "Parse jj log output from BUF (defaults to `current-buffer').
If LOG-OUTPUT is provided, parse it.
If `majutsu-log--cached-entries' is set, return it.
Otherwise, run the command in BUF (or current buffer) to get output.

The parser keeps graph spacer lines (blank or connector rows) with the
preceding revision so `magit-section-forward' can jump between commits
instead of stopping on visual padding."
  (if (and majutsu-log--cached-entries (not log-output))
      majutsu-log--cached-entries
    (with-current-buffer (or buf (current-buffer))
      (let* ((args (majutsu-log--build-args))
             (output (or log-output (apply #'majutsu-run-jj args))))
        (when (and output (not (string-empty-p output)))
          (let ((lines (split-string output "\n"))
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
            (nreverse entries)))))))

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

;;; Log insert status

(defun majutsu--analyze-status-for-hints (status-output)
  "Analyze jj status output and provide helpful hints."
  (when (and status-output (not (string-empty-p status-output)))
    (cond
     ;; No changes
     ((string-match-p "The working copy is clean" status-output)
      (message "Working copy is clean - no changes to commit"))

     ;; Conflicts present
     ((string-match-p "There are unresolved conflicts" status-output)
      (message "💡 Resolve conflicts with 'jj resolve' or use diffedit (E/M)"))

     ;; Untracked files
     ((string-match-p "Untracked paths:" status-output)
      (message "💡 Add files with 'jj file track' or create .gitignore"))

     ;; Working copy changes
     ((string-match-p "Working copy changes:" status-output)
      (message "💡 Commit changes with 'jj commit' or describe with 'jj describe'")))))

(defun majutsu-log-insert-status ()
  "Insert jj status into current buffer."
  (let ((status-output (majutsu-run-jj "status")))
    (when (and status-output (not (string-empty-p status-output)))
      (magit-insert-section (majutsu-status-section)
        (magit-insert-heading "Working Copy Status")
        (insert status-output)
        (insert "\n")
        ;; Analyze status and provide hints in the minibuffer
        (majutsu--analyze-status-for-hints status-output)))))

;;; Log insert diff

(defun majutsu-log-insert-diff ()
  "Insert jj diff with hunks into current buffer asynchronously."
  (let* ((section (magit-insert-section (majutsu-diff-section)
                    (magit-insert-heading "Working Copy Changes")
                    (insert "Loading diffs...\n")))
         (buf (current-buffer)))
    (majutsu-run-jj-async
     '("diff" "--git")
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t)
                 (magit-insert-section--parent section))
             (save-excursion
               (goto-char (oref section content))
               (delete-region (point) (oref section end))
               (if (and output (not (string-empty-p output)))
                   (progn
                     (majutsu--insert-diff-hunks output)
                     (insert "\n"))
                 (insert "No changes.\n"))
               ;; Update section end marker
               (set-marker (oref section end) (point)))))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t)
                 (magit-insert-section--parent section))
             (save-excursion
               (goto-char (oref section content))
               (delete-region (point) (oref section end))
               (insert (format "Error loading diffs: %s\n" err))
               (set-marker (oref section end) (point))))))))))

;;; Log insert conflicts

(defun majutsu-log-insert-conflicts ()
  "Insert conflicted files section."
  (let ((output (majutsu-run-jj "resolve" "--list")))
    (when (and output (not (string-empty-p output)))
      (magit-insert-section (majutsu-conflict-section)
        (magit-insert-heading "Unresolved Conflicts")
        (dolist (line (split-string output "\n" t))
          (let ((file (string-trim line)))
            (magit-insert-section (majutsu-conflict-file-section file nil :file file)
              (magit-insert-heading (propertize file 'face 'error))
              (insert "\n"))))
        (insert "\n")))))

;;; Log Navigation

(defconst majutsu--show-change-id-template
  (tpl-compile [:change_id :shortest 8]))

(defconst majutsu--show-commit-id-template
  (tpl-compile [:commit_id :shortest 8]))

(defun majutsu-current-change-id ()
  (majutsu-run-jj "log" "--no-graph" "-r" "@" "-T" majutsu--show-change-id-template))

(defun majutsu-current-commit-id ()
  (majutsu-run-jj "log" "--no-graph" "-r" "@" "-T" majutsu--show-commit-id-template))

(defun majutsu-log-goto-@ ()
  "Jump to the current changeset (@)."
  (interactive)
  (majutsu--goto-log-entry (majutsu-current-change-id)
                           (majutsu-current-commit-id)))

(defun majutsu-goto-commit (commit-id)
  "Jump to a specific COMMIT-ID in the log."
  (interactive "sCommit ID: ")
  (let ((start-pos (point)))
    (goto-char (point-min))
    (if (re-search-forward (regexp-quote commit-id) nil t)
        (goto-char (line-beginning-position))
      (goto-char start-pos)
      (message "Commit %s not found" commit-id))))

(defun majutsu-goto-change (change-id)
  "Jump to a specific CHANGE-ID in the log."
  (interactive "sChange ID: ")
  (let ((start-pos (point)))
    (goto-char (point-min))
    (if (re-search-forward (regexp-quote change-id) nil t)
        (goto-char (line-beginning-position))
      (goto-char start-pos)
      (message "Change %s not found" change-id))))

(defun majutsu--goto-log-entry (change-id &optional commit-id)
  "Move point to the log entry section matching CHANGE-ID.
When CHANGE-ID is nil, fall back to COMMIT-ID.
Return non-nil when the section could be located."
  (when-let* ((section (majutsu--find-log-entry-section change-id commit-id)))
    (magit-section-goto section)
    (goto-char (oref section start))
    t))

;;;###autoload
(defun majutsu-goto-next-changeset ()
  "Navigate to the next changeset in the log."
  (interactive)
  (let ((pos (point))
        found)
    (while (and (not found)
                (< (point) (point-max)))
      (magit-section-forward)
      (when-let* ((section (magit-current-section)))
        (when (and (eq (oref section type) 'majutsu-log-entry-section)
                   (> (point) pos))
          (setq found t))))
    (unless found
      (goto-char pos)
      (message "No more changesets"))))

;;;###autoload
(defun majutsu-goto-prev-changeset ()
  "Navigate to the previous changeset in the log."
  (interactive)
  (let ((pos (point))
        found)
    (while (and (not found)
                (> (point) (point-min)))
      (magit-section-backward)
      (when-let* ((section (magit-current-section)))
        (when (and (eq (oref section type) 'majutsu-log-entry-section)
                   (< (point) pos))
          (setq found t))))
    (unless found
      (goto-char pos)
      (message "No more changesets"))))

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

(defvar-keymap majutsu-log-mode-map
  :doc "Keymap for `majutsu-log-mode'."
  :parent majutsu-mode-map
  "n" 'majutsu-goto-next-changeset
  "p" 'majutsu-goto-prev-changeset)

(define-derived-mode majutsu-log-mode majutsu-mode "Majutsu Log"
  "Major mode for interacting with jj version control system."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer)
  ;; Clear rebase selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-rebase-clear-selections nil t)
  ;; Clear new selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-new-clear-selections nil t)
  ;; Clear squash selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-squash-clear-selections nil t)
  ;; Clear duplicate selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-duplicate-clear-selections nil t))

(defun majutsu-log-render ()
  "Render the log buffer using cached data."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (magit-root-section)
      (run-hooks 'majutsu-log-sections-hook))))

(defun majutsu-log--refresh-buffer (target-change target-commit)
  "Implementation helper to refresh the current log buffer.
Assumes `current-buffer' is a `majutsu-log-mode' buffer."
  (majutsu--assert-mode 'majutsu-log-mode)
  (let* ((root (majutsu--root))
         (buf (current-buffer))
         (target-change (or target-change (majutsu-log--change-id-at-point)))
         (target-commit (or target-commit (majutsu-log--commit-id-at-point))))
    (setq-local majutsu--repo-root root)
    (setq default-directory root)
    (setq majutsu-log--cached-entries nil)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Loading..."))
    (majutsu-run-jj-async
     (majutsu-log--build-args)
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (derived-mode-p 'majutsu-log-mode)
             (setq majutsu-log--cached-entries (majutsu-parse-log-entries nil output))
             (majutsu-log-render)
             (unless (when target-change (majutsu--goto-log-entry target-change target-commit))
               (majutsu-log-goto-@))))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (derived-mode-p 'majutsu-log-mode)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert "Error: " err)))))))))

(defun majutsu-log-refresh (&optional target-change target-commit _ignore-auto _noconfirm)
  "Refresh a majutsu log buffer for the current repository.
When called outside a log buffer, try to refresh an existing log
buffer for the same repository.  If none exists and the command
is invoked interactively, signal a user error instead of
mutating the wrong buffer."
  (interactive)
  (let* ((root (majutsu--buffer-root))
         (buffer (majutsu--resolve-mode-buffer 'majutsu-log-mode root)))
    (cond
     (buffer
      (with-current-buffer buffer
        (majutsu-log--refresh-buffer target-change target-commit)))
     ((called-interactively-p 'interactive)
      (user-error "No Majutsu log buffer for this repository; open one with `majutsu-log`"))
     (t
      (majutsu--debug "Skipping log refresh: no log buffer for %s" (or root "unknown repo"))))))

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

;;; Operation Log

(defclass majutsu-op-log-entry-section (magit-section)
  ((op-id :initarg :op-id)
   (user :initarg :user)
   (time :initarg :time)
   (description :initarg :description)))

(defconst majutsu--op-log-template
  (tpl-compile
   [:separate "\x1e"
              [:call 'id.short]
              [:user]
              [:method [:call 'time.end] :format "%Y-%m-%d %H:%M"]
              [:description]
              "\n"]
   'Operation))

(defvar-local majutsu-op-log--cached-entries nil
  "Cached operation log entries.")

(defun majutsu-parse-op-log-entries (&optional buf log-output)
  "Parse jj op log output."
  (if (and majutsu-op-log--cached-entries (not log-output))
      majutsu-op-log--cached-entries
    (with-current-buffer (or buf (current-buffer))
      (let* ((args (list "op" "log" "--no-graph" "-T" majutsu--op-log-template))
             (output (or log-output (apply #'majutsu-run-jj args))))
        (when (and output (not (string-empty-p output)))
          (let ((lines (split-string output "\n" t))
                (entries '()))
            (dolist (line lines)
              (seq-let (id user time desc) (split-string line "\x1e")
                (push (list :id id :user user :time time :desc desc) entries)))
            (nreverse entries)))))))

(defun majutsu-op-log-insert-entries ()
  "Insert operation log entries."
  (magit-insert-section (majutsu-log-graph-section)
    (magit-insert-heading "Operation Log")
    (dolist (entry (majutsu-parse-op-log-entries))
      (magit-insert-section section (majutsu-op-log-entry-section entry t)
                            (oset section op-id (plist-get entry :id))
                            (magit-insert-heading
                              (format "%-12s %-15s %-16s %s"
                                      (propertize (plist-get entry :id) 'face 'font-lock-constant-face)
                                      (propertize (plist-get entry :user) 'face 'font-lock-variable-name-face)
                                      (propertize (plist-get entry :time) 'face 'font-lock-comment-face)
                                      (plist-get entry :desc)))
                            (insert "\n")))))

(defun majutsu-op-log-render ()
  "Render the op log buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (magit-root-section)
      (majutsu-op-log-insert-entries))))

(defun majutsu-op-log-refresh ()
  "Refresh the op log buffer."
  (interactive)
  (majutsu--assert-mode 'majutsu-op-log-mode)
  (let ((root (majutsu--root))
        (buf (current-buffer)))
    (setq-local majutsu--repo-root root)
    (setq default-directory root)
    (setq majutsu-op-log--cached-entries nil)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Loading..."))
    (majutsu-run-jj-async
     (list "op" "log" "--no-graph" "-T" majutsu--op-log-template)
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (derived-mode-p 'majutsu-op-log-mode)
             (setq majutsu-op-log--cached-entries (majutsu-parse-op-log-entries nil output))
             (majutsu-op-log-render)))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (derived-mode-p 'majutsu-op-log-mode)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert "Error: " err)))))))))

(defvar-keymap majutsu-op-log-mode-map
  :doc "Keymap for `majutsu-op-log-mode'."
  :parent majutsu-mode-map)

(define-derived-mode majutsu-op-log-mode majutsu-mode "Majutsu Op Log"
  "Major mode for viewing jj operation log."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

(defun majutsu-op-log ()
  "Open the majutsu operation log."
  (interactive)
  (let* ((root (majutsu--root))
         (buffer (get-buffer-create (format "*majutsu-op: %s*" (file-name-nondirectory (directory-file-name root))))))
    (with-current-buffer buffer
      (majutsu-op-log-mode)
      (setq-local majutsu--repo-root root)
      (setq default-directory root)
      (majutsu-op-log-refresh))
    (majutsu--display-buffer-for-editor buffer)))
(provide 'majutsu-log)
;;; majutsu-log.el ends here
