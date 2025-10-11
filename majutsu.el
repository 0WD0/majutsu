;;; majutsu.el -*- lexical-binding: t; -*-

(require 'magit)
(require 'magit-section)
(require 'transient)
(require 'ansi-color)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup majutsu nil
  "Interface to jj version control system."
  :group 'tools)

(defcustom majutsu-executable "jj"
  "Path to jj executable."
  :type 'string
  :group 'majutsu)

(defcustom majutsu-debug nil
  "Enable debug logging for jj operations."
  :type 'boolean
  :group 'majutsu)

(defcustom majutsu-show-command-output t
  "Show jj command output in messages."
  :type 'boolean
  :group 'majutsu)

(defcustom majutsu-log-sections-hook '(majutsu-log-insert-logs
                                       majutsu-log-insert-status
                                       majutsu-log-insert-diff)
  "Hook run to insert sections in the log buffer."
  :type 'hook
  :group 'majutsu)

(defcustom majutsu-log-display-function #'pop-to-buffer
  "Function called to display the majutsu log buffer.
The function must accept one argument: the buffer to display."
  :type '(choice
          (function-item switch-to-buffer)
          (function-item pop-to-buffer)
          (function-item display-buffer)
          (function :tag "Custom function"))
  :group 'majutsu)

(defvar majutsu-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (define-key map (kbd "n") 'magit-section-forward)
    (define-key map (kbd "p") 'magit-section-backward)
    (define-key map (kbd "M-n") 'magit-section-forward-sibling)
    (define-key map (kbd "M-p") 'magit-section-backward-sibling)
    (define-key map (kbd ".") 'majutsu-goto-current)
    (define-key map (kbd "TAB") 'magit-section-toggle)
    (define-key map (kbd "q") 'quit-window)

    ;; Basic operations
    (define-key map (kbd "g") 'majutsu-log-refresh)
    (define-key map (kbd "c") 'majutsu-commit)
    (define-key map (kbd "e") 'majutsu-edit-changeset)
    (define-key map (kbd "u") 'majutsu-undo)
    (define-key map (kbd "R") 'majutsu-redo)
    (define-key map (kbd "N") 'majutsu-new)
    (define-key map (kbd "s") 'majutsu-squash-transient)
    (define-key map (kbd "c") 'majutsu-commit)
    (define-key map (kbd "d") 'majutsu-describe)
    (define-key map (kbd "a") 'majutsu-abandon)

    ;; Advanced Operations
    (define-key map (kbd "RET") 'majutsu-enter-dwim)
    (define-key map (kbd "b") 'majutsu-bookmark-transient)
    (define-key map (kbd "r") 'majutsu-rebase-transient)
    (define-key map (kbd "G") 'majutsu-git-transient)

    ;; Experimental
    (define-key map (kbd "D") 'majutsu-diff)
    (define-key map (kbd "E") 'majutsu-diffedit-emacs)
    (define-key map (kbd "M") 'majutsu-diffedit-smerge)
    (define-key map (kbd "?") 'majutsu-mode-transient)
    map)
  "Keymap for `majutsu-mode'.")

;;;###autoload
(transient-define-prefix majutsu-mode-transient ()
  "JJ commands transient menu."
  [:description "JJ Commands" :class transient-columns
                ["Basic Operations"
                 ("g" "Refresh log" majutsu-log-refresh)
                 ("c" "Commit" majutsu-commit)
                 ("e" "Edit changeset" majutsu-edit-changeset)
                 ("u" "Undo last change" majutsu-undo)
                 ("R" "Redo last change" majutsu-redo)
                 ("N" "New changeset" majutsu-new)
                 ("a" "Abandon changeset" majutsu-abandon)
                 ("d" "Describe changeset" majutsu-describe)
                 ("s" "Squash changeset" majutsu-squash-transient)]
                ["Advanced Operations"
                 ("r" "Rebase changeset" majutsu-rebase-transient)
                 ("b" "Bookmark operations" majutsu-bookmark-transient)
                 ("G" "Git operations" majutsu-git-transient)]
                ["Experimental"
                 ("D" "Show diff" majutsu-diff)
                 ("E" "DiffEdit (ediff)" majutsu-diffedit-emacs)
                 ("M" "DiffEdit (smerge)" majutsu-diffedit-smerge)]
                ["Exit"
                 ("?" "Show cool help" transient-help)
                 ("q" "Quit transient" transient-quit-one)]])

(define-derived-mode majutsu-mode magit-section-mode "Majutsu"
  "Major mode for interacting with jj version control system."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function 'majutsu-log-refresh)
  ;; Clear rebase selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-rebase-clear-selections nil t)
  ;; Clear squash selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-squash-clear-selections nil t))

(defvar-local majutsu--repo-root nil
  "Cached repository root for the current buffer.")

(defconst majutsu--log-template
  "'\x1e' ++
if(self.root(),
  format_root_commit(self),
  label(
    separate('\x1e',
      if(self.current_working_copy(), 'working_copy'),
      if(self.immutable(), 'immutable', 'mutable'),
      if(self.conflict(), 'conflicted'),
    ),
    concat(
      separate('\x1e',
        format_short_change_id_with_hidden_and_divergent_info(self),
        format_short_signature_oneline(self.author()),
        concat(' ', self.bookmarks(), self.tags(), self.working_copies()),
        if(self.git_head(), label('git_head', 'git_head()'), ' '),
        if(self.conflict(), label('conflict', 'conflict'), ' '),
        if(config('ui.show-cryptographic-signatures').as_boolean(),
          format_short_cryptographic_signature(self.signature()),
          ' '),
        if(self.empty(), label('empty', '(empty)'), ' '),
        if(self.description(),
          self.description().first_line(),
          label(if(self.empty(), 'empty'), description_placeholder),
        ),
        format_short_commit_id(self.commit_id()),
        format_timestamp(commit_timestamp(self)),
        if(self.description(), json(self.description()), json(' ')),
      ),
    ),
  )
)
"
  "Template for formatting log entries.")

(defun majutsu--root ()
  "Find root of the current repository."
  (let ((root (or (and (boundp 'majutsu--repo-root) majutsu--repo-root)
                  (locate-dominating-file default-directory ".jj"))))
    (unless root
      (user-error "Cannot find root -- not in a JJ repo"))
    root))

(defun majutsu--debug (format-string &rest args)
  "Log debug message if majutsu-debug is enabled."
  (when majutsu-debug
    (message "[majutsu-mode] %s" (apply #'format format-string args))))

(defun majutsu--message-with-log (format-string &rest args)
  "Display message and log if debug enabled."
  (let ((msg (apply #'format format-string args)))
    (majutsu--debug "User message: %s" msg)
    (message "%s" msg)))

(defun majutsu--run-command (&rest args)
  "Run jj command with ARGS and return output."
  (let ((start-time (current-time))
        (safe-args (seq-remove #'null args))
        result exit-code)
    (majutsu--debug "Running command: %s %s" majutsu-executable (string-join safe-args " "))
    (with-temp-buffer
      (setq exit-code (apply #'process-file majutsu-executable nil t nil safe-args))
      (setq result (buffer-string))
      (majutsu--debug "Command completed in %.3f seconds, exit code: %d"
                      (float-time (time-subtract (current-time) start-time))
                      exit-code)
      (when (and majutsu-show-command-output (not (string-empty-p result)))
        (majutsu--debug "Command output: %s" (string-trim result)))
      result)))

(defun majutsu--run-command-color (&rest args)
  "Run jj command with ARGS and return colorized output."
  (majutsu--debug "Running color command: %s --color=always %s" majutsu-executable (string-join args " "))
  (let ((start-time (current-time))
        result exit-code)
    (with-temp-buffer
      (let ((process-environment (cons "FORCE_COLOR=1" (cons "CLICOLOR_FORCE=1" process-environment))))
        (setq exit-code (apply #'process-file majutsu-executable nil t nil "--color=always" args))
        (setq result (ansi-color-apply (buffer-string)))
        (majutsu--debug "Color command completed in %.3f seconds, exit code: %d"
                        (float-time (time-subtract (current-time) start-time))
                        exit-code)
        result))))

(defun majutsu--run-command-async (callback &rest args)
  "Run jj command with ARGS asynchronously and call CALLBACK with output."
  (majutsu--debug "Starting async command: %s %s" majutsu-executable (string-join args " "))
  (let ((buffer (generate-new-buffer " *majutsu-async*"))
        (start-time (current-time)))
    (set-process-sentinel
     (apply #'start-file-process "jj" buffer majutsu-executable args)
     (lambda (process _event)
       (let ((exit-code (process-exit-status process)))
         (majutsu--debug "Async command completed in %.3f seconds, exit code: %d"
                         (float-time (time-subtract (current-time) start-time))
                         exit-code)
         (when (eq (process-status process) 'exit)
           (with-current-buffer (process-buffer process)
             (funcall callback (buffer-string)))
           (kill-buffer (process-buffer process))))))))

(defun majutsu--suggest-help (command-name error-msg)
  "Provide helpful suggestions when COMMAND-NAME fails with ERROR-MSG."
  (let ((suggestions
         (cond
          ((string-match-p "No such revision" error-msg)
           "Try refreshing the log (g) or check if the commit still exists.")
          ((string-match-p "Working copy is stale" error-msg)
           "Run 'jj workspace update-stale' to fix stale working copy.")
          ((string-match-p "Merge conflict" error-msg)
           "Resolve conflicts manually or use jj diffedit (E or M).")
          ((string-match-p "nothing to squash" error-msg)
           "Select a different commit that has changes to squash.")
          ((string-match-p "would create a loop" error-msg)
           "Check your rebase selections - source and destinations create a cycle.")
          ((string-match-p "No changes" error-msg)
           "No changes to commit. Make some changes first.")
          ((and (string= command-name "git")
                (or (string-match-p "Refusing to push" error-msg)
                    (string-match-p "would create new heads" error-msg)
                    (string-match-p "new bookmark" error-msg)))
           "Use --allow-new flag to push new bookmarks.")
          ((and (string= command-name "git") (string-match-p "authentication" error-msg))
           "Check your git credentials and remote repository access.")
          (t "Check 'jj help' or enable debug mode (M-x customize-variable majutsu-debug) for more info."))))
    (when suggestions
      (majutsu--message-with-log "💡 %s" suggestions))))

(defun majutsu--handle-command-result (command-args result &optional success-msg error-msg)
  "Handle command result with proper error checking and messaging."
  (let ((trimmed-result (string-trim result))
        (command-name (car command-args)))
    (majutsu--debug "Command result for '%s': %s"
                    (string-join command-args " ")
                    trimmed-result)

    ;; Always show command output if it exists (like CLI)
    (unless (string-empty-p trimmed-result)
      (message "%s" trimmed-result))

    (cond
     ;; Check for various error indicators
     ((or (string-match-p "^Error:\\|^error:" trimmed-result)
          (string-match-p "^Warning:\\|^warning:" trimmed-result)
          (string-match-p "^fatal:" trimmed-result))

      ;; Provide majutsu-specific contextual suggestions
      (cond
       ;; Working copy issues
       ((string-match-p "working copy is stale\\|concurrent modification" trimmed-result)
        (message "💡 Run 'jj workspace update-stale' to fix the working copy"))

       ;; Conflict resolution needed
       ((string-match-p "merge conflict\\|conflict in" trimmed-result)
        (message "💡 Resolve conflicts manually, then run 'jj resolve' or use diffedit (E/M)"))

       ;; Revision not found
       ((string-match-p "No such revision\\|revision.*not found" trimmed-result)
        (message "💡 Check the revision ID or refresh the log (g)"))

       ;; Empty commit issues
       ((string-match-p "nothing to squash\\|would be empty" trimmed-result)
        (message "💡 Select a different commit with actual changes"))

       ;; Rebase loop detection
       ((string-match-p "would create a loop\\|circular dependency" trimmed-result)
        (message "💡 Check your rebase source and destinations for cycles"))

       ;; Authentication/permission issues
       ((string-match-p "authentication\\|permission denied" trimmed-result)
        (message "💡 Check your git credentials and repository access"))

       ;; Generic suggestion for other errors
       (t
        (message "💡 Check 'jj help %s' for more information" command-name)))
      nil)

     ;; Success case
     (t
      (when (and success-msg (string-empty-p trimmed-result))
        (message "%s" success-msg))
      t))))

(defun majutsu--with-progress (message command-func)
  "Execute COMMAND-FUNC with minimal progress indication."
  (let ((start-time (current-time))
        result)
    (majutsu--debug "Starting operation: %s" message)
    (setq result (funcall command-func))
    (majutsu--debug "Operation completed in %.3f seconds"
                    (float-time (time-subtract (current-time) start-time)))
    result))

(defun majutsu--extract-bookmark-names (text)
  "Extract bookmark names from jj command output TEXT."
  (let ((names '())
        (start 0))
    (while (string-match "bookmark[: ]+\\([^ \n,]+\\)" text start)
      (push (match-string 1 text) names)
      (setq start (match-end 0)))
    (nreverse names)))


(defun majutsu--get-bookmark-names (&optional all-remotes)
  "Return bookmark names using --quiet to suppress hints.
When ALL-REMOTES is non-nil, include remote bookmarks formatted as NAME@REMOTE."
  (let* ((template (if all-remotes
                       "if(remote, name ++ '@' ++ remote ++ '\n', '')"
                     "name ++ '\n'"))
         (args (append '("bookmark" "list" "--quiet")
                       (and all-remotes '("--all"))
                       (list "-T" template))))
    (delete-dups (split-string (apply #'majutsu--run-command args) "\n" t))))

(defun majutsu--completion-table-with-category (candidates category)
  "Wrap CANDIDATES with completion METADATA to set CATEGORY.
This prevents third-party UIs (e.g., icons for `bookmark') from
misclassifying Majutsu candidates."
  (completion-table-with-metadata candidates
                                  `(metadata (category . ,category))))

(defun majutsu--handle-push-result (cmd-args result success-msg)
  "Enhanced push result handler with bookmark analysis."
  (let ((trimmed-result (string-trim result)))
    (majutsu--debug "Push result: %s" trimmed-result)

    ;; Always show the raw command output first (like CLI)
    (unless (string-empty-p trimmed-result)
      (message "%s" trimmed-result))

    (cond
     ;; Check for bookmark push restrictions
     ((or (string-match-p "Refusing to push" trimmed-result)
          (string-match-p "Refusing to create new remote bookmark" trimmed-result)
          (string-match-p "would create new heads" trimmed-result))
      ;; Extract bookmark names that couldn't be pushed
      (let ((bookmark-names (majutsu--extract-bookmark-names trimmed-result)))
        (if bookmark-names
            (message "💡 Use 'jj git push --allow-new' to push new bookmarks: %s"
                     (string-join bookmark-names ", "))
          (message "💡 Use 'jj git push --allow-new' to push new bookmarks")))
      nil)

     ;; Check for authentication issues
     ((string-match-p "Permission denied\\|authentication failed\\|403" trimmed-result)
      (message "💡 Check your git credentials and repository permissions")
      nil)

     ;; Check for network issues
     ((string-match-p "Could not resolve hostname\\|Connection refused\\|timeout" trimmed-result)
      (message "💡 Check your network connection and remote URL")
      nil)

     ;; Check for non-fast-forward issues
     ((string-match-p "non-fast-forward\\|rejected.*fetch first" trimmed-result)
      (message "💡 Run 'jj git fetch' first to update remote tracking")
      nil)

     ;; Analyze majutsu-specific push patterns and provide contextual help
     ((string-match-p "Nothing changed" trimmed-result)
      (message "💡 Nothing to push - all bookmarks are up to date")
      t)

     ;; General error check
     ((or (string-match-p "^error:" trimmed-result)
          (string-match-p "^fatal:" trimmed-result))
      nil)                              ; Error already shown above

     ;; Success case
     (t
      (when (string-empty-p trimmed-result)
        (message "%s" success-msg))
      t))))

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

(defclass majutsu-commit-section (magit-section)
  ((commit-id :initarg :commit-id)
   (author :initarg :author)
   (date :initarg :date)
   (description :initarg :description)
   (bookmarks :initarg :bookmarks)))

(defclass majutsu-commits-section (magit-section) ())
(defclass majutsu-status-section (magit-section) ())
(defclass majutsu-diff-stat-section (magit-section) ())
(defclass majutsu-log-graph-section (magit-section) ())
(defclass majutsu-log-entry-section (magit-section)
  ((commit-id :initarg :commit-id)
   (description :initarg :description)
   (bookmarks :initarg :bookmarks)))
(defclass majutsu-diff-section (magit-section) ())
(defclass majutsu-file-section (magit-section)
  ((file :initarg :file)))
(defclass majutsu-hunk-section (magit-section)
  ((file :initarg :file)
   (start :initarg :hunk-start)
   (header :initarg :header)))



(defun majutsu-parse-log-entries (&optional buf)
  "Get log line pairs from BUF (defaults to `current-buffer').

This somewhat naively runs log, splits on newlines, and partitions the
lines into pairs.

Each pair SHOULD be (line-with-changeset-id-and-email description-line).

The results of this fn are fed into `majutsu--parse-log-entries'."
  (with-current-buffer (or buf (current-buffer))
    (let ((log-output (majutsu--run-command-color "log" "-T" majutsu--log-template)))
      (when (and log-output (not (string-empty-p log-output)))
        (let ((lines (split-string log-output "\n" t)))
          (cl-loop for line in lines
                   for elems = (mapcar #'string-trim (split-string line "\x1e" ))
                   when (> (length elems) 1) collect
                   (seq-let (prefix change-id author bookmarks git-head conflict signature empty short-desc commit-id timestamp long-desc) elems
                     (let* ((cid (if (stringp change-id) (substring-no-properties change-id) ""))
                            (full (if (stringp commit-id) (substring-no-properties commit-id) ""))
                            (id8  (if (> (length cid) 8) (substring cid 0 8) cid))
                            (idv  (unless (string-empty-p id8) id8)))
                       (list :id idv
                             :prefix prefix
                             :line line
                             :elems (seq-remove (lambda (l) (or (not l) (string-blank-p l))) elems)
                             :author author
                             :commit_id full
                             :short-desc short-desc
                             :long-desc  (if long-desc (json-parse-string long-desc) nil)
                             :timestamp  timestamp
                             :bookmarks bookmarks )))
                   else collect
                   (list :elems (list line nil))))))))

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
    (magit-insert-heading "Log Graph")
    (dolist (entry (majutsu-parse-log-entries))
      (magit-insert-section section (majutsu-log-entry-section entry t)
                            (oset section commit-id (plist-get entry :id))
                            (oset section description (plist-get entry :description))
                            (oset section bookmarks (plist-get entry :bookmarks))
                            (magit-insert-heading
                              (insert (string-join (butlast (plist-get entry :elems)) " ")) "\n")
                            (when-let* ((long-desc (plist-get entry :long-desc))
                                        (long-desc (majutsu--indent-string long-desc (+ 10 (length (plist-get entry :prefix))))))
                              (magit-insert-section-body
                                (insert long-desc "\n")))))
    (insert "\n")))

(defun majutsu-log-insert-status ()
  "Insert jj status into current buffer."
  (let ((status-output (majutsu--run-command-color "status")))
    (when (and status-output (not (string-empty-p status-output)))
      (magit-insert-section (majutsu-status-section)
        (magit-insert-heading "Working Copy Status")
        (insert status-output)
        (insert "\n")
        ;; Analyze status and provide hints in the minibuffer
        (majutsu--analyze-status-for-hints status-output)))))

(defun majutsu-log-insert-diff ()
  "Insert jj diff with hunks into current buffer."
  (let ((diff-output (majutsu--run-command-color "diff" "--git")))
    (when (and diff-output (not (string-empty-p diff-output)))
      (magit-insert-section (majutsu-diff-section)
        (magit-insert-heading "Working Copy Changes")
        (majutsu--insert-diff-hunks diff-output)
        (insert "\n")))))

(defun majutsu--insert-diff-hunks (diff-output)
  "Parse and insert diff output as navigable hunk sections."
  (let ((lines (split-string diff-output "\n"))
        current-file
        file-section-content
        in-file-section)
    (dolist (line lines)
      (let ((clean-line (substring-no-properties line)))
        (cond
         ;; File header
         ((and (string-match "^diff --git a/\\(.*\\) b/\\(.*\\)$" clean-line)
               (let ((file-a (match-string 1 clean-line))
                     (file-b (match-string 2 clean-line)))
                 ;; Process any pending file section
                 (when (and in-file-section current-file)
                   (majutsu--insert-file-section current-file file-section-content))
                 ;; Start new file section
                 (setq current-file (or file-b file-a)
                       file-section-content (list line)
                       in-file-section t)
                 t)) ;; Return t to satisfy the condition
          ;; This is just a placeholder - the real work is done in the condition above
          nil)
         ;; Accumulate lines for current file section
         (in-file-section
          (push line file-section-content))
         ;; Outside of any file section
         (t nil))))
    ;; Process final file section if any
    (when (and in-file-section current-file)
      (majutsu--insert-file-section current-file file-section-content))))

(defun majutsu--insert-file-section (file lines)
  "Insert a file section with its hunks."
  (magit-insert-section file-section (majutsu-file-section)
                        (oset file-section file file)
                        (insert (propertize (concat "modified   " file "\n")
                                            'face 'magit-filename))
                        ;; Process the lines to find and insert hunks
                        (let ((remaining-lines (nreverse lines))
                              hunk-lines
                              in-hunk)
                          (dolist (line remaining-lines)
                            (cond
                             ;; Start of a hunk
                             ((string-match "^@@.*@@" line)
                              ;; Insert previous hunk if any
                              (when in-hunk
                                (majutsu--insert-hunk-lines file (nreverse hunk-lines)))
                              ;; Start new hunk
                              (setq hunk-lines (list line)
                                    in-hunk t))
                             ;; Skip header lines
                             ((string-match "^\\(diff --git\\|index\\|---\\|\\+\\+\\+\\|new file\\|deleted file\\)" line)
                              nil)
                             ;; Accumulate hunk lines
                             (in-hunk
                              (push line hunk-lines))))
                          ;; Insert final hunk if any
                          (when in-hunk
                            (majutsu--insert-hunk-lines file (nreverse hunk-lines))))))

(defun majutsu--insert-hunk-lines (file lines)
  "Insert a hunk section from LINES."
  (when lines
    (let ((header-line (car lines)))
      (when (string-match "^\\(@@.*@@\\)\\(.*\\)$" header-line)
        (let ((header (match-string 1 header-line))
              (context (match-string 2 header-line)))
          (magit-insert-section hunk-section (majutsu-hunk-section)
                                (oset hunk-section file file)
                                (oset hunk-section header header)
                                ;; Insert the hunk header
                                (insert (propertize header 'face 'magit-diff-hunk-heading))
                                (when (and context (not (string-empty-p context)))
                                  (insert (propertize context 'face 'magit-diff-hunk-heading)))
                                (insert "\n")
                                ;; Insert the hunk content
                                (dolist (line (cdr lines))
                                  (cond
                                   ((string-prefix-p "+" line)
                                    (insert (propertize line 'face 'magit-diff-added) "\n"))
                                   ((string-prefix-p "-" line)
                                    (insert (propertize line 'face 'magit-diff-removed) "\n"))
                                   (t
                                    (insert (propertize line 'face 'magit-diff-context) "\n"))))))))))

;;;###autoload
(defun majutsu-log ()
  "Display jj log in a magit-style buffer."
  (interactive)
  (let* ((repo-root (majutsu--root))
         (buffer-name (format "*majutsu-log:%s*" (file-name-nondirectory (directory-file-name repo-root))))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (default-directory repo-root)
            (inhibit-modification-hooks t))
        (erase-buffer)
        (majutsu-mode)
        (funcall majutsu-log-display-function buffer)
        (setq-local majutsu--repo-root repo-root)
        (magit-insert-section (jjbuf)  ; Root section wrapper
          (magit-insert-section-body
            (magit-run-section-hook 'majutsu-log-sections-hook))
          (insert "\n"))
        (goto-char (point-min))))))


;;;###autoload
(defalias 'majutsu #'majutsu-log
  "Begin using Majutsu.

This alias for `majutsu-log' exists for better discoverability.

Instead of invoking this alias for `majutsu-log' using
\"M-x majutsu RET\", you should bind a key to `majutsu-log'.")


(defun majutsu-log-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the majutsu log buffer."
  (interactive)
  (when (derived-mode-p 'majutsu-mode)
    (majutsu--with-progress "Refreshing log view"
                            (lambda ()
                              (let ((inhibit-read-only t)
                                    (pos (point)))
                                (erase-buffer)
                                (magit-insert-section (jjbuf)  ; Root section wrapper
                                  (magit-run-section-hook 'majutsu-log-sections-hook))
                                (goto-char pos)
                                (majutsu--debug "Log refresh completed"))))))

(defun majutsu-enter-dwim ()
  "Context-sensitive Enter key behavior."
  (interactive)
  (let ((section (magit-current-section)))
    (cond
     ;; On a changeset/commit - edit it with jj edit
     ((and section
           (memq (oref section type) '(majutsu-log-entry-section majutsu-commit-section))
           (slot-boundp section 'commit-id))
      (majutsu-edit-changeset-at-point))

     ;; On a diff hunk line - jump to that line in the file
     ((and section
           (eq (oref section type) 'majutsu-hunk-section)
           (slot-boundp section 'file))
      (majutsu-goto-diff-line))

     ;; On a file section - visit the file
     ((and section
           (eq (oref section type) 'majutsu-file-section)
           (slot-boundp section 'file))
      (majutsu-visit-file)))))

(defun majutsu-edit-changeset-at-point ()
  "Edit the commit at point using jj edit."
  (interactive)
  (when-let ((commit-id (majutsu-get-changeset-at-point)))
    (let ((result (majutsu--run-command "edit" commit-id)))
      (if (majutsu--handle-command-result (list "edit" commit-id) result
                                          (format "Now editing commit %s" commit-id)
                                          "Failed to edit commit")
          (progn
            (majutsu-log-refresh)
            (back-to-indentation))))))

(defun majutsu-goto-diff-line ()
  "Jump to the line in the file corresponding to the diff line at point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (_ (eq (oref section type) 'majutsu-hunk-section))
              (file (oref section file))
              (header (oref section header))
              (repo-root (majutsu--root)))
    ;; Parse the hunk header to get line numbers
    (when (string-match "^@@.*\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?.*@@" header)
      (let* ((start-line (string-to-number (match-string 1 header)))
             ;; Calculate which line within the hunk we're on
             (hunk-start (oref section start))
             (current-pos (point))
             (line-offset 0)
             (full-file-path (expand-file-name file repo-root)))
        ;; Count lines from hunk start to current position
        (save-excursion
          (goto-char hunk-start)
          (forward-line 1) ; Skip hunk header
          (while (< (point) current-pos)
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              ;; Only count context and added lines for line numbering
              (unless (string-prefix-p "-" line)
                (setq line-offset (1+ line-offset))))
            (forward-line 1)))
        ;; Open file and jump to calculated line
        (let ((target-line (+ start-line line-offset -1))) ; -1 because we start counting from the header
          (find-file full-file-path)
          (goto-char (point-min))
          (forward-line (max 0 target-line))
          (message "Jumped to line %d in %s" (1+ target-line) file))))))

(defun majutsu-visit-file ()
  "Visit the file at point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (file (oref section file))
              (repo-root (majutsu--root)))
    (let ((full-file-path (expand-file-name file repo-root)))
      (find-file full-file-path))))

(defun majutsu-diffedit-emacs ()
  "Emacs-based diffedit using built-in ediff."
  (interactive)
  (let* ((section (magit-current-section))
         (file (cond
                ((and section (eq (oref section type) 'majutsu-file-section))
                 (oref section file))
                ((and section (eq (oref section type) 'majutsu-hunk-section))
                 (oref section file))
                (t nil))))
    (if file
        (majutsu-diffedit-with-ediff file)
      (majutsu-diffedit-all))))

(defun majutsu-diffedit-with-ediff (file)
  "Open ediff session for a specific file against parent."
  (let* ((repo-root (majutsu--root))
         (full-file-path (expand-file-name file repo-root))
         (file-ext (file-name-extension file))
         (parent-temp-file (make-temp-file (format "majutsu-parent-%s" (file-name-nondirectory file))
                                           nil (when file-ext (concat "." file-ext))))
         (parent-content (let ((default-directory repo-root))
                           (majutsu--run-command "file" "show" "-r" "@-" file))))

    ;; Write parent content to temp file
    (with-temp-file parent-temp-file
      (insert parent-content)
      ;; Enable proper major mode for syntax highlighting
      (when file-ext
        (let ((mode (assoc-default (concat "." file-ext) auto-mode-alist 'string-match)))
          (when mode
            (funcall mode)))))

    ;; Set up cleanup
    (add-hook 'ediff-quit-hook
              `(lambda ()
                 (when (file-exists-p ,parent-temp-file)
                   (delete-file ,parent-temp-file))
                 (majutsu-log-refresh))
              nil t)

    ;; Start ediff session
    (ediff-files parent-temp-file full-file-path)
    (message "Ediff: Left=Parent (@-), Right=Current (@). Edit right side, then 'q' to quit and save.")))

(defun majutsu-diffedit-smerge ()
  "Emacs-based diffedit using smerge-mode (merge conflict style)."
  (interactive)
  (let* ((section (magit-current-section))
         (file (cond
                ((and section (eq (oref section type) 'majutsu-file-section))
                 (oref section file))
                ((and section (eq (oref section type) 'majutsu-hunk-section))
                 (oref section file))
                (t nil))))
    (if file
        (majutsu-diffedit-with-smerge file)
      (majutsu-diffedit-all))))

(defun majutsu-diffedit-with-smerge (file)
  "Open smerge-mode session for a specific file."
  (let* ((repo-root (majutsu--root))
         (full-file-path (expand-file-name file repo-root))
         (parent-content (let ((default-directory repo-root))
                           (majutsu--run-command "file" "show" "-r" "@-" file)))
         (current-content (if (file-exists-p full-file-path)
                              (with-temp-buffer
                                (insert-file-contents full-file-path)
                                (buffer-string))
                            ""))
         (merge-buffer (get-buffer-create (format "*majutsu-smerge-%s*" (file-name-nondirectory file)))))

    (with-current-buffer merge-buffer
      (erase-buffer)

      ;; Create merge-conflict format
      (insert "<<<<<<< Parent (@-)\n")
      (insert parent-content)
      (unless (string-suffix-p "\n" parent-content)
        (insert "\n"))
      (insert "=======\n")
      (insert current-content)
      (unless (string-suffix-p "\n" current-content)
        (insert "\n"))
      (insert ">>>>>>> Current (@)\n")

      ;; Enable smerge-mode
      (smerge-mode 1)
      (setq-local majutsu-smerge-file file)
      (setq-local majutsu-smerge-repo-root repo-root)

      ;; Add save hook
      (add-hook 'after-save-hook 'majutsu-smerge-apply-changes nil t)

      (goto-char (point-min)))

    (switch-to-buffer-other-window merge-buffer)
    (message "SMerge mode: Use C-c ^ commands to navigate/resolve conflicts, then save to apply.")))

(defun majutsu-smerge-apply-changes ()
  "Apply smerge changes to the original file."
  (when (and (boundp 'majutsu-smerge-file) majutsu-smerge-file)
    (let* ((file majutsu-smerge-file)
           (repo-root majutsu-smerge-repo-root)
           (full-file-path (expand-file-name file repo-root))
           (content (buffer-string)))

      ;; Only apply if no conflict markers remain
      (unless (or (string-match "^<<<<<<<" content)
                  (string-match "^=======" content)
                  (string-match "^>>>>>>>" content))
        (with-temp-file full-file-path
          (insert content))
        (majutsu-log-refresh)
        (message "Changes applied to %s" file)))))

(defun majutsu-diffedit-all ()
  "Open diffedit interface for all changes."
  (let* ((changed-files (majutsu--get-changed-files))
         (choice (if (= (length changed-files) 1)
                     (car changed-files)
                   (completing-read "Edit file: " changed-files))))
    (when choice
      (majutsu-diffedit-with-ediff choice))))

(defun majutsu--get-changed-files ()
  "Get list of files with changes in working copy."
  (let ((diff-output (majutsu--run-command "diff" "--name-only")))
    (split-string diff-output "\n" t)))

(defun majutsu-edit-changeset ()
  "Edit commit at point."
  (interactive)
  (when-let ((commit-id (majutsu-get-changeset-at-point)))
    (let ((result (majutsu--run-command "edit" commit-id)))
      (if (majutsu--handle-command-result (list "edit" commit-id) result
                                          (format "Now editing changeset %s" commit-id)
                                          "Failed to edit commit")
          (majutsu-log-refresh)))))

;; Squash state management
(defvar-local majutsu-squash-from nil
  "Currently selected 'from' commit for squash.")

(defvar-local majutsu-squash-into nil
  "Currently selected 'into' commit for squash.")

(defvar-local majutsu-squash-from-overlay nil
  "Overlay for highlighting the selected 'from' commit.")

(defvar-local majutsu-squash-into-overlay nil
  "Overlay for highlighting the selected 'into' commit.")

;;;###autoload
(defun majutsu-squash-clear-selections ()
  "Clear all squash selections and overlays."
  (interactive)
  (setq majutsu-squash-from nil
        majutsu-squash-into nil)
  (when majutsu-squash-from-overlay
    (delete-overlay majutsu-squash-from-overlay)
    (setq majutsu-squash-from-overlay nil))
  (when majutsu-squash-into-overlay
    (delete-overlay majutsu-squash-into-overlay)
    (setq majutsu-squash-into-overlay nil))
  (message "Cleared all squash selections"))

;;;###autoload
(defun majutsu-squash-set-from ()
  "Set the commit at point as squash `from' source."
  (interactive)
  (when-let ((commit-id (majutsu-get-changeset-at-point))
             (section (magit-current-section)))
    ;; Clear previous from overlay
    (when majutsu-squash-from-overlay
      (delete-overlay majutsu-squash-from-overlay))
    ;; Set new from
    (setq majutsu-squash-from commit-id)
    ;; Create overlay for visual indication
    (setq majutsu-squash-from-overlay
          (make-overlay (oref section start) (oref section end)))
    (overlay-put majutsu-squash-from-overlay 'face '(:background "dark orange" :foreground "white"))
    (overlay-put majutsu-squash-from-overlay 'before-string "[FROM] ")
    (message "Set from: %s" commit-id)))

;;;###autoload
(defun majutsu-squash-set-into ()
  "Set the commit at point as squash 'into' destination."
  (interactive)
  (when-let ((commit-id (majutsu-get-changeset-at-point))
             (section (magit-current-section)))
    ;; Clear previous into overlay
    (when majutsu-squash-into-overlay
      (delete-overlay majutsu-squash-into-overlay))
    ;; Set new into
    (setq majutsu-squash-into commit-id)
    ;; Create overlay for visual indication
    (setq majutsu-squash-into-overlay
          (make-overlay (oref section start) (oref section end)))
    (overlay-put majutsu-squash-into-overlay 'face '(:background "dark cyan" :foreground "white"))
    (overlay-put majutsu-squash-into-overlay 'before-string "[INTO] ")
    (message "Set into: %s" commit-id)))

;;;###autoload
(defun majutsu-squash-execute (&optional args)
  "Execute squash with selected from and into commits."
  (interactive (list (transient-args 'majutsu-squash-transient--internal)))
  (let ((keep-commit (member "--keep" args)))
    (cond
     ;; Both from and into selected
     ((and majutsu-squash-from majutsu-squash-into)
      (let* ((into-desc (string-trim (majutsu--run-command "log" "-r" majutsu-squash-into "--no-graph" "-T" "description")))
             (from-desc (string-trim (majutsu--run-command "log" "-r" majutsu-squash-from "--no-graph" "-T" "description")))
             (combined-desc (if (string-empty-p into-desc)
                                from-desc
                              into-desc))) ; Keep into message by default
        (majutsu--open-message-buffer "SQUASH_MSG"
                                      (format "jj squash --from %s --into %s" majutsu-squash-from majutsu-squash-into)
                                      'majutsu--squash-finish
                                      (list :from majutsu-squash-from :into majutsu-squash-into :keep keep-commit)
                                      combined-desc)))
     ;; Only from selected - use default behavior (squash into parent)
     (majutsu-squash-from
      (let* ((parent-desc (string-trim (majutsu--run-command "log" "-r" (format "%s-" majutsu-squash-from) "--no-graph" "-T" "description")))
             (from-desc (string-trim (majutsu--run-command "log" "-r" majutsu-squash-from "--no-graph" "-T" "description")))
             (combined-desc (if (string-empty-p parent-desc)
                                from-desc
                              parent-desc))) ; Keep parent message by default
        (majutsu--open-message-buffer "SQUASH_MSG"
                                      (format "jj squash -r %s" majutsu-squash-from)
                                      'majutsu--squash-finish
                                      (list :from majutsu-squash-from :into nil :keep keep-commit)
                                      combined-desc)))
     ;; No selection - use commit at point
     (t
      (if-let ((commit-id (majutsu-get-changeset-at-point)))
          (let* ((parent-desc (string-trim (majutsu--run-command "log" "-r" (format "%s-" commit-id) "--no-graph" "-T" "description")))
                 (commit-desc (string-trim (majutsu--run-command "log" "-r" commit-id "--no-graph" "-T" "description")))
                 (combined-desc (if (string-empty-p parent-desc)
                                    commit-desc
                                  parent-desc))) ; Keep parent message by default
            (majutsu--open-message-buffer "SQUASH_MSG"
                                          (format "jj squash -r %s" commit-id)
                                          'majutsu--squash-finish
                                          (list :from commit-id :into nil :keep keep-commit)
                                          combined-desc))
        (majutsu--message-with-log "No commit selected for squash"))))))

(defun majutsu--do-squash (from into keep-commit message)
  "Perform the actual squash operation."
  (let* ((cmd-args (cond
                    ;; Both from and into specified
                    ((and from into)
                     (append (list "squash" "--from" from "--into" into)
                             (when keep-commit (list "--keep-emptied"))
                             (when message (list "-m" message))))
                    ;; Only from specified (squash into parent)
                    (from
                     (append (list "squash" "-r" from)
                             (when keep-commit (list "--keep-emptied"))
                             (when message (list "-m" message))))
                    (t nil)))
         (progress-msg (if into
                           (format "Squashing %s into %s" from into)
                         (format "Squashing %s into its parent" from)))
         (success-msg (if into
                          (format "Squashed %s into %s" from into)
                        (format "Squashed %s into its parent" from))))
    (when cmd-args
      (majutsu--message-with-log "%s..." progress-msg)
      (let ((result (apply #'majutsu--run-command cmd-args)))
        (if (majutsu--handle-command-result cmd-args result success-msg "Squash failed")
            (progn
              (majutsu-squash-clear-selections)
              (majutsu-log-refresh)))))))

(defun majutsu--squash-finish (message &optional squash-params)
  "Finish squash with MESSAGE and SQUASH-PARAMS."
  (when squash-params
    (let ((from (plist-get squash-params :from))
          (into (plist-get squash-params :into))
          (keep (plist-get squash-params :keep)))
      (majutsu--do-squash from into keep message))))

(defun majutsu-squash-cleanup-on-exit ()
  "Clean up squash selections when transient exits."
  (unless (eq this-command 'majutsu-mode-bury-squash)
    (majutsu-squash-clear-selections)
    (remove-hook 'transient-exit-hook 'majutsu-squash-cleanup-on-exit t)))

;; Squash transient menu
;;;###autoload
(defun majutsu-squash-transient ()
  "Transient for jj squash operations."
  (interactive)
  ;; Add cleanup hook for when transient exits
  (add-hook 'transient-exit-hook 'majutsu-squash-cleanup-on-exit nil t)
  (majutsu-squash-transient--internal))

(transient-define-prefix majutsu-squash-transient--internal ()
  "Internal transient for jj squash operations."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description
   (lambda ()
     (concat "JJ Squash"
             (when majutsu-squash-from
               (format " | From: %s" majutsu-squash-from))
             (when majutsu-squash-into
               (format " | Into: %s" majutsu-squash-into))))
   ["Selection"
    ("f" "Set from" majutsu-squash-set-from
     :description (lambda ()
                    (if majutsu-squash-from
                        (format "Set from (current: %s)" majutsu-squash-from)
                      "Set from"))
     :transient t)
    ("t" "Set into" majutsu-squash-set-into
     :description (lambda ()
                    (if majutsu-squash-into
                        (format "Set into (current: %s)" majutsu-squash-into)
                      "Set into"))
     :transient t)
    ("c" "Clear selections" majutsu-squash-clear-selections
     :transient t)]
   ["Options"
    ("-k" "Keep emptied commit" "--keep")]
   ["Actions"
    ("s" "Execute squash" majutsu-squash-execute
     :description (lambda ()
                    (cond
                     ((and majutsu-squash-from majutsu-squash-into)
                      (format "Squash %s into %s" majutsu-squash-from majutsu-squash-into))
                     (majutsu-squash-from
                      (format "Squash %s into parent" majutsu-squash-from))
                     (t "Execute squash (select commits first)")))
     :transient nil)
    ("q" "Quit" transient-quit-one)
    ("b" "Bury" majutsu-mode-bury-squash)]])

(defun majutsu-mode-bury-squash ()
  (interactive)
  (transient-quit-one))

(defun majutsu-bookmark-create ()
  "Create a new bookmark."
  (interactive)
  (let* ((commit-id (or (majutsu-get-changeset-at-point) "@"))
         (name (read-string "Bookmark name: ")))
    (unless (string-empty-p name)
      (majutsu--run-command "bookmark" "create" name "-r" commit-id)
      (majutsu-log-refresh))))

(defun majutsu-bookmark-delete ()
  "Delete a bookmark and propagate on next push."
  (interactive)
  (let* ((names (majutsu--get-bookmark-names))
         (table (majutsu--completion-table-with-category names 'majutsu-bookmark))
         (choice (and names (completing-read "Delete bookmark (propagates on push): " table nil t))))
    (if (not choice)
        (message "No bookmarks found")
      (majutsu--run-command "bookmark" "delete" choice)
      (majutsu-log-refresh)
      (message "Deleted bookmark '%s'" choice))))

(defun majutsu-bookmark-forget ()
  "Forget a bookmark (local only, no deletion propagation)."
  (interactive)
  (let* ((names (majutsu--get-bookmark-names))
         (table (majutsu--completion-table-with-category names 'majutsu-bookmark))
         (choice (and names (completing-read "Forget bookmark: " table nil t))))
    (if (not choice)
        (message "No bookmarks found")
      (majutsu--run-command "bookmark" "forget" choice)
      (majutsu-log-refresh)
      (message "Forgot bookmark '%s'" choice))))

(defun majutsu-bookmark-track ()
  "Track remote bookmark(s)."
  (interactive)
  (let* ((remote-bookmarks (majutsu--get-bookmark-names t))
         (table (majutsu--completion-table-with-category remote-bookmarks 'majutsu-bookmark))
         (choice (and remote-bookmarks (completing-read "Track remote bookmark: " table nil t))))
    (if (not choice)
        (message "No remote bookmarks found")
      (majutsu--run-command "bookmark" "track" choice)
      (majutsu-log-refresh)
      (message "Tracking bookmark '%s'" choice))))

;;;###autoload
(defun majutsu-bookmark-list (&optional all)
  "List bookmarks in a temporary buffer.
With prefix ALL, include remote bookmarks."
  (interactive "P")
  (let* ((args (append '("bookmark" "list" "--quiet") (and all '("--all"))))
         (output (apply #'majutsu--run-command-color args))
         (buf (get-buffer-create "*Majutsu Bookmarks*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
      (view-mode 1))
    (funcall majutsu-log-display-function buf)))

;;;###autoload
(defun majutsu-bookmark-move (commit names)
  "Move existing bookmark(s) NAMES to COMMIT."
  (interactive
   (let* ((existing (majutsu--get-bookmark-names))
          (table (majutsu--completion-table-with-category existing 'majutsu-bookmark))
          (crm-separator (or (bound-and-true-p crm-separator) ", *"))
          (names (completing-read-multiple "Move bookmark(s): " table nil t))
          (at (or (majutsu-get-changeset-at-point) "@"))
          (rev (read-string (format "Target revision (default %s): " at) nil nil at)))
     (list rev names)))
  (when names
    (apply #'majutsu--run-command (append '("bookmark" "move" "-t" ) (list commit) names))
    (majutsu-log-refresh)
    (message "Moved bookmark(s) to %s: %s" commit (string-join names ", "))))

;;;###autoload
(defun majutsu-bookmark-rename (old new)
  "Rename bookmark OLD to NEW."
  (interactive
   (let* ((existing (majutsu--get-bookmark-names))
          (table (majutsu--completion-table-with-category existing 'majutsu-bookmark))
          (old (completing-read "Rename bookmark: " table nil t))
          (new (read-string (format "New name for %s: " old))))
     (list old new)))
  (when (and (not (string-empty-p old)) (not (string-empty-p new)))
    (majutsu--run-command "bookmark" "rename" old new)
    (majutsu-log-refresh)
    (message "Renamed bookmark '%s' -> '%s'" old new)))

;;;###autoload
(defun majutsu-bookmark-set (name commit)
  "Create or update bookmark NAME to point to COMMIT."
  (interactive
   (let* ((existing (majutsu--get-bookmark-names))
          (table (majutsu--completion-table-with-category existing 'majutsu-bookmark))
          (name (completing-read "Set bookmark: " table nil nil))
          (at (or (majutsu-get-changeset-at-point) "@"))
          (rev (read-string (format "Target revision (default %s): " at) nil nil at)))
     (list name rev)))
  (majutsu--run-command "bookmark" "set" name "-r" commit)
  (majutsu-log-refresh)
  (message "Set bookmark '%s' to %s" name commit))

;;;###autoload
(defun majutsu-bookmark-untrack (names)
  "Stop tracking remote bookmark(s) NAMES (e.g., name@remote)."
  (interactive
   (let* ((remote-names (majutsu--get-bookmark-names t))
          (table (majutsu--completion-table-with-category remote-names 'majutsu-bookmark))
          (crm-separator (or (bound-and-true-p crm-separator) ", *"))
          (names (completing-read-multiple "Untrack remote bookmark(s): " table nil t)))
     (list names)))
  (when names
    (apply #'majutsu--run-command (append '("bookmark" "untrack") names))
    (majutsu-log-refresh)
    (message "Untracked: %s" (string-join names ", "))))


;; Bookmark transient menu
;;;###autoload
(defun majutsu-bookmark-transient ()
  "Transient for jj bookmark operations."
  (interactive)
  (majutsu-bookmark-transient--internal))

(transient-define-prefix majutsu-bookmark-transient--internal ()
  "Internal transient for jj bookmark operations."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  ["Bookmark Operations"
   [
    ("l" "List bookmarks" majutsu-bookmark-list
     :description "Show bookmark list" :transient nil)
    ("c" "Create bookmark" majutsu-bookmark-create
     :description "Create new bookmark" :transient nil)]
   [
    ("s" "Set bookmark" majutsu-bookmark-set
     :description "Create/update to commit" :transient nil)
    ("m" "Move bookmark(s)" majutsu-bookmark-move
     :description "Move existing to commit" :transient nil)
    ("r" "Rename bookmark" majutsu-bookmark-rename
     :description "Rename existing bookmark" :transient nil)]
   [
    ("t" "Track remote" majutsu-bookmark-track
     :description "Track remote bookmark" :transient nil)
    ("u" "Untrack remote" majutsu-bookmark-untrack
     :description "Stop tracking remote" :transient nil)]
   [
    ("d" "Delete bookmark" majutsu-bookmark-delete
     :description "Delete (propagate)" :transient nil)
    ("f" "Forget bookmark" majutsu-bookmark-forget
     :description "Forget (local)" :transient nil)]
   [("q" "Quit" transient-quit-one)]] )

(defun majutsu-undo ()
  "Undo the last change."
  (interactive)
  (let ((commit-id (majutsu-get-changeset-at-point)))
    (majutsu--run-command "undo")
    (majutsu-log-refresh)
    (when commit-id
      (majutsu-goto-commit commit-id))))

(defun majutsu-redo ()
  "Redo the last undone change."
  (interactive)
  (let ((commit-id (majutsu-get-changeset-at-point)))
    (majutsu--run-command "redo")
    (majutsu-log-refresh)
    (when commit-id
      (majutsu-goto-commit commit-id))))

(defun majutsu-abandon ()
  "Abandon a changeset."
  (interactive)
  (if-let ((commit-id (majutsu-get-changeset-at-point)))
      (progn
        (majutsu--run-command "abandon" "-r" commit-id)
        (majutsu-log-refresh))
    (message "Can only run new on a change")))

(defun majutsu-new (arg)
  "Create a new changeset.
With prefix ARG, prompt for the name/ID of the base changeset from all remotes."
  (interactive "P")
  (let* ((base (if arg
                   (let* ((cands (majutsu--get-bookmark-names t))
                          (table (majutsu--completion-table-with-category cands 'majutsu-bookmark))
                          (s (completing-read "Create new changeset from (id/bookmark): "
                                              table nil nil)))
                     (when (not (string-empty-p s)) s))
                 (majutsu-get-changeset-at-point))))
    (if (not base)
        (user-error "Can only run new on a change")
      (let ((result (majutsu--run-command "new" "-r" base)))
        (when (majutsu--handle-command-result
               (list "new" "-r" base)
               result
               "Created new changeset"
               "Failed to create new changeset")
          (majutsu-log-refresh)
          (majutsu-goto-current))))))

(defun majutsu-goto-current ()
  "Jump to the current changeset (@)."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward "^.*@.*$" nil t)
      (goto-char (line-beginning-position))
    (message "Current changeset (@) not found")))

(defun majutsu-goto-commit (commit-id)
  "Jump to a specific COMMIT-ID in the log."
  (interactive "sCommit ID: ")
  (let ((start-pos (point)))
    (goto-char (point-min))
    (if (re-search-forward (regexp-quote commit-id) nil t)
        (goto-char (line-beginning-position))
      (goto-char start-pos)
      (message "Commit %s not found" commit-id))))

(defun majutsu-git-push (args)
  "Push to git remote with ARGS."
  (interactive (list (transient-args 'majutsu-git-push-transient)))
  (let* ((allow-new? (member "--allow-new" args))
         (all? (member "--all" args))
         (tracked? (member "--tracked" args))
         (deleted? (member "--deleted" args))
         (allow-empty? (member "--allow-empty-description" args))
         (allow-private? (member "--allow-private" args))
         (dry-run? (member "--dry-run" args))

         (remote-arg (seq-find (lambda (arg) (string-prefix-p "--remote=" arg)) args))
         (remote (when remote-arg (substring remote-arg (length "--remote="))))

         ;; Collect potential multi-value options supplied via --opt=value
         (bookmark-args (seq-filter (lambda (arg) (string-prefix-p "--bookmark=" arg)) args))
         (revision-args (seq-filter (lambda (arg) (string-prefix-p "--revisions=" arg)) args))
         (change-args   (seq-filter (lambda (arg) (string-prefix-p "--change=" arg)) args))
         (named-args    (seq-filter (lambda (arg) (string-prefix-p "--named=" arg)) args))

         (cmd-args (append '("git" "push")
                           (when remote (list "--remote" remote))
                           (when allow-new? '("--allow-new"))
                           (when all? '("--all"))
                           (when tracked? '("--tracked"))
                           (when deleted? '("--deleted"))
                           (when allow-empty? '("--allow-empty-description"))
                           (when allow-private? '("--allow-private"))
                           (when dry-run? '("--dry-run"))

                           ;; Expand = style into separate args as jj accepts space-separated
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--bookmark" (substring s (length "--bookmark="))))
                                                   bookmark-args))
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--revisions" (substring s (length "--revisions="))))
                                                   revision-args))
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--change" (substring s (length "--change="))))
                                                   change-args))
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--named" (substring s (length "--named="))))
                                                   named-args))))

         (success-msg (cond
                       ((and bookmark-args (= (length bookmark-args) 1))
                        (format "Successfully pushed bookmark %s"
                                (substring (car bookmark-args) (length "--bookmark="))))
                       (bookmark-args "Successfully pushed selected bookmarks")
                       (t "Successfully pushed to remote"))))
    (let ((result (apply #'majutsu--run-command cmd-args)))
      (when (majutsu--handle-push-result cmd-args result success-msg)
        (majutsu-log-refresh)))))

(defun majutsu--get-git-remotes ()
  "Return a list of Git remote names for the current repository.
Tries `jj git remote list' first, then falls back to `git remote'."
  (let* ((out (condition-case _
                  (majutsu--run-command "git" "remote" "list")
                (error "")))
         (names (if (and out (not (string-empty-p out)))
                    (let* ((lines (split-string out "\n" t))
                           (names (mapcar (lambda (l)
                                            (car (split-string l "[ :\t]" t)))
                                          lines)))
                      (delete-dups (copy-sequence names)))
                  ;; Fallback to plain `git remote`
                  (with-temp-buffer
                    (let* ((default-directory (majutsu--root))
                           (exit (process-file "git" nil t nil "remote")))
                      (when (eq exit 0)
                        (split-string (buffer-string) "\n" t)))))))
    names))

(defun majutsu-commit ()
  "Open commit message buffer."
  (interactive)
  (let ((current-desc (string-trim (majutsu--run-command "log" "-r" "@" "--no-graph" "-T" "description"))))
    (majutsu--open-message-buffer "COMMIT_MSG" "jj commit" 'majutsu--commit-finish nil current-desc)))

(defun majutsu-describe ()
  "Open describe message buffer."
  (interactive)
  (let ((commit-id (majutsu-get-changeset-at-point)))
    (if commit-id
        (let ((current-desc (string-trim (majutsu--run-command "log" "-r" commit-id "--no-graph" "-T" "description"))))
          (majutsu--open-message-buffer "DESCRIBE_MSG"
                                        (format "jj describe -r %s" commit-id)
                                        'majutsu--describe-finish commit-id current-desc))
      (message "No changeset at point"))))

(defun majutsu--open-message-buffer (buffer-name command finish-func &optional commit-id initial-desc)
  "Open a message editing buffer."
  (let* ((repo-root (majutsu--root))
         (log-buffer (current-buffer))
         (window-config (current-window-configuration))
         (buffer (get-buffer-create (format "*%s:%s*" buffer-name (file-name-nondirectory (directory-file-name repo-root))))))
    (with-current-buffer buffer
      (erase-buffer)
      (text-mode)
      (setq-local default-directory repo-root)
      (setq-local majutsu--message-command command)
      (setq-local majutsu--message-finish-func finish-func)
      (setq-local majutsu--message-commit-id commit-id)
      (setq-local majutsu--log-buffer log-buffer)
      (setq-local majutsu--window-config window-config)
      (local-set-key (kbd "C-c C-c") 'majutsu--message-finish)
      (local-set-key (kbd "C-c C-k") 'majutsu--message-abort)
      (when initial-desc
        (insert initial-desc))
      (insert "\n\n# Enter your message. C-c C-c to finish, C-c C-k to cancel\n"))
    (pop-to-buffer buffer)
    (goto-char (point-min))))

(defun majutsu--message-finish ()
  "Finish editing the message and execute the command."
  (interactive)
  (let* ((message (buffer-substring-no-properties (point-min) (point-max)))
         (lines (split-string message "\n"))
         (filtered-lines (seq-remove (lambda (line) (string-prefix-p "#" line)) lines))
         (final-message (string-trim (string-join filtered-lines "\n")))
         (command majutsu--message-command)
         (finish-func majutsu--message-finish-func)
         (commit-id majutsu--message-commit-id)
         (log-buffer majutsu--log-buffer)
         (window-config majutsu--window-config))
    (if (string-empty-p final-message)
        (message "Empty message, aborting")
      (kill-buffer)
      (set-window-configuration window-config)
      (funcall finish-func final-message commit-id))))

(defun majutsu--message-abort ()
  "Abort message editing."
  (interactive)
  (when (yes-or-no-p "Abort message editing? ")
    (let ((window-config majutsu--window-config))
      (kill-buffer)
      (set-window-configuration window-config)
      (message "Aborted"))))

(defun majutsu--commit-finish (message &optional _commit-id)
  "Finish commit with MESSAGE."
  (majutsu--message-with-log "Committing changes...")
  (let ((result (majutsu--run-command "commit" "-m" message)))
    (if (majutsu--handle-command-result (list "commit" "-m" message) result
                                        "Successfully committed changes"
                                        "Failed to commit")
        (majutsu-log-refresh))))

(defun majutsu--describe-finish (message &optional commit-id)
  "Finish describe with MESSAGE for COMMIT-ID."
  (if commit-id
      (progn
        (majutsu--message-with-log "Updating description for %s..." commit-id)
        (let ((result (majutsu--run-command "describe" "-r" commit-id "-m" message)))
          (if (majutsu--handle-command-result (list "describe" "-r" commit-id "-m" message) result
                                              (format "Description updated for %s" commit-id)
                                              "Failed to update description")
              (majutsu-log-refresh))))
    (majutsu--message-with-log "No commit ID available for description update")))

(defun majutsu-git-fetch (args)
  "Fetch from git remote with ARGS from transient."
  (interactive (list (transient-args 'majutsu-git-fetch-transient)))
  (majutsu--message-with-log "Fetching from remote...")
  (let* ((tracked? (member "--tracked" args))
         (all-remotes? (member "--all-remotes" args))

         (branch-args (seq-filter (lambda (arg) (string-prefix-p "--branch=" arg)) args))
         (remote-args (seq-filter (lambda (arg) (string-prefix-p "--remote=" arg)) args))

         (cmd-args (append '("git" "fetch")
                           (when tracked? '("--tracked"))
                           (when all-remotes? '("--all-remotes"))
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--branch" (substring s (length "--branch="))))
                                                   branch-args))
                           (apply #'append (mapcar (lambda (s)
                                                     (list "--remote" (substring s (length "--remote="))))
                                                   remote-args))))
         (result (apply #'majutsu--run-command cmd-args)))
    (if (majutsu--handle-command-result cmd-args result
                                        "Fetched from remote" "Fetch failed")
        (majutsu-log-refresh))))

(defun majutsu-diff ()
  "Show diff for current change or commit at point."
  (interactive)
  (let* ((commit-id (majutsu-get-changeset-at-point))
         (buffer (get-buffer-create "*majutsu-diff*"))
         (prev-buffer (current-buffer)))
    (if (not commit-id)
        (message "No diff to view at point.  Try again on a changeset.")
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (if commit-id
              (insert (majutsu--run-command-color "show" "-r" commit-id))
            (insert (majutsu--run-command-color "show")))
          (diff-mode)
          (ansi-color-apply-on-region (point-min) (point-max))
          (goto-char (point-min))
          ;; Make buffer read-only
          (setq buffer-read-only t)
          ;; Set up local keymap
          (use-local-map (copy-keymap diff-mode-map))
          (local-set-key (kbd "q")
                         (lambda ()
                           (interactive)
                           (kill-buffer)
                           (when (buffer-live-p prev-buffer)
                             (switch-to-buffer prev-buffer)))))))
    (switch-to-buffer buffer)))

;;;###autoload
(defun majutsu-goto-next-changeset ()
  "Navigate to the next changeset in the log."
  (interactive)
  (let ((pos (point))
        found)
    (while (and (not found)
                (< (point) (point-max)))
      (magit-section-forward)
      (when-let ((section (magit-current-section)))
        (when (and (memq (oref section type) '(majutsu-log-entry-section majutsu-commit-section))
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
      (when-let ((section (magit-current-section)))
        (when (and (memq (oref section type) '(majutsu-log-entry-section majutsu-commit-section))
                   (< (point) pos))
          (setq found t))))
    (unless found
      (goto-char pos)
      (message "No more changesets"))))

(defun majutsu-get-changeset-at-point ()
  "Get the changeset ID at point as a plain string (no text properties)."
  (when-let ((section (magit-current-section)))
    (cond
     ((and (slot-exists-p section 'commit-id)
           (slot-boundp section 'commit-id)
           (memq (oref section type) '(majutsu-log-entry-section majutsu-commit-section)))
      (let ((cid (oref section commit-id)))
        (when (stringp cid)
          (substring-no-properties cid))))
     (t nil))))

;; Rebase state management
(defvar-local majutsu-rebase-source nil
  "Currently selected source commit for rebase.")

(defvar-local majutsu-rebase-destinations nil
  "List of currently selected destination commits for rebase.")

(defvar-local majutsu-rebase-source-overlay nil
  "Overlay for highlighting the selected source commit.")

(defvar-local majutsu-rebase-destination-overlays nil
  "List of overlays for highlighting selected destination commits.")

;;;###autoload
(defun majutsu-rebase-clear-selections ()
  "Clear all rebase selections and overlays."
  (interactive)
  (setq majutsu-rebase-source nil
        majutsu-rebase-destinations nil)
  (when majutsu-rebase-source-overlay
    (delete-overlay majutsu-rebase-source-overlay)
    (setq majutsu-rebase-source-overlay nil))
  (dolist (overlay majutsu-rebase-destination-overlays)
    (delete-overlay overlay))
  (setq majutsu-rebase-destination-overlays nil)
  (message "Cleared all rebase selections"))

;;;###autoload
(defun majutsu-rebase-set-source ()
  "Set the commit at point as rebase source."
  (interactive)
  (when-let ((commit-id (majutsu-get-changeset-at-point))
             (section (magit-current-section)))
    ;; Clear previous source overlay
    (when majutsu-rebase-source-overlay
      (delete-overlay majutsu-rebase-source-overlay))
    ;; Set new source
    (setq majutsu-rebase-source commit-id)
    ;; Create overlay for visual indication
    (setq majutsu-rebase-source-overlay
          (make-overlay (oref section start) (oref section end)))
    (overlay-put majutsu-rebase-source-overlay 'face '(:background "dark green" :foreground "white"))
    (overlay-put majutsu-rebase-source-overlay 'before-string "[SOURCE] ")
    (message "Set source: %s" commit-id)))

;;;###autoload
(defun majutsu-rebase-toggle-destination ()
  "Toggle the commit at point as a rebase destination."
  (interactive)
  (when-let ((commit-id (majutsu-get-changeset-at-point))
             (section (magit-current-section)))
    (if (member commit-id majutsu-rebase-destinations)
        ;; Remove from destinations
        (progn
          (setq majutsu-rebase-destinations (remove commit-id majutsu-rebase-destinations))
          ;; Remove overlay
          (dolist (overlay majutsu-rebase-destination-overlays)
            (when (and (>= (overlay-start overlay) (oref section start))
                       (<= (overlay-end overlay) (oref section end)))
              (delete-overlay overlay)
              (setq majutsu-rebase-destination-overlays (remove overlay majutsu-rebase-destination-overlays))))
          (message "Removed destination: %s" commit-id))
      ;; Add to destinations
      (push commit-id majutsu-rebase-destinations)
      ;; Create overlay for visual indication
      (let ((overlay (make-overlay (oref section start) (oref section end))))
        (overlay-put overlay 'face '(:background "dark blue" :foreground "white"))
        (overlay-put overlay 'before-string "[DEST] ")
        (push overlay majutsu-rebase-destination-overlays)
        (message "Added destination: %s" commit-id)))))

;;;###autoload
(defun majutsu-rebase-execute ()
  "Execute rebase with selected source and destinations."
  (interactive)
  (if (and majutsu-rebase-source majutsu-rebase-destinations)
      (when (yes-or-no-p (format "Rebase %s -> %s? "
                                 majutsu-rebase-source
                                 (string-join majutsu-rebase-destinations ", ")))
        (let* ((dest-args (apply 'append (mapcar (lambda (dest) (list "-d" dest)) majutsu-rebase-destinations)))
               (all-args (append (list "rebase" "-s" majutsu-rebase-source) dest-args))
               (progress-msg (format "Rebasing %s onto %s"
                                     majutsu-rebase-source
                                     (string-join majutsu-rebase-destinations ", ")))
               (success-msg (format "Rebase completed: %s -> %s"
                                    majutsu-rebase-source
                                    (string-join majutsu-rebase-destinations ", "))))
          (majutsu--message-with-log "%s..." progress-msg)
          (let ((result (apply #'majutsu--run-command all-args)))
            (if (majutsu--handle-command-result all-args result success-msg "Rebase failed")
                (progn
                  (majutsu-rebase-clear-selections)
                  (majutsu-log-refresh))))))
    (majutsu--message-with-log "Please select source (s) and at least one destination (d) first")))

;; Transient rebase menu
;;;###autoload
(defun majutsu-rebase-transient ()
  "Transient for jj rebase operations."
  (interactive)
  ;; Add cleanup hook for when transient exits
  (add-hook 'transient-exit-hook 'majutsu-rebase-cleanup-on-exit nil t)
  (majutsu-rebase-transient--internal))

(defun majutsu-rebase-cleanup-on-exit ()
  "Clean up rebase selections when transient exits."
  (majutsu-rebase-clear-selections)
  (remove-hook 'transient-exit-hook 'majutsu-rebase-cleanup-on-exit t))

(transient-define-prefix majutsu-rebase-transient--internal ()
  "Internal transient for jj rebase operations."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description
   (lambda ()
     (concat "JJ Rebase"
             (when majutsu-rebase-source
               (format " | Source: %s" majutsu-rebase-source))
             (when majutsu-rebase-destinations
               (format " | Destinations: %s"
                       (string-join majutsu-rebase-destinations ", ")))))
   :class transient-columns
   ["Selection"
    ("s" "Set source" majutsu-rebase-set-source
     :description (lambda ()
                    (if majutsu-rebase-source
                        (format "Set source (current: %s)" majutsu-rebase-source)
                      "Set source"))
     :transient t)
    ("d" "Toggle destination" majutsu-rebase-toggle-destination
     :description (lambda ()
                    (format "Toggle destination (%d selected)"
                            (length majutsu-rebase-destinations)))
     :transient t)
    ("c" "Clear selections" majutsu-rebase-clear-selections
     :transient t)]
   ["Actions"
    ("r" "Execute rebase" majutsu-rebase-execute
     :description (lambda ()
                    (if (and majutsu-rebase-source majutsu-rebase-destinations)
                        (format "Rebase %s -> %s"
                                majutsu-rebase-source
                                (string-join majutsu-rebase-destinations ", "))
                      "Execute rebase (select source & destinations first)"))
     :transient nil)

    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix majutsu-git-transient ()
  "Top-level transient for jj git operations."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description "JJ Git"
   :class transient-columns
   ["Sync"
    ("p" "Push" majutsu-git-push-transient)
    ("f" "Fetch" majutsu-git-fetch-transient)
    ("e" "Export" majutsu-git-export)
    ("m" "Import" majutsu-git-import)]
   ["Remotes"
    ("r" "Manage remotes" majutsu-git-remote-transient)
    ("o" "Git root" majutsu-git-root)]
   ["Repository"
    ("c" "Clone" majutsu-git-clone-transient)
    ("i" "Init" majutsu-git-init-transient)]
   [("q" "Quit" transient-quit-one)]])

;; Push transient and command
(transient-define-prefix majutsu-git-push-transient ()
  "Transient for jj git push."
  [:class transient-columns
          ["Arguments"
           ("-R" "Remote" "--remote=" :choices majutsu--get-git-remotes)
           ("-b" "Bookmark" "--bookmark=" :choices majutsu--get-bookmark-names)
           ("-a" "All bookmarks" "--all")
           ("-t" "Tracked only" "--tracked")
           ("-D" "Deleted" "--deleted")
           ("-n" "Allow new" "--allow-new")
           ("-E" "Allow empty desc" "--allow-empty-description")
           ("-P" "Allow private" "--allow-private")
           ("-r" "Revisions" "--revisions=")
           ("-c" "Change" "--change=")
           ("-N" "Named X=REV" "--named=")
           ("-y" "Dry run" "--dry-run")]
          [("p" "Push" majutsu-git-push :transient nil)
           ("q" "Quit" transient-quit-one)]])

;; Fetch transient and command
(transient-define-prefix majutsu-git-fetch-transient ()
  "Transient for jj git fetch."
  [:class transient-columns
          ["Arguments"
           ("-R" "Remote" "--remote=" :choices majutsu--get-git-remotes)
           ("-B" "Branch" "--branch=")
           ("-t" "Tracked only" "--tracked")
           ("-A" "All remotes" "--all-remotes")]
          [("f" "Fetch" majutsu-git-fetch :transient nil)
           ("q" "Quit" transient-quit-one)]])

;; Remote management transients and commands
(defun majutsu-git-remote-list ()
  "List Git remotes in a temporary buffer."
  (interactive)
  (let* ((output (majutsu--run-command-color "git" "remote" "list"))
         (buf (get-buffer-create "*Majutsu Git Remotes*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
      (view-mode 1))
    (funcall majutsu-log-display-function buf)))

(defun majutsu-git-remote-add (args)
  "Add a Git remote. Prompts for name and URL; respects ARGS from transient."
  (interactive (list (transient-args 'majutsu-git-remote-transient)))
  (let* ((remote (read-string "Remote name: "))
         (url (read-string (format "URL for %s: " remote)))
         (fetch-tags (seq-find (lambda (a) (string-prefix-p "--fetch-tags=" a)) args))
         (cmd-args (append '("git" "remote" "add")
                           (when fetch-tags (list fetch-tags))
                           (list remote url)))
         (result (apply #'majutsu--run-command cmd-args)))
    (majutsu--handle-command-result cmd-args result
                                    (format "Added remote %s" remote)
                                    "Failed to add remote")))

(defun majutsu-git-remote-remove ()
  "Remove a Git remote and forget its bookmarks."
  (interactive)
  (let* ((remotes (or (majutsu--get-git-remotes) '("origin")))
         (remote (completing-read "Remove remote: " remotes nil t)))
    (when (and remote (not (string-empty-p remote)))
      (let* ((cmd-args (list "git" "remote" "remove" remote))
             (result (apply #'majutsu--run-command cmd-args)))
        (majutsu--handle-command-result cmd-args result
                                        (format "Removed remote %s" remote)
                                        "Failed to remove remote")))))

(defun majutsu-git-remote-rename ()
  "Rename a Git remote."
  (interactive)
  (let* ((remotes (or (majutsu--get-git-remotes) '("origin")))
         (old (completing-read "Rename remote: " remotes nil t))
         (new (read-string (format "New name for %s: " old))))
    (when (and (not (string-empty-p old)) (not (string-empty-p new)))
      (let* ((cmd-args (list "git" "remote" "rename" old new))
             (result (apply #'majutsu--run-command cmd-args)))
        (majutsu--handle-command-result cmd-args result
                                        (format "Renamed remote %s -> %s" old new)
                                        "Failed to rename remote")))))

(defun majutsu-git-remote-set-url ()
  "Set URL of a Git remote."
  (interactive)
  (let* ((remotes (or (majutsu--get-git-remotes) '("origin")))
         (remote (completing-read "Set URL for remote: " remotes nil t))
         (url (read-string (format "New URL for %s: " remote))))
    (when (and (not (string-empty-p remote)) (not (string-empty-p url)))
      (let* ((cmd-args (list "git" "remote" "set-url" remote url))
             (result (apply #'majutsu--run-command cmd-args)))
        (majutsu--handle-command-result cmd-args result
                                        (format "Set URL for %s" remote)
                                        "Failed to set remote URL")))))

(transient-define-prefix majutsu-git-remote-transient ()
  "Transient for managing Git remotes."
  [:class transient-columns
          ["Arguments (add)"
           ("-T" "Fetch tags" "--fetch-tags="
            :choices ("all" "included" "none"))]
          ["Actions"
           ("l" "List" majutsu-git-remote-list)
           ("a" "Add" majutsu-git-remote-add)
           ("d" "Remove" majutsu-git-remote-remove)
           ("r" "Rename" majutsu-git-remote-rename)
           ("u" "Set URL" majutsu-git-remote-set-url)
           ("q" "Quit" transient-quit-one)]])

;; Clone
(defun majutsu-git-clone (args)
  "Clone a Git repo into a new jj repo. Prompts for SOURCE and optional DEST; uses ARGS."
  (interactive (list (transient-args 'majutsu-git-clone-transient)))
  (let* ((source (read-string "Source (URL or path): "))
         (dest   (let ((d (read-directory-name "Destination (optional): " nil nil t)))
                   (when (and d (not (string-empty-p (expand-file-name d))))
                     ;; If user picks current dir, treat as empty and let jj default
                     (let ((dd (directory-file-name d)))
                       (if (string= dd (directory-file-name default-directory)) nil dd)))))
         (remote-name (let ((arg (seq-find (lambda (a) (string-prefix-p "--remote=" a)) args)))
                        (when arg (substring arg (length "--remote=")))))
         (depth (let ((arg (seq-find (lambda (a) (string-prefix-p "--depth=" a)) args)))
                  (when arg (substring arg (length "--depth=")))))
         (fetch-tags (seq-find (lambda (a) (string-prefix-p "--fetch-tags=" a)) args))
         (colocate? (member "--colocate" args))
         (no-colocate? (member "--no-colocate" args))
         (cmd-args (append '("git" "clone")
                           (when remote-name (list "--remote" remote-name))
                           (when colocate? '("--colocate"))
                           (when no-colocate? '("--no-colocate"))
                           (when depth (list "--depth" depth))
                           (when fetch-tags (list fetch-tags))
                           (list source)
                           (when dest (list dest))))
         (result (apply #'majutsu--run-command cmd-args)))
    (majutsu--handle-command-result cmd-args result
                                    "Clone completed"
                                    "Clone failed")))

(transient-define-prefix majutsu-git-clone-transient ()
  "Transient for jj git clone."
  [:class transient-columns
          ["Arguments"
           ("-R" "Remote name" "--remote=")
           ("-C" "Colocate" "--colocate")
           ("-x" "No colocate" "--no-colocate")
           ("-d" "Depth" "--depth=")
           ("-T" "Fetch tags" "--fetch-tags=" :choices ("all" "included" "none"))]
          [("c" "Clone" majutsu-git-clone :transient nil)
           ("q" "Quit" transient-quit-one)]])

;; Init
(defun majutsu-git-init (args)
  "Initialize a new Git-backed jj repo. Prompts for DEST; uses ARGS."
  (interactive (list (transient-args 'majutsu-git-init-transient)))
  (let* ((dest (let ((d (read-directory-name "Destination (default .): " nil nil t)))
                 (if (and d (not (string-empty-p d))) (directory-file-name d) ".")))
         (git-repo (let ((arg (seq-find (lambda (a) (string-prefix-p "--git-repo=" a)) args)))
                     (when arg (substring arg (length "--git-repo=")))))
         (colocate? (member "--colocate" args))
         (no-colocate? (member "--no-colocate" args))
         (cmd-args (append '("git" "init")
                           (when colocate? '("--colocate"))
                           (when no-colocate? '("--no-colocate"))
                           (when git-repo (list "--git-repo" git-repo))
                           (list dest)))
         (result (apply #'majutsu--run-command cmd-args)))
    (majutsu--handle-command-result cmd-args result
                                    "Init completed"
                                    "Init failed")))

(transient-define-prefix majutsu-git-init-transient ()
  "Transient for jj git init."
  [:class transient-columns
          ["Arguments"
           ("-C" "Colocate" "--colocate")
           ("-x" "No colocate" "--no-colocate")
           ("-g" "Use existing git repo" "--git-repo=")]
          [("i" "Init" majutsu-git-init :transient nil)
           ("q" "Quit" transient-quit-one)]])

;; Export / Import / Root
(defun majutsu-git-export ()
  "Update the underlying Git repo with changes made in the repo."
  (interactive)
  (let* ((cmd '("git" "export"))
         (result (apply #'majutsu--run-command cmd)))
    (majutsu--handle-command-result cmd result "Exported to Git" "Export failed")))

(defun majutsu-git-import ()
  "Update repo with changes made in the underlying Git repo."
  (interactive)
  (let* ((cmd '("git" "import"))
         (result (apply #'majutsu--run-command cmd)))
    (majutsu--handle-command-result cmd result "Imported from Git" "Import failed")))

(defun majutsu-git-root ()
  "Show the underlying Git directory of the current repository."
  (interactive)
  (let* ((dir (string-trim (majutsu--run-command "git" "root"))))
    (if (string-empty-p dir)
        (message "No underlying Git directory found")
      (kill-new dir)
      (message "Git root: %s (copied)" dir))))

(provide 'majutsu)
