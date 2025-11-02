;;; majutsu.el -*- lexical-binding: t; -*-

(require 'magit)
(require 'magit-section)
(require 'transient)
(require 'ansi-color)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'majutsu-template)
(eval-when-compile
  (require 'with-editor nil 'noerror)
  (declare-function with-editor--setup "with-editor" ()))
(defvar with-editor-emacsclient-executable)
(defvar with-editor-filter-visit-hook)
(defvar with-editor--envvar)
(defvar with-editor-envvars)
(defvar with-editor-server-window-alist)

(defcustom majutsu-with-editor-envvar "JJ_EDITOR"
  "Environment variable used to tell jj which editor to invoke."
  :type 'string
  :group 'majutsu)

(defconst majutsu--with-editor-description-regexp
  (rx (seq (or string-start
               (seq (* (not (any ?\n)))
                    (any ?/ ?\\)))
           "editor-" (+ (in "0-9A-Za-z"))
           ".jjdescription" string-end))
  "Regexp matching temporary jj description files created for editing.")

(defvar majutsu--with-editor-visit-queue nil
  "Queue of pending initializer functions for Majutsu with-editor buffers.")

(defun majutsu--with-editor--queue-visit (fn)
  "Enqueue FN so it runs when the next Majutsu editor buffer opens."
  (push fn majutsu--with-editor-visit-queue)
  fn)

(defun majutsu--with-editor--cancel-visit (fn)
  "Remove FN from the pending with-editor queue."
  (setq majutsu--with-editor-visit-queue
        (delq fn majutsu--with-editor-visit-queue)))

(defun majutsu--with-editor--apply-visit ()
  "Run the next pending Majutsu with-editor initializer if applicable."
  (let ((next '())
        handled)
    (dolist (fn majutsu--with-editor-visit-queue)
      (if (and (not handled)
               (with-demoted-errors "Majutsu with-editor init failed: %S"
                 (funcall fn)))
          (setq handled t)
        (push fn next)))
    (setq majutsu--with-editor-visit-queue (nreverse next))
    handled))

(defun majutsu--with-editor--target-buffer-p ()
  "Return non-nil when current buffer is a jj temporary editor file."
  (and buffer-file-name
       (string-match-p majutsu--with-editor-description-regexp buffer-file-name)))

(defvar-local majutsu--with-editor-return-window nil
  "Window to restore after finishing a Majutsu with-editor session.")

(defvar-local majutsu--with-editor-return-buffer nil
  "Buffer to restore after finishing a Majutsu with-editor session.")

(defun majutsu--with-editor--restore-context ()
  "Restore window focus and buffer after a Majutsu with-editor session."
  (let ((window majutsu--with-editor-return-window)
        (buffer majutsu--with-editor-return-buffer))
    (cond
     ((and (window-live-p window)
           (buffer-live-p buffer))
      (with-selected-window window
        (switch-to-buffer buffer)))
     ((buffer-live-p buffer)
      (majutsu--display-buffer-for-editor buffer))))
  (remove-hook 'with-editor-post-finish-hook #'majutsu--with-editor--restore-context t)
  (remove-hook 'with-editor-post-cancel-hook #'majutsu--with-editor--restore-context t))

(defun majutsu--with-editor--setup-return (window buffer)
  "Remember WINDOW and BUFFER for restoring after the editor closes."
  (setq-local majutsu--with-editor-return-window window)
  (setq-local majutsu--with-editor-return-buffer buffer)
  (add-hook 'with-editor-post-finish-hook #'majutsu--with-editor--restore-context nil t)
  (add-hook 'with-editor-post-cancel-hook #'majutsu--with-editor--restore-context nil t))

(defun majutsu--display-buffer-for-editor (buffer &optional window)
  "Display BUFFER using `majutsu-log-display-function'.
When WINDOW is a live window, run the display function in that window.
Return the window showing BUFFER."
  (let ((display-fn (or majutsu-log-display-function #'pop-to-buffer)))
    (if (window-live-p window)
        (with-selected-window window
          (funcall display-fn buffer))
      (funcall display-fn buffer))
    (or (get-buffer-window buffer t)
        (selected-window))))

(defun majutsu--with-editor-ensure-setup ()
  "Ensure with-editor integration specific to Majutsu is configured."
  (when (majutsu--with-editor-available-p)
    (when (boundp 'with-editor-envvars)
      (cl-pushnew majutsu-with-editor-envvar with-editor-envvars :test #'equal))
    (when (boundp 'with-editor-server-window-alist)
      (unless (cl-assoc majutsu--with-editor-description-regexp
                        with-editor-server-window-alist
                        :test #'string=)
        (push (cons majutsu--with-editor-description-regexp
                    (or majutsu-message-display-function #'pop-to-buffer))
              with-editor-server-window-alist)))
    (when (boundp 'with-editor-filter-visit-hook)
      (unless (memq #'majutsu--with-editor--apply-visit with-editor-filter-visit-hook)
        (add-hook 'with-editor-filter-visit-hook #'majutsu--with-editor--apply-visit)))
    (when (require 'server nil 'noerror)
      (unless (memq #'majutsu--with-editor--apply-visit server-visit-hook)
        (add-hook 'server-visit-hook #'majutsu--with-editor--apply-visit))
      (unless (memq #'majutsu--with-editor--apply-visit server-switch-hook)
        (add-hook 'server-switch-hook #'majutsu--with-editor--apply-visit)))))

(defmacro majutsu-with-editor (&rest body)
  "Ensure BODY runs with the correct editor environment for jj."
  (declare (indent 0) (debug (body)))
  `(let ((magit-with-editor-envvar majutsu-with-editor-envvar))
     (magit-with-editor
       ,@body)))

(defun majutsu--with-editor--normalize-editor (command)
  "Normalize editor COMMAND before exporting it to jj.
On Windows the jj launcher expects a bare executable path, so strip the
outer shell quoting that `with-editor' adds, matching Magit's workaround."
  (let ((trimmed (string-trim (or command ""))))
    (if (and (eq system-type 'windows-nt)
             (string-match "\\`\"\\([^\"]+\\)\"\\(.*\\)\\'" trimmed))
        (concat (match-string 1 trimmed)
                (match-string 2 trimmed))
      trimmed)))

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

(defcustom majutsu-confirm-critical-actions t
  "If non-nil, prompt for confirmation before undo/redo/abandon operations."
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

(defcustom majutsu-message-display-function #'pop-to-buffer
  "Function called to display the majutsu with-editor message buffer
The function must accept one argument: the buffer to display."
  :type '(choice
          (function-item switch-to-buffer)
          (function-item pop-to-buffer)
          (function-item display-buffer)
          (function :tag "Custom function"))
  :group 'majutsu)

(defconst majutsu-log--state-template
  '(:revisions nil
    :limit nil
    :reversed nil
    :no-graph nil
    :filesets nil)
  "Default plist template describing log view options.")

(defvar majutsu-log-state (copy-sequence majutsu-log--state-template)
  "Plist capturing the current jj log view options.")

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
    (when-let ((rev (majutsu-log--state-get :revisions)))
      (push (format "rev=%s" rev) parts))
    (when-let ((limit (majutsu-log--state-get :limit)))
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

(defun majutsu-log--build-args ()
  "Build argument list for `jj log' using current state."
  (let ((args '("log")))
    (when-let ((rev (majutsu-log--state-get :revisions)))
      (setq args (append args (list "-r" rev))))
    (when-let ((limit (majutsu-log--state-get :limit)))
      (setq args (append args (list "-n" limit))))
    (when (majutsu-log--state-get :reversed)
      (setq args (append args '("--reversed"))))
    (when (majutsu-log--state-get :no-graph)
      (setq args (append args '("--no-graph"))))
    (setq args (append args (list "-T" majutsu--log-template)))
    (setq args (append args (majutsu-log--state-get :filesets)))
    args))

(defun majutsu-log--refresh-view ()
  "Refresh current log buffer or open a new one."
  (if (derived-mode-p 'majutsu-mode)
      (majutsu-log-refresh)
    (majutsu-log)))

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
    (define-key map (kbd "l") 'majutsu-log-transient)
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
                 ("l" "Log options" majutsu-log-transient)
                 ("R" "Redo last change" majutsu-redo)
                 ("N" "New changeset" majutsu-new)
                 ("n" "New (transient)" majutsu-new-transient)
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
  ;; Clear new selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-new-clear-selections nil t)
  ;; Clear squash selections when buffer is killed
  (add-hook 'kill-buffer-hook 'majutsu-squash-clear-selections nil t))

(defvar-local majutsu--repo-root nil
  "Cached repository root for the current buffer.")

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
      (let ((coding-system-for-read 'utf-8-unix)
            (coding-system-for-write 'utf-8-unix))
        (setq exit-code (apply #'process-file majutsu-executable nil t nil safe-args)))
      (setq result (buffer-string))
      (majutsu--debug "Command completed in %.3f seconds, exit code: %d"
                      (float-time (time-subtract (current-time) start-time))
                      exit-code)
      (when (and majutsu-show-command-output (not (string-empty-p result)))
        (majutsu--debug "Command output: %s" (string-trim result)))
      result)))

(defun majutsu--with-editor-available-p ()
  "Return non-nil when `with-editor' can be used for JJ commands."
  (and (require 'with-editor nil 'noerror)
       with-editor-emacsclient-executable))

(defun majutsu--with-editor--visit-hook (window buffer)
  "Return initializer enabling `with-editor-mode' and tracking WINDOW/BUFFER."
  (let ((done nil))
    (lambda ()
      (when (and (not done)
                 (majutsu--with-editor--target-buffer-p))
        (setq done t)
        (when (fboundp 'with-editor-mode)
          (with-editor-mode 1))
        (goto-char (point-min))
        (majutsu--with-editor--setup-return window buffer)
        t))))

(defun majutsu--with-editor--sentinel (args success-msg error-msg success-callback)
  "Return sentinel handling JJ command completion.
ARGS, SUCCESS-MSG, ERROR-MSG mirror `majutsu--handle-command-result'.
SUCCESS-CALLBACK, when non-nil, is invoked after a successful command."
  (lambda (process _event)
    (let ((status (process-status process))
          (buffer (process-buffer process)))
      (when (and buffer (memq status '(exit signal)))
        (let ((output (with-current-buffer buffer
                        (ansi-color-filter-apply (buffer-string))))
              (exit-code (process-exit-status process)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer))
          (if (and (eq status 'exit) (zerop exit-code))
              (when (majutsu--handle-command-result args output success-msg nil)
                (majutsu-log-refresh)
                (when success-callback
                  (funcall success-callback)))
            (majutsu--handle-command-result args output nil
                                            (or error-msg "Command failed"))))))))

(defun majutsu--with-editor-run (args success-msg error-msg &optional success-callback)
  "Run JJ ARGS using with-editor.
On success, display SUCCESS-MSG and refresh the log; otherwise use ERROR-MSG."
  (unless (majutsu--with-editor-available-p)
    (user-error "with-editor is not available in this Emacs"))
  (majutsu--with-editor-ensure-setup)
  (let* ((default-directory (majutsu--root))
         (process-environment (copy-sequence process-environment))
         (origin-window (selected-window))
         (origin-buffer (current-buffer))
         (visit-hook (majutsu--with-editor--visit-hook origin-window origin-buffer))
         process buffer)
    (majutsu--with-editor--queue-visit visit-hook)
    (condition-case err
        (majutsu-with-editor
          (setq buffer (generate-new-buffer " *majutsu-jj*"))
          (when-let ((editor (getenv majutsu-with-editor-envvar)))
            (setq editor (majutsu--with-editor--normalize-editor editor))
            (setenv majutsu-with-editor-envvar editor)
            (setenv "EDITOR" editor))
          (setq process (apply #'start-file-process "majutsu-jj"
                               buffer majutsu-executable args)))
      (error
       (majutsu--with-editor--cancel-visit visit-hook)
       (signal (car err) (cdr err))))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (majutsu--with-editor--sentinel args success-msg error-msg success-callback))
    (majutsu--message-with-log "Launching jj %s (edit in current Emacs)..."
                               (string-join args " "))
    process))

(defun majutsu--run-command-color (&rest args)
  "Run jj command with ARGS and return colorized output."
  (majutsu--debug "Running color command: %s --color=always %s" majutsu-executable (string-join args " "))
  (let ((start-time (current-time))
        result exit-code)
    (with-temp-buffer
      (let ((process-environment (cons "FORCE_COLOR=1" (cons "CLICOLOR_FORCE=1" process-environment))))
        (let ((coding-system-for-read 'utf-8-unix)
              (coding-system-for-write 'utf-8-unix))
          (setq exit-code (apply #'process-file majutsu-executable nil t nil "--color=always" args)))
        (setq result (ansi-color-apply (buffer-string)))
        (majutsu--debug "Color command completed in %.3f seconds, exit code: %d"
                        (float-time (time-subtract (current-time) start-time))
                        exit-code)
        result))))

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
  (let ((metadata `(metadata (category . ,category))))
    (cond
     ((fboundp 'completion-table-with-metadata)
      (completion-table-with-metadata candidates metadata))
     ((functionp candidates)
      (lambda (string pred action)
        (if (eq action 'metadata)
            metadata
          (funcall candidates string pred action))))
     (t
      (lambda (string pred action)
        (if (eq action 'metadata)
            metadata
          (complete-with-action action candidates string pred)))))))

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
   (change-id :initarg :change-id)
   (description :initarg :description)
   (bookmarks :initarg :bookmarks)))

(defun majutsu--normalize-id-value (value)
  "Normalize VALUE (string/symbol/number) into a plain string without text properties."
  (cond
   ((stringp value) (substring-no-properties value))
   ((and value (not (stringp value))) (format "%s" value))
   (t nil)))

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
(defclass majutsu-diff-section (magit-section) ())
(defclass majutsu-file-section (magit-section)
  ((file :initarg :file)))
(defclass majutsu-hunk-section (magit-section)
  ((file :initarg :file)
   (start :initarg :hunk-start)
   (header :initarg :header)))



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
                   (trimmed-elems (mapcar #'string-trim raw-elems))
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
                          (seq-let (prefix change-id author bookmarks git-head conflict signature empty short-desc commit-id timestamp long-desc)
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
                            (when-let ((suffix-lines (plist-get entry :suffix-lines)))
                              (dolist (suffix-line suffix-lines)
                                (insert suffix-line)
                                (insert "\n")))))
    (insert "\n")))

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

(defun majutsu--goto-log-entry (change-id &optional commit-id)
  "Move point to the log entry section matching CHANGE-ID.
When CHANGE-ID is nil, fall back to COMMIT-ID.
Return non-nil when the section could be located."
  (when-let ((section (majutsu--find-log-entry-section change-id commit-id)))
    (magit-section-goto section)
    (goto-char (oref section start))
    t))

(defun majutsu-log--change-id-at-point ()
  "Return change id for the log entry at point, or nil otherwise."
  (majutsu--section-change-id (magit-current-section)))

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
        (majutsu--display-buffer-for-editor buffer)
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
    (let ((target-commit (majutsu-log--commit-id-at-point))
          (target-change (majutsu-log--change-id-at-point)))
      (majutsu--with-progress "Refreshing log view"
                              (lambda ()
                                (let ((inhibit-read-only t))
                                  (erase-buffer)
                                  (magit-insert-section (jjbuf)  ; Root section wrapper
                                    (magit-insert-section-body
                                      (magit-run-section-hook 'majutsu-log-sections-hook))
                                    (insert "\n")))
                                (unless (majutsu--goto-log-entry target-change target-commit)
                                  (majutsu-goto-current))
                                (when (fboundp 'magit-section-update-highlight)
                                  (magit-section-update-highlight))
                                (majutsu--debug "Log refresh completed"))))))

(defun majutsu-log--toggle-desc (label key)
  "Return LABEL annotated with ON/OFF state for KEY."
  (if (majutsu-log--state-get key)
      (format "%s [on]" label)
    (format "%s [off]" label)))

(defun majutsu-log--value-desc (label key)
  "Return LABEL annotated with the string value stored at KEY."
  (if-let ((value (majutsu-log--state-get key)))
      (format "%s (%s)" label value)
    label))

(defun majutsu-log--paths-desc ()
  "Return description for path filters."
  (let ((paths (majutsu-log--state-get :filesets)))
    (cond
     ((null paths) "Add path filter")
     ((= (length paths) 1) (format "Add path filter (%s)" (car paths)))
     (t (format "Add path filter (%d paths)" (length paths))))))

(defun majutsu-log-transient--redisplay ()
  "Redisplay the log transient, compatible with older transient versions."
  (if (fboundp 'transient-redisplay)
      (transient-redisplay)
    (when (fboundp 'transient--redisplay)
      (transient--redisplay))))

(defun majutsu-log-transient-set-revisions ()
  "Prompt for a revset and store it in log state."
  (interactive)
  (let* ((current (majutsu-log--state-get :revisions))
         (input (string-trim (read-from-minibuffer "Revset (empty to clear): " current))))
    (majutsu-log--state-set :revisions (unless (string-empty-p input) input))
    (majutsu-log-transient--redisplay)))

(defun majutsu-log-transient-clear-revisions ()
  "Clear the stored revset."
  (interactive)
  (majutsu-log--state-set :revisions nil)
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-set-limit ()
  "Prompt for a numeric limit and store it in log state."
  (interactive)
  (let* ((current (majutsu-log--state-get :limit))
         (input (string-trim (read-from-minibuffer "Limit (empty to clear): " current))))
    (cond
     ((string-empty-p input)
      (majutsu-log--state-set :limit nil))
     ((string-match-p "\\`[0-9]+\\'" input)
      (majutsu-log--state-set :limit input))
     (t
      (user-error "Limit must be a positive integer"))))
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-clear-limit ()
  "Clear the stored limit."
  (interactive)
  (majutsu-log--state-set :limit nil)
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient--toggle (key)
  "Toggle boolean KEY in log state."
  (majutsu-log--state-set key (not (majutsu-log--state-get key)))
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-toggle-reversed ()
  "Toggle reversed log ordering."
  (interactive)
  (majutsu-log-transient--toggle :reversed))

(defun majutsu-log-transient-toggle-no-graph ()
  "Toggle whether jj log should hide the ASCII graph."
  (interactive)
  (majutsu-log-transient--toggle :no-graph))

(defun majutsu-log-transient-add-path ()
  "Add a fileset/path filter to the log view."
  (interactive)
  (let* ((input (string-trim (read-from-minibuffer "Add path/pattern: ")))
         (paths (majutsu-log--state-get :filesets)))
    (when (and (not (string-empty-p input))
               (not (member input paths)))
      (majutsu-log--state-set :filesets (append paths (list input)))
      (majutsu-log-transient--redisplay))))

(defun majutsu-log-transient-clear-paths ()
  "Clear all path filters."
  (interactive)
  (majutsu-log--state-set :filesets nil)
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-reset ()
  "Reset log state to defaults."
  (interactive)
  (majutsu-log--reset-state)
  (majutsu-log-transient--redisplay))

(defun majutsu-log-transient-apply ()
  "Apply the current log state by refreshing or opening the log view."
  (interactive)
  (majutsu-log--refresh-view))

(transient-define-prefix majutsu-log-transient ()
  "Transient interface for adjusting jj log options."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description majutsu-log--transient-description
   :class transient-columns
   ["Revisions"
    ("r" "Revset" majutsu-log-transient-set-revisions
     :description (lambda ()
                    (majutsu-log--value-desc "Revset" :revisions))
     :transient t)
    ("R" "Clear revset" majutsu-log-transient-clear-revisions
     :if (lambda () (majutsu-log--state-get :revisions))
     :transient t)
    ("n" "Limit" majutsu-log-transient-set-limit
     :description (lambda ()
                    (majutsu-log--value-desc "Limit" :limit))
     :transient t)
    ("N" "Clear limit" majutsu-log-transient-clear-limit
     :if (lambda () (majutsu-log--state-get :limit))
     :transient t)
    ("v" "Reverse order" majutsu-log-transient-toggle-reversed
     :description (lambda ()
                    (majutsu-log--toggle-desc "Reverse order" :reversed))
     :transient t)
    ("t" "Hide graph" majutsu-log-transient-toggle-no-graph
     :description (lambda ()
                    (majutsu-log--toggle-desc "Hide graph" :no-graph))
     :transient t)]
   ["Paths"
    ("a" "Add path filter" majutsu-log-transient-add-path
     :description #'majutsu-log--paths-desc
     :transient t)
    ("A" "Clear path filters" majutsu-log-transient-clear-paths
     :if (lambda () (majutsu-log--state-get :filesets))
     :transient t)]
   ["Actions"
    ("g" "Apply & refresh" majutsu-log-transient-apply :transient nil)
    ("0" "Reset options" majutsu-log-transient-reset :transient t)
    ("q" "Quit" transient-quit-one)]])

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
  (when-let ((revset (majutsu-log--revset-at-point)))
    (let ((result (majutsu--run-command "edit" revset)))
      (if (majutsu--handle-command-result (list "edit" revset) result
                                          (format "Now editing revset %s" revset)
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
  (when-let ((revset (majutsu-log--revset-at-point)))
    (let ((result (majutsu--run-command "edit" revset)))
      (if (majutsu--handle-command-result (list "edit" revset) result
                                          (format "Now editing commit %s" revset)
                                          "Failed to edit commit")
          (majutsu-log-refresh)))))

;; Squash state management
(defvar-local majutsu-squash-from nil
  "List containing at most one entry struct for squash source.")

(defvar-local majutsu-squash-into nil
  "List containing at most one entry struct for squash destination.")

(defun majutsu-squash--from-entry ()
  "Return the entry selected as squash source, if any."
  (car majutsu-squash-from))

(defun majutsu-squash--into-entry ()
  "Return the entry selected as squash destination, if any."
  (car majutsu-squash-into))

(defun majutsu-squash--from-display ()
  "Return a display string for the squash source."
  (when-let ((entry (majutsu-squash--from-entry)))
    (majutsu--transient-entry-display entry)))

(defun majutsu-squash--into-display ()
  "Return a display string for the squash destination."
  (when-let ((entry (majutsu-squash--into-entry)))
    (majutsu--transient-entry-display entry)))

;;;###autoload
(defun majutsu-squash-clear-selections ()
  "Clear all squash selections and overlays."
  (interactive)
  (majutsu--transient-clear-overlays majutsu-squash-from)
  (majutsu--transient-clear-overlays majutsu-squash-into)
  (setq majutsu-squash-from nil
        majutsu-squash-into nil)
  (message "Cleared all squash selections"))

;;;###autoload
(defun majutsu-squash-set-from ()
  "Set the commit at point as squash `from' source."
  (interactive)
  (majutsu--transient-select-refset
   :kind "from"
   :label "[FROM]"
   :face '(:background "dark orange" :foreground "white")
   :collection-var 'majutsu-squash-from))

;;;###autoload
(defun majutsu-squash-set-into ()
  "Set the commit at point as squash 'into' destination."
  (interactive)
  (majutsu--transient-select-refset
   :kind "into"
   :label "[INTO]"
   :face '(:background "dark cyan" :foreground "white")
   :collection-var 'majutsu-squash-into))

;;;###autoload
(defun majutsu-squash-execute (&optional args)
  "Execute squash with selections recorded in the transient."
  (interactive (list (transient-args 'majutsu-squash-transient--internal)))
  (let* ((keep (member "--keep" args))
         (from-entry (majutsu-squash--from-entry))
         (into-entry (majutsu-squash--into-entry))
         (from (when from-entry (majutsu--transient-entry-revset from-entry)))
         (into (when into-entry (majutsu--transient-entry-revset into-entry))))
    (cond
     ((and from into)
      (majutsu--squash-run from into keep))
     (from
      (majutsu--squash-run from nil keep))
     ((majutsu-log--revset-at-point)
      (majutsu--squash-run (majutsu-log--revset-at-point) nil keep))
     (t
      (majutsu--message-with-log "No commit selected for squash")))))

(defun majutsu--squash-run (from into keep)
  "Run jj squash using with-editor."
  (let* ((args (append (if into
                           (list "squash" "--from" from "--into" into)
                         (list "squash" "-r" from))
                       (when keep '("--keep-emptied"))))
         (success-msg (if into
                          (format "Squashed %s into %s" from into)
                        (format "Squashed %s into its parent" from))))
    (majutsu--with-editor-run args success-msg "Squash failed"
                              #'majutsu-squash-clear-selections)))

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
             (when-let ((from (majutsu-squash--from-display)))
               (format " | From: %s" from))
             (when-let ((into (majutsu-squash--into-display)))
               (format " | Into: %s" into))))
   ["Selection"
    ("f" "Set from" majutsu-squash-set-from
     :description (lambda ()
                    (if (majutsu-squash--from-entry)
                        (format "Set from (current: %s)" (majutsu-squash--from-display))
                      "Set from"))
     :transient t)
    ("t" "Set into" majutsu-squash-set-into
     :description (lambda ()
                    (if (majutsu-squash--into-entry)
                        (format "Set into (current: %s)" (majutsu-squash--into-display))
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
                     ((and (majutsu-squash--from-entry) (majutsu-squash--into-entry))
                      (format "Squash %s into %s"
                              (majutsu-squash--from-display)
                              (majutsu-squash--into-display)))
                     ((majutsu-squash--from-entry)
                      (format "Squash %s into parent" (majutsu-squash--from-display)))
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
  (let* ((revset (or (majutsu-log--revset-at-point) "@"))
         (name (read-string "Bookmark name: ")))
    (unless (string-empty-p name)
      (majutsu--run-command "bookmark" "create" name "-r" revset)
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
(defun majutsu--bookmark-read-move-args ()
  "Return interactive arguments for bookmark move commands."
  (let* ((existing (majutsu--get-bookmark-names))
         (table (majutsu--completion-table-with-category existing 'majutsu-bookmark))
         (crm-separator (or (bound-and-true-p crm-separator) ", *"))
         (names (completing-read-multiple "Move bookmark(s): " table nil t))
         (at (or (majutsu-log--commit-id-at-point) "@"))
         (rev (read-string (format "Target revision (default %s): " at) nil nil at)))
    (ignore crm-separator)
    (list rev names)))

(defun majutsu--bookmark-move (commit names &optional allow-backwards)
  "Internal helper to move bookmark(s) NAMES to COMMIT.
When ALLOW-BACKWARDS is non-nil, include `--allow-backwards'."
  (when names
    (let ((args (append '("bookmark" "move")
                        (and allow-backwards '("--allow-backwards"))
                        (list "-t" commit)
                        names)))
      (apply #'majutsu--run-command args)
      (majutsu-log-refresh)
      (message (if allow-backwards
                   "Moved bookmark(s) (allow backwards) to %s: %s"
                 "Moved bookmark(s) to %s: %s")
               commit (string-join names ", ")))))

;;;###autoload
(defun majutsu-bookmark-move (commit names &optional allow-backwards)
  "Move existing bookmark(s) NAMES to COMMIT.
With optional ALLOW-BACKWARDS, pass `--allow-backwards' to jj."
  (interactive (majutsu--bookmark-read-move-args))
  (majutsu--bookmark-move commit names allow-backwards))

;;;###autoload
(defun majutsu-bookmark-move-allow-backwards (commit names)
  "Move bookmark(s) NAMES to COMMIT allowing backwards moves."
  (interactive (majutsu--bookmark-read-move-args))
  (majutsu--bookmark-move commit names t))

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
          (at (or (majutsu-log--commit-id-at-point) "@"))
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
    ("M" "Move bookmark(s) --allow-backwards" majutsu-bookmark-move-allow-backwards
     :description "Move allowing backwards" :transient nil)
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
  (if (and majutsu-confirm-critical-actions
           (not (yes-or-no-p "Undo the most recent change? ")))
      (message "Undo canceled")
    (let ((revset (majutsu-log--revset-at-point)))
      (majutsu--run-command "undo")
      (majutsu-log-refresh)
      (when revset
        (majutsu-goto-commit revset)))))

(defun majutsu-redo ()
  "Redo the last undone change."
  (interactive)
  (if (and majutsu-confirm-critical-actions
           (not (yes-or-no-p "Redo the previously undone change? ")))
      (message "Redo canceled")
    (let ((revset (majutsu-log--revset-at-point)))
      (majutsu--run-command "redo")
      (majutsu-log-refresh)
      (when revset
        (majutsu-goto-commit revset)))))

(defun majutsu-abandon ()
  "Abandon the changeset at point."
  (interactive)
  (if-let ((revset (majutsu-log--revset-at-point)))
      (if (and majutsu-confirm-critical-actions
               (not (yes-or-no-p (format "Abandon changeset %s? " revset))))
          (message "Abandon canceled")
        (progn
          (majutsu--run-command "abandon" "-r" revset)
          (majutsu-log-refresh)))
    (message "No changeset at point to abandon")))

(defun majutsu-new (arg)
  "Create a new changeset.
Without prefix ARG, use the changeset at point (or `@` when unavailable).
With prefix ARG, prompt for the parent revset via completion."
  (interactive "P")
  (let* ((parent (if arg
                     (let* ((cands (majutsu--get-bookmark-names t))
                            (table (majutsu--completion-table-with-category cands 'majutsu-bookmark))
                            (input (completing-read "Create new changeset from (id/bookmark): "
                                                    table nil nil)))
                       (unless (string-empty-p input) input))
                   (majutsu-log--revset-at-point)))
         (parents (when parent (list parent)))
         (args (majutsu-new--build-args
                :parents parents
                :after '()
                :before '()
                :message nil
                :no-edit nil)))
    (majutsu-new--run-command args)))

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
  "Create a commit using Emacs as the editor."
  (interactive)
  (majutsu--with-editor-run '("commit")
                            "Successfully committed changes"
                            "Failed to commit"))

(defun majutsu-describe ()
  "Update the description for the commit at point."
  (interactive)
  (let ((revset (or (majutsu-log--revset-at-point) "@")))
    (majutsu--with-editor-run (list "describe" "-r" revset)
                              (format "Description updated for %s" revset)
                              "Failed to update description")))


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

(defun majutsu-diff (&optional revision)
  "Show diff for REVISION or commit at point, defaulting to `@'."
  (interactive
   (list (or (majutsu-log--commit-id-at-point) "@")))
  (let* ((rev (or revision (majutsu-log--commit-id-at-point) "@"))
         (buffer (get-buffer-create "*majutsu-diff*"))
         (prev-buffer (current-buffer))
         (repo-root (majutsu--root)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (default-directory repo-root))
        (erase-buffer)
        (insert (majutsu--run-command-color "show" "-r" rev))
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
                           (switch-to-buffer prev-buffer))))))
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

(defun majutsu-log--commit-id-at-point ()
  "Get the changeset ID at point as a plain string (no text properties)."
  (or (majutsu-log--commit-only-at-point)
      (majutsu-log--change-id-at-point)))

;; New state management
(cl-defstruct (majutsu--transient-entry
               (:constructor majutsu--transient-entry-create))
  "Bookkeeping for transient selections bound to log entries."
  change-id
  commit-id
  overlay)

(defun majutsu--transient-entry-revset (entry)
  "Return the revset string to use for ENTRY, preferring change-id."
  (or (majutsu--transient-entry-change-id entry)
      (majutsu--transient-entry-commit-id entry)))

(defun majutsu--transient-entry-display (entry)
  "Return a human-readable identifier for ENTRY."
  (or (majutsu--transient-entry-change-id entry)
      (majutsu--transient-entry-commit-id entry)
      "?"))

(defun majutsu--transient-entry-delete-overlay (entry)
  "Delete the overlay associated with ENTRY, if present."
  (let ((overlay (majutsu--transient-entry-overlay entry)))
    (when (overlayp overlay)
      (delete-overlay overlay)
      (setf (majutsu--transient-entry-overlay entry) nil)))
  nil)

(defun majutsu--transient-clear-overlays (entries)
  "Delete overlays for all ENTRIES and return nil."
  (dolist (entry entries)
    (majutsu--transient-entry-delete-overlay entry))
  nil)

(defun majutsu--transient-make-overlay (section face label ref)
  "Create an overlay for SECTION with FACE, LABEL, and REF identifier."
  (let ((overlay (make-overlay (oref section start) (oref section end))))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'before-string (concat label " "))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'majutsu-ref (or ref ""))
    overlay))

(defun majutsu--transient-entry-apply-overlay (entry section face label)
  "Attach a fresh overlay for ENTRY using SECTION, FACE, and LABEL."
  (majutsu--transient-entry-delete-overlay entry)
  (when section
    (let ((ref (majutsu--transient-entry-revset entry)))
      (when ref
        (let ((overlay (majutsu--transient-make-overlay section face label ref)))
          (setf (majutsu--transient-entry-overlay entry) overlay)
          overlay)))))

(defun majutsu--transient-entry-reapply (entry face label)
  "Reapply overlay for ENTRY after a log refresh."
  (when-let ((section (majutsu--find-log-entry-section
                       (majutsu--transient-entry-change-id entry)
                       (majutsu--transient-entry-commit-id entry))))
    (majutsu--transient-entry-apply-overlay entry section face label)))

(defun majutsu--transient-selection-find (entries change-id commit-id)
  "Find ENTRY in ENTRIES matching CHANGE-ID (preferred) or COMMIT-ID."
  (seq-find (lambda (entry)
              (let ((entry-change (majutsu--transient-entry-change-id entry))
                    (entry-commit (majutsu--transient-entry-commit-id entry)))
                (cond
                 ((and change-id entry-change)
                  (string= entry-change change-id))
                 ((and (not change-id) commit-id entry-commit)
                  (string= entry-commit commit-id)))))
            entries))

(defun majutsu--transient-selection-remove (entries entry)
  "Return ENTRIES without ENTRY, cleaning up overlay side effects."
  (majutsu--transient-entry-delete-overlay entry)
  (delq entry entries))

(defun majutsu--transient-refresh-if-rewritten (entry new-commit face label)
  "Refresh log buffer when ENTRY's commit id changed to NEW-COMMIT.
FACE and LABEL are used to reapply the overlay post-refresh.
Return the section corresponding to the rewritten change when refresh occurs."
  (let ((old-commit (majutsu--transient-entry-commit-id entry)))
    (when (and entry old-commit new-commit (not (string= old-commit new-commit)))
      (let ((change (majutsu--transient-entry-change-id entry)))
        (majutsu--message-with-log "Change %s rewritten (%s -> %s); refreshing log"
                                   (or change "?")
                                   old-commit
                                   new-commit)
        (setf (majutsu--transient-entry-commit-id entry) new-commit)
        (majutsu--transient-entry-delete-overlay entry)
        (majutsu-log-refresh)
        (let ((section (majutsu--find-log-entry-section change new-commit)))
          (majutsu--transient-entry-apply-overlay entry section face label)
          section)))))

(defun majutsu-log--commit-only-at-point ()
  "Return the raw commit id at point, or nil if unavailable."
  (when-let ((section (magit-current-section)))
    (majutsu--section-commit-id section)))

(defun majutsu-log--ids-at-point ()
  "Return a plist (:change .. :commit .. :section ..) describing ids at point."
  (when-let ((section (magit-current-section)))
    (let ((change (majutsu--section-change-id section))
          (commit (majutsu--section-commit-id section)))
      (when (or change commit)
        (list :change change :commit commit :section section)))))

(defun majutsu-log--revset-at-point ()
  "Return the preferred revset (change id if possible) at point."
  (when-let ((ids (majutsu-log--ids-at-point)))
    (or (plist-get ids :change)
        (plist-get ids :commit))))

(cl-defun majutsu--transient--toggle-selection (&key kind label face collection-var (type 'multi))
  "Internal helper to mutate refset selections for the current log entry.
KIND/LABEL/FACE describe the UI; COLLECTION-VAR is the symbol storing entries.
TYPE is either `single' or `multi'."
  (let* ((ids (majutsu-log--ids-at-point)))
    (if (not ids)
        (message "No changeset at point to toggle")
      (let* ((change (plist-get ids :change))
             (commit (plist-get ids :commit))
             (section (plist-get ids :section))
             (entries (symbol-value collection-var))
             (existing (majutsu--transient-selection-find entries change commit)))
        (when existing
          (when-let ((new-section (majutsu--transient-refresh-if-rewritten existing commit face label)))
            (setq section new-section)))
        (pcase type
          ('single
           (if existing
               (progn
                 (majutsu--transient-clear-overlays entries)
                 (set collection-var nil)
                 (message "Cleared %s" kind))
             (let ((entry (majutsu--transient-entry-create
                           :change-id change :commit-id commit)))
               (majutsu--transient-clear-overlays entries)
               (let ((overlay-section (or section (majutsu--find-log-entry-section change commit))))
                 (majutsu--transient-entry-apply-overlay entry overlay-section face label))
               (set collection-var (list entry))
               (message "Set %s: %s" kind (majutsu--transient-entry-display entry)))))
          (_
           (if existing
               (progn
                 (set collection-var (majutsu--transient-selection-remove entries existing))
                 (message "Removed %s: %s" kind (majutsu--transient-entry-display existing)))
             (let ((entry (majutsu--transient-entry-create
                           :change-id change :commit-id commit)))
               (let ((overlay-section (or section (majutsu--find-log-entry-section change commit))))
                 (majutsu--transient-entry-apply-overlay entry overlay-section face label))
               (set collection-var (append entries (list entry)))
               (message "Added %s: %s" kind (majutsu--transient-entry-display entry))))))))))

(cl-defun majutsu--transient-select-refset (&key kind label face collection-var)
  "Shared helper for `<REFSET>' style single selections."
  (majutsu--transient--toggle-selection
   :kind kind :label label :face face
   :collection-var collection-var :type 'single))

(cl-defun majutsu--transient-toggle-refsets (&key kind label face collection-var)
  "Shared helper for `<REFSETS>' style multi selections."
  (majutsu--transient--toggle-selection
   :kind kind :label label :face face
   :collection-var collection-var :type 'multi))

(defvar-local majutsu-new-parents nil
  "List of selected parent entries for jj new.")

(defvar-local majutsu-new-after nil
  "List of selected --after entries for jj new.")

(defvar-local majutsu-new-before nil
  "List of selected --before entries for jj new.")

(defvar-local majutsu-new-message nil
  "Cached commit message for jj new transient.")

(defvar-local majutsu-new-no-edit nil
  "Non-nil when jj new should pass --no-edit.")

(defun majutsu-new--message-preview ()
  "Return a truncated preview of `majutsu-new-message'."
  (when majutsu-new-message
    (truncate-string-to-width majutsu-new-message 30 nil nil "...")))

(defun majutsu-new--selection-summary ()
  "Return a list summarizing the current jj new selections."
  (let (parts)
    (when majutsu-new-parents
      (push (format "Parents: %s"
                    (string-join (mapcar #'majutsu--transient-entry-display majutsu-new-parents)
                                 ", "))
            parts))
    (when majutsu-new-after
      (push (format "After: %s"
                    (string-join (mapcar #'majutsu--transient-entry-display majutsu-new-after)
                                 ", "))
            parts))
    (when majutsu-new-before
      (push (format "Before: %s"
                    (string-join (mapcar #'majutsu--transient-entry-display majutsu-new-before)
                                 ", "))
            parts))
    (when majutsu-new-message
      (push (format "Message: %s" (majutsu-new--message-preview)) parts))
    (when majutsu-new-no-edit
      (push "--no-edit" parts))
    (nreverse parts)))

(defun majutsu-new--description ()
  "Compose the transient description for jj new selections."
  (let ((parts (majutsu-new--selection-summary)))
    (if parts
        (concat "JJ New | " (string-join parts " | "))
      "JJ New")))

(defun majutsu-new--action-summary ()
  "Return a short summary string for the jj new execute action."
  (let ((parts (majutsu-new--selection-summary)))
    (if parts
        (string-join parts " | ")
      "Parents: @")))

;;;###autoload
(defun majutsu-new-clear-selections ()
  "Clear all jj new selections and overlays."
  (interactive)
  (majutsu--transient-clear-overlays majutsu-new-parents)
  (majutsu--transient-clear-overlays majutsu-new-after)
  (majutsu--transient-clear-overlays majutsu-new-before)
  (setq majutsu-new-parents nil
        majutsu-new-after nil
        majutsu-new-before nil
        majutsu-new-message nil
        majutsu-new-no-edit nil)
  (when (called-interactively-p 'interactive)
    (message "Cleared all jj new selections")))

;;;###autoload
(defun majutsu-new-toggle-parent ()
  "Toggle the commit at point as a jj new parent."
  (interactive)
  (majutsu--transient-toggle-refsets
   :kind "parent"
   :label "[PARENT]"
   :face '(:background "dark orange" :foreground "black")
   :collection-var 'majutsu-new-parents))

;;;###autoload
(defun majutsu-new-toggle-after ()
  "Toggle the commit at point as a jj new --after target."
  (interactive)
  (majutsu--transient-toggle-refsets
   :kind "--after"
   :label "[AFTER]"
   :face '(:background "dark blue" :foreground "white")
   :collection-var 'majutsu-new-after))

;;;###autoload
(defun majutsu-new-toggle-before ()
  "Toggle the commit at point as a jj new --before target."
  (interactive)
  (majutsu--transient-toggle-refsets
   :kind "--before"
   :label "[BEFORE]"
   :face '(:background "dark magenta" :foreground "white")
   :collection-var 'majutsu-new-before))

;;;###autoload
(defun majutsu-new-edit-message ()
  "Prompt for a jj new commit message."
  (interactive)
  (let ((input (read-string "New change message (empty to clear): " majutsu-new-message)))
    (if (string-empty-p input)
        (setq majutsu-new-message nil)
      (setq majutsu-new-message input))
    (message (if majutsu-new-message
                 "Set message for jj new"
               "Cleared message for jj new"))))

;;;###autoload
(defun majutsu-new-toggle-no-edit ()
  "Toggle passing --no-edit to jj new."
  (interactive)
  (setq majutsu-new-no-edit (not majutsu-new-no-edit))
  (message "jj new --no-edit %s"
           (if majutsu-new-no-edit "enabled" "disabled")))

(defun majutsu--transient-normalize-revsets (items)
  "Convert ITEMS (entries or strings) into a list of clean revset strings."
  (seq-filter (lambda (rev) (and rev (not (string-empty-p rev))))
              (mapcar (lambda (item)
                        (cond
                         ((majutsu--transient-entry-p item)
                          (majutsu--transient-entry-revset item))
                         ((stringp item)
                          (substring-no-properties item))
                         (t nil)))
                      items)))

(cl-defun majutsu-new--build-args (&key parents after before message (no-edit majutsu-new-no-edit))
  "Build the argument list for jj new.
PARENTS, AFTER, BEFORE, MESSAGE, and NO-EDIT default to transient state."
  (let* ((parents (majutsu--transient-normalize-revsets (or parents majutsu-new-parents)))
         (after (majutsu--transient-normalize-revsets (or after majutsu-new-after)))
         (before (majutsu--transient-normalize-revsets (or before majutsu-new-before)))
         (message (or message majutsu-new-message))
         (args '("new")))
    (dolist (rev after)
      (setq args (append args (list "--after" rev))))
    (dolist (rev before)
      (setq args (append args (list "--before" rev))))
    (when (and message (not (string-empty-p message)))
      (setq args (append args (list "--message" message))))
    (when no-edit
      (setq args (append args '("--no-edit"))))
    (setq args (append args (or parents '("@"))))
    args))

(defun majutsu-new--run-command (args)
  "Execute jj new with ARGS and refresh the log on success."
  (let ((result (apply #'majutsu--run-command args)))
    (when (majutsu--handle-command-result
           args result
           "Created new changeset"
           "Failed to create new changeset")
      (majutsu-log-refresh)
      (majutsu-goto-current)
      t)))

;;;###autoload
(defun majutsu-new-execute ()
  "Execute jj new using the current transient selections."
  (interactive)
  (let ((args (majutsu-new--build-args)))
    (when (majutsu-new--run-command args)
      (majutsu-new-clear-selections))))

;;;###autoload
(defun majutsu-new-transient ()
  "Open the jj new transient."
  (interactive)
  (add-hook 'transient-exit-hook 'majutsu-new-cleanup-on-exit nil t)
  (majutsu-new-transient--internal))

(defun majutsu-new-cleanup-on-exit ()
  "Clean up jj new selections when the transient exits."
  (majutsu-new-clear-selections)
  (remove-hook 'transient-exit-hook 'majutsu-new-cleanup-on-exit t))

(transient-define-prefix majutsu-new-transient--internal ()
  "Internal transient for jj new operations."
  :transient-suffix 'transient--do-exit
  :transient-non-suffix t
  [:description majutsu-new--description
   :class transient-columns
   ["Selections"
    ("p" "Toggle parent" majutsu-new-toggle-parent
     :description (lambda ()
                    (format "Toggle parent (%d selected)"
                            (length majutsu-new-parents)))
     :transient t)
    ("a" "Toggle --after" majutsu-new-toggle-after
     :description (lambda ()
                    (format "Toggle --after (%d selected)"
                            (length majutsu-new-after)))
     :transient t)
    ("b" "Toggle --before" majutsu-new-toggle-before
     :description (lambda ()
                    (format "Toggle --before (%d selected)"
                            (length majutsu-new-before)))
     :transient t)
    ("c" "Clear selections" majutsu-new-clear-selections
     :transient t)]
   ["Options"
    ("m" "Set message" majutsu-new-edit-message
     :description (lambda ()
                    (if majutsu-new-message
                        (format "Set message (%s)"
                                (majutsu-new--message-preview))
                      "Set message"))
     :transient t)
    ("e" "Toggle --no-edit" majutsu-new-toggle-no-edit
     :description (lambda ()
                    (if majutsu-new-no-edit
                        "--no-edit (enabled)"
                      "--no-edit (disabled)"))
     :transient t)]
   ["Actions"
    ("n" "Create new change" majutsu-new-execute
     :description (lambda ()
                    (format "Create new change (%s)"
                            (majutsu-new--action-summary)))
     :transient nil)
    ("RET" "Create new change" majutsu-new-execute
     :description (lambda ()
                    (format "Create new change (%s)"
                            (majutsu-new--action-summary)))
     :transient nil)
    ("q" "Quit" transient-quit-one)]])

;; Rebase state management
(defvar-local majutsu-rebase-source nil
  "List containing at most one entry struct for the rebase source.")

(defvar-local majutsu-rebase-destinations nil
  "List of entry structs selected as rebase destinations.")

;;;###autoload
(defun majutsu-rebase-clear-selections ()
  "Clear all rebase selections and overlays."
  (interactive)
  (majutsu--transient-clear-overlays majutsu-rebase-source)
  (majutsu--transient-clear-overlays majutsu-rebase-destinations)
  (setq majutsu-rebase-source nil
        majutsu-rebase-destinations nil)
  (message "Cleared all rebase selections"))

(defun majutsu-rebase--source-entry ()
  "Return the entry selected as rebase source, if any."
  (car majutsu-rebase-source))

(defun majutsu-rebase--destination-revsets ()
  "Return the list of destination revsets."
  (mapcar #'majutsu--transient-entry-revset majutsu-rebase-destinations))

(defun majutsu-rebase--destination-display ()
  "Return a comma-separated string for destination display."
  (string-join (mapcar #'majutsu--transient-entry-display majutsu-rebase-destinations) ", "))

(defun majutsu-rebase--source-display ()
  "Return a display string for the current source."
  (when-let ((entry (majutsu-rebase--source-entry)))
    (majutsu--transient-entry-display entry)))

;;;###autoload
(defun majutsu-rebase-set-source ()
  "Set the commit at point as rebase source."
  (interactive)
  (majutsu--transient-select-refset
   :kind "source"
   :label "[SOURCE]"
   :face '(:background "dark green" :foreground "white")
   :collection-var 'majutsu-rebase-source))

;;;###autoload
(defun majutsu-rebase-toggle-destination ()
  "Toggle the commit at point as a rebase destination."
  (interactive)
  (majutsu--transient-toggle-refsets
   :kind "destination"
   :label "[DEST]"
   :face '(:background "dark blue" :foreground "white")
   :collection-var 'majutsu-rebase-destinations))

;;;###autoload
(defun majutsu-rebase-execute ()
  "Execute rebase with selected source and destinations."
  (interactive)
  (if (and (majutsu-rebase--source-entry) majutsu-rebase-destinations)
      (let* ((source-entry (majutsu-rebase--source-entry))
             (source-rev (majutsu--transient-entry-revset source-entry))
             (source-display (majutsu--transient-entry-display source-entry))
             (dest-revs (seq-filter (lambda (rev) (and rev (not (string-empty-p rev))))
                                    (majutsu-rebase--destination-revsets)))
             (dest-display (majutsu-rebase--destination-display)))
        (when (and source-rev dest-revs
                   (yes-or-no-p (format "Rebase %s -> %s? " source-display dest-display)))
          (let* ((dest-args (apply #'append (mapcar (lambda (dest) (list "-d" dest)) dest-revs)))
                 (all-args (append (list "rebase" "-s" source-rev) dest-args))
                 (progress-msg (format "Rebasing %s onto %s" source-display dest-display))
                 (success-msg (format "Rebase completed: %s -> %s" source-display dest-display)))
            (majutsu--message-with-log "%s..." progress-msg)
            (let ((result (apply #'majutsu--run-command all-args)))
              (if (majutsu--handle-command-result all-args result success-msg "Rebase failed")
                  (progn
                    (majutsu-rebase-clear-selections)
                    (majutsu-log-refresh)))))))
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
             (when-let ((source (majutsu-rebase--source-display)))
               (format " | Source: %s" source))
             (when majutsu-rebase-destinations
               (format " | Destinations: %s" (majutsu-rebase--destination-display)))))
   :class transient-columns
   ["Selection"
    ("s" "Set source" majutsu-rebase-set-source
     :description (lambda ()
                    (if (majutsu-rebase--source-entry)
                        (format "Set source (current: %s)" (majutsu-rebase--source-display))
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
                    (if (and (majutsu-rebase--source-entry) majutsu-rebase-destinations)
                        (format "Rebase %s -> %s"
                                (majutsu-rebase--source-display)
                                (majutsu-rebase--destination-display))
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
