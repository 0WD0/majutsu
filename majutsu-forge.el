;;; majutsu-forge.el --- Forge integration for Majutsu  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Optional integration between Majutsu and Forge.
;;
;; This library reuses Forge's topic database, commands, and section
;; renderers in Majutsu log buffers.  In a colocated jj workspace, Forge is
;; initialized from the underlying Git worktree so sibling JJ workspaces need
;; not contain a `.git' directory.

;;; Code:

(require 'cl-lib)

(require 'majutsu)
(require 'majutsu-log)
(require 'majutsu-mode)
(require 'magit-section)

(declare-function forge-db "forge-db" (&optional livep))
(declare-function forge-dispatch "forge-commands")
(declare-function forge-insert-discussions "forge-discussion" (&optional spec heading))
(declare-function forge-insert-issues "forge-issue" (&optional spec heading))
(declare-function forge-insert-pullreqs "forge-pullreq" (&optional spec heading))
(declare-function forge-list-discussions "forge-discussion")
(declare-function forge-list-issues "forge-issue")
(declare-function forge-list-pullreqs "forge-pullreq")
(declare-function forge-refresh-buffer "forge-core" (&optional buffer))
(declare-function forge-set-buffer-repository "forge-repo")
(declare-function forge-visit-this-repository "forge-commands")
(declare-function forge-visit-this-topic "forge-commands")
(declare-function forge--init-buffer-topics-spec "forge-topic")
(declare-function forge--insert-pullreq-commits "forge-pullreq" (pullreq &optional all))

(defvar forge-discussion-section-map)
(defvar forge-discussions-section-map)
(defvar forge-issue-section-map)
(defvar forge-issues-section-map)
(defvar forge-pullreq-section-map)
(defvar forge-pullreqs-section-map)
(defvar forge-repository-section-map)
(defvar magit--right-margin-config)
(defvar magit-root-section)
(defvar magit-section-initial-visibility-alist)
(defvar magit-status-margin)

(defvar majutsu-forge--sections-installed nil
  "Non-nil when `majutsu-forge-mode' completed section hook setup.")

(defvar majutsu-forge--installed-section-hooks nil
  "Section hook functions installed by `majutsu-forge-mode'.

This only records entries that were absent before the mode was enabled, so
cleanup does not remove an identical hook function installed by the user.")

(defvar majutsu-forge--installed-hooks nil
  "Ordinary hook entries installed by `majutsu-forge-mode'.

Each entry is a cons cell of the form (HOOK . FUNCTION).")

(defvar majutsu-forge--installed-advices nil
  "Advice entries installed by `majutsu-forge-mode'.

Each entry is a cons cell of the form (SYMBOL . FUNCTION).")

(defvar majutsu-forge--refreshing nil
  "Non-nil while Forge advice is refreshing Majutsu buffers.")

(defvar majutsu-forge--pending-refresh-roots nil
  "Repository roots queued by reentrant Forge refreshes.")

(defvar majutsu-forge--bindings-installed nil
  "Non-nil when `majutsu-forge-mode' installed key bindings.")

(defconst majutsu-forge--no-binding :majutsu-forge-no-binding
  "Marker used for keymaps that had no local binding before modification.")

(defvar majutsu-forge--saved-bindings nil
  "Key bindings changed by `majutsu-forge-mode'.")

(defvar-local majutsu-forge-section-errors nil
  "Section insertion errors suppressed in the current Majutsu buffer.")

;;; Options

(defgroup majutsu-forge nil
  "Forge integration for Majutsu."
  :group 'majutsu)

(defcustom majutsu-forge-add-default-sections t
  "Whether `majutsu-forge-mode' adds Forge topic sections to Majutsu logs."
  :type 'boolean
  :group 'majutsu-forge)

(defcustom majutsu-forge-add-default-bindings t
  "Whether `majutsu-forge-mode' adds Forge bindings to Majutsu keymaps."
  :type 'boolean
  :group 'majutsu-forge)

(defcustom majutsu-forge-show-section-errors nil
  "Whether Forge section insertion errors should be shown in the echo area.

When nil, errors while inserting Forge sections are ignored.  This keeps
Majutsu buffers usable in repositories where Forge cannot determine a Git
repository, remote, or tracked Forge repository."
  :type 'boolean
  :group 'majutsu-forge)

(defcustom majutsu-forge-suppress-pullreq-commits t
  "Whether to suppress Forge's Git log body inside pull-request sections.

Forge renders commits below unmerged pull requests with Magit's Git log
renderer.  That is not JJ-aware, so Majutsu hides that nested commit list by
default.  The pull-request section itself remains visitable."
  :type 'boolean
  :group 'majutsu-forge)

(defcustom majutsu-forge-expand-sections t
  "Whether Forge topic list sections are expanded when inserted in Majutsu."
  :type 'boolean
  :group 'majutsu-forge)

;;; Loading

(defun majutsu-forge--require ()
  "Load Forge or signal a user error."
  (unless (require 'forge nil t)
    (user-error "Majutsu Forge integration requires the `forge' package"))
  t)

;;; Buffer setup

(defun majutsu-forge--git-worktree ()
  "Return the Git worktree paired with the current JJ workspace, if any."
  (when-let* ((git-dir (car (ignore-errors (majutsu-jj-lines "git" "root"))))
              (git-dir (majutsu-jj-expand-directory-from-jj git-dir default-directory))
              ((file-directory-p git-dir)))
    (file-name-directory (directory-file-name git-dir))))

(defmacro majutsu-forge--with-git-worktree (&rest body)
  "Evaluate BODY from the current workspace's underlying Git worktree."
  (declare (indent 0) (debug (body)))
  `(let ((default-directory (or (majutsu-forge--git-worktree)
                                default-directory)))
     ,@body))

(defun majutsu-forge--connect-database-once ()
  "Connect the Forge database the first time a Majutsu log buffer is used."
  (remove-hook 'majutsu-log-mode-hook #'majutsu-forge--connect-database-once)
  (when (featurep 'forge)
    (ignore-errors (forge-db))))

(defun majutsu-forge--resolve-margin-config (config)
  "Return a copy of Magit margin CONFIG with a numeric width."
  (when config
    (let ((config (copy-sequence config)))
      (when (functionp (nth 2 config))
        (setf (nth 2 config)
              (funcall (nth 2 config) (nth 1 config) (nth 3 config) (nth 4 config))))
      config)))

(defun majutsu-forge--margin-config ()
  "Return a Magit right-margin config usable while Forge inserts topics."
  (majutsu-forge--resolve-margin-config
   (or (and (boundp 'magit--right-margin-config)
            magit--right-margin-config)
       (and (boundp 'magit-status-margin)
            (symbol-value 'magit-status-margin)))))

(defun majutsu-forge--init-buffer ()
  "Initialize Forge buffer-local state for the current Majutsu buffer."
  (when (featurep 'forge)
    (setq-local magit--right-margin-config (majutsu-forge--margin-config))
    (majutsu-forge--with-git-worktree
      (when (fboundp 'forge--init-buffer-topics-spec)
        (ignore-errors (forge--init-buffer-topics-spec)))
      (when (fboundp 'forge-set-buffer-repository)
        (ignore-errors (forge-set-buffer-repository))))))

(defun majutsu-forge--ensure-buffer ()
  "Initialize Forge state needed by topic section insertion."
  (when (featurep 'forge)
    (ignore-errors (forge-db))
    (majutsu-forge--init-buffer)))

;;; Sections

(defun majutsu-forge--record-section-error (section error)
  "Record ERROR raised while inserting Forge SECTION."
  (let ((message (error-message-string error)))
    (push (cons section message) majutsu-forge-section-errors)
    (majutsu--debug "Forge: cannot insert %s: %s" section message)
    (when majutsu-forge-show-section-errors
      (message "Majutsu Forge: cannot insert %s: %s" section message))))

(defmacro majutsu-forge--with-section-errors (section &rest body)
  "Evaluate BODY, handling Forge insertion errors for SECTION."
  (declare (indent 1) (debug (form body)))
  `(let ((start (copy-marker (point)))
         (end (copy-marker (point) t)))
     (unwind-protect
         (condition-case-unless-debug err
             (let ((magit--right-margin-config (majutsu-forge--margin-config))
                   (magit-section-initial-visibility-alist
                    (append '((pullreqs . show)
                              (issues . show)
                              (discussions . show))
                            magit-section-initial-visibility-alist)))
               (majutsu-forge--ensure-buffer)
               ,@body)
           (error
            (let ((inhibit-read-only t))
              (delete-region start end))
            (majutsu-forge--record-section-error ,section err)
            nil))
       (set-marker start nil)
       (set-marker end nil))))

(defun majutsu-forge--insert-pullreq-commits (_pullreq &optional _all)
  "Placeholder used instead of Forge's Magit-specific PR commit renderer."
  (let ((inhibit-read-only t))
    (insert (propertize "Commit list is not shown in Majutsu Forge sections.\n"
                        'font-lock-face 'magit-dimmed))))

(defun majutsu-forge--insert-pullreq-commits-around (fn pullreq &rest args)
  "Call FN with PULLREQ and ARGS unless Majutsu should hide its commit list."
  (if (and majutsu-forge-suppress-pullreq-commits
           (derived-mode-p 'majutsu-log-mode))
      (majutsu-forge--insert-pullreq-commits pullreq (car args))
    (apply fn pullreq args)))

(defun majutsu-forge--show-section (type)
  "Show the Forge topic list section of TYPE when requested."
  (when (and majutsu-forge-expand-sections
             (bound-and-true-p magit-root-section))
    (magit-map-sections
     (lambda (section)
       (when (eq (oref section type) type)
         (magit-section-show section)))
     magit-root-section)))

(defun majutsu-forge--clear-section-errors ()
  "Clear suppressed Forge section errors before rendering Forge sections."
  (setq majutsu-forge-section-errors nil))

(defun majutsu-forge--expand-sections ()
  "Expand Forge topic list sections after a Majutsu refresh."
  (when (derived-mode-p 'majutsu-log-mode)
    (let ((inhibit-read-only t))
      (dolist (type '(pullreqs issues discussions))
        (majutsu-forge--show-section type)))))

;;;###autoload
(defun majutsu-forge-insert-pullreqs ()
  "Insert Forge pull request sections into a Majutsu log buffer."
  (majutsu-forge--with-section-errors "pull requests"
    (forge-insert-pullreqs)))

;;;###autoload
(defun majutsu-forge-insert-issues ()
  "Insert Forge issue sections into a Majutsu log buffer."
  (majutsu-forge--with-section-errors "issues"
    (forge-insert-issues)))

;;;###autoload
(defun majutsu-forge-insert-discussions ()
  "Insert Forge discussion sections into a Majutsu log buffer."
  (majutsu-forge--with-section-errors "discussions"
    (forge-insert-discussions)))

(defun majutsu-forge--default-hook-member-p (hook function)
  "Return non-nil when FUNCTION is in HOOK's default value."
  (let ((value (default-value hook)))
    (or (eq value function)
        (and (listp value) (memq function value)))))

(defun majutsu-forge--add-owned-hook (hook function)
  "Add FUNCTION to HOOK and remember when Majutsu owns the entry."
  (unless (majutsu-forge--default-hook-member-p hook function)
    (unwind-protect
        (add-hook hook function)
      ;; Record an entry even if a wrapper around `add-hook' signals after
      ;; modifying the hook.  The mode's error path can then roll it back.
      (when (majutsu-forge--default-hook-member-p hook function)
        (cl-pushnew (cons hook function)
                    majutsu-forge--installed-hooks
                    :test #'equal)))))

(defun majutsu-forge--remove-owned-hooks ()
  "Remove ordinary hook entries installed by `majutsu-forge-mode'."
  (dolist (entry majutsu-forge--installed-hooks)
    (remove-hook (car entry) (cdr entry)))
  (setq majutsu-forge--installed-hooks nil))

(defun majutsu-forge--add-owned-section-hook (function)
  "Add section hook FUNCTION and remember when Majutsu owns the entry."
  (unless (majutsu-forge--default-hook-member-p
           'majutsu-log-sections-hook function)
    (unwind-protect
        (magit-add-section-hook 'majutsu-log-sections-hook function nil t)
      ;; As above, make a partially successful installation reversible.
      (when (majutsu-forge--default-hook-member-p
             'majutsu-log-sections-hook function)
        (cl-pushnew function majutsu-forge--installed-section-hooks)))))

(defun majutsu-forge--add-section-hooks ()
  "Add Forge section hooks to `majutsu-log-sections-hook'."
  (dolist (function '(majutsu-forge--clear-section-errors
                      majutsu-forge-insert-pullreqs
                      majutsu-forge-insert-issues
                      majutsu-forge-insert-discussions))
    (majutsu-forge--add-owned-section-hook function))
  (setq majutsu-forge--sections-installed t))

(defun majutsu-forge--remove-section-hooks ()
  "Remove section hooks installed by `majutsu-forge-mode'."
  (dolist (function majutsu-forge--installed-section-hooks)
    (remove-hook 'majutsu-log-sections-hook function))
  (setq majutsu-forge--installed-section-hooks nil
        majutsu-forge--sections-installed nil))

;;; Bindings

(defun majutsu-forge--local-binding (map key)
  "Return MAP's local binding for KEY, ignoring its parent keymap."
  (let* ((keymap (and (boundp map) (symbol-value map)))
         (parent (and (keymapp keymap) (keymap-parent keymap))))
    (when (keymapp keymap)
      (unwind-protect
          (progn
            (set-keymap-parent keymap nil)
            (keymap-lookup keymap key))
        (set-keymap-parent keymap parent)))))

(defun majutsu-forge--save-binding (map key command)
  "Remember MAP's current binding for KEY before installing COMMAND."
  (unless (assoc (list map key) majutsu-forge--saved-bindings)
    (let ((binding (majutsu-forge--local-binding map key)))
      (push (list (list map key)
                  (if (integerp binding)
                      majutsu-forge--no-binding
                    binding)
                  command)
            majutsu-forge--saved-bindings))))

(defun majutsu-forge--set-key (map key command)
  "Bind KEY in MAP to COMMAND when MAP and COMMAND are available."
  (when (and (boundp map) (keymapp (symbol-value map)) (fboundp command))
    (majutsu-forge--save-binding map key command)
    (keymap-set (symbol-value map) key command)))

(defun majutsu-forge--restore-bindings ()
  "Restore key bindings changed by `majutsu-forge-mode'."
  (pcase-dolist (`((,map ,key) ,binding ,installed)
                 majutsu-forge--saved-bindings)
    (when (and (boundp map)
               (keymapp (symbol-value map))
               (equal (majutsu-forge--local-binding map key) installed))
      (define-key (symbol-value map)
                  (key-parse key)
                  (unless (eq binding majutsu-forge--no-binding)
                    binding))))
  (setq majutsu-forge--saved-bindings nil))

(defun majutsu-forge--add-section-remaps ()
  "Teach Forge section keymaps about Majutsu placeholder commands."
  (majutsu-forge--set-key 'forge-pullreqs-section-map
                          "<remap> <majutsu-visit-thing>"
                          'forge-list-pullreqs)
  (majutsu-forge--set-key 'forge-pullreq-section-map
                          "<remap> <majutsu-visit-thing>"
                          'forge-visit-this-topic)
  (majutsu-forge--set-key 'forge-issues-section-map
                          "<remap> <majutsu-visit-thing>"
                          'forge-list-issues)
  (majutsu-forge--set-key 'forge-issue-section-map
                          "<remap> <majutsu-visit-thing>"
                          'forge-visit-this-topic)
  (majutsu-forge--set-key 'forge-discussions-section-map
                          "<remap> <majutsu-visit-thing>"
                          'forge-list-discussions)
  (majutsu-forge--set-key 'forge-discussion-section-map
                          "<remap> <majutsu-visit-thing>"
                          'forge-visit-this-topic)
  (majutsu-forge--set-key 'forge-repository-section-map
                          "<remap> <majutsu-visit-thing>"
                          'forge-visit-this-repository))

;;;###autoload
(defun majutsu-forge-dispatch ()
  "Dispatch a Forge command from Majutsu."
  (interactive)
  (majutsu-forge--require)
  (majutsu-forge--with-git-worktree
    (call-interactively #'forge-dispatch)))

(defun majutsu-forge--add-mode-bindings ()
  "Add Forge bindings to `majutsu-mode-map'."
  (majutsu-forge--set-key 'majutsu-mode-map "N" #'majutsu-forge-dispatch)
  (majutsu-forge--set-key 'majutsu-mode-map "'" #'majutsu-forge-dispatch)
  (majutsu-forge--add-section-remaps)
  (setq majutsu-forge--bindings-installed t))

(defun majutsu-forge--remove-mode-bindings ()
  "Remove Forge bindings from `majutsu-mode-map'."
  (majutsu-forge--restore-bindings)
  (setq majutsu-forge--bindings-installed nil))

;;; Refresh

(defun majutsu-forge--buffer-root (&optional buffer)
  "Return the Majutsu root associated with BUFFER, if any."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (or (and (derived-mode-p 'majutsu-mode)
                 majutsu--default-directory)
            (ignore-errors (majutsu--toplevel-safe default-directory)))))))

(defun majutsu-forge--same-root-p (root other)
  "Return non-nil if ROOT and OTHER name the same repository root."
  (and root other
       (ignore-errors
         (file-equal-p (file-truename root) (file-truename other)))))

(defun majutsu-forge--refresh-majutsu-buffers (&optional root)
  "Refresh live Majutsu buffers under ROOT or any root in a list.

If ROOT is nil or a list containing nil, refresh all live Majutsu buffers."
  (let ((roots (cond ((null root) nil)
                     ((listp root) root)
                     (t (list root)))))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (derived-mode-p 'majutsu-mode)
                     (or (not roots)
                         (memq nil roots)
                         (cl-some (lambda (candidate)
                                    (majutsu-forge--same-root-p
                                     candidate majutsu--default-directory))
                                  roots)))
            (ignore-errors (majutsu-refresh-buffer))))))))

(defun majutsu-forge--request-majutsu-refresh (root)
  "Refresh Majutsu buffers for ROOT without recursive refresh fanout.

Requests made while a refresh is running are coalesced by repository root.
After the initial pass, all pending roots share one supplemental pass; requests
made during that supplemental pass are deliberately dropped."
  (if majutsu-forge--refreshing
      (cl-pushnew root majutsu-forge--pending-refresh-roots :test #'equal)
    (let ((majutsu-forge--refreshing t)
          (majutsu-forge--pending-refresh-roots nil))
      (unwind-protect
          (progn
            (majutsu-forge--refresh-majutsu-buffers root)
            (let ((pending (nreverse majutsu-forge--pending-refresh-roots)))
              ;; Clear the queue before the supplemental pass.  Nested
              ;; requests can set it again, but the unwind cleanup below
              ;; drops them instead of allowing an unbounded retry loop.
              (setq majutsu-forge--pending-refresh-roots nil)
              (when pending
                (majutsu-forge--refresh-majutsu-buffers pending))))
        (setq majutsu-forge--pending-refresh-roots nil)))))

(defun majutsu-forge--after-forge-refresh (&optional buffer)
  "Refresh Majutsu buffers after `forge-refresh-buffer' refreshes BUFFER."
  ;; Forge handles a non-nil BUFFER by recursively calling itself without an
  ;; argument inside that buffer.  The inner call performs the refresh and
  ;; invokes this advice, so handling the outer call too would refresh every
  ;; matching Majutsu buffer twice.
  (unless buffer
    (when-let* ((root (majutsu-forge--buffer-root)))
      (majutsu-forge--request-majutsu-refresh root))))

;;; Advice

(defun majutsu-forge--add-owned-advice (symbol where function)
  "Advise SYMBOL at WHERE with FUNCTION and record Majutsu ownership."
  (unless (advice-member-p function symbol)
    (unwind-protect
        (advice-add symbol where function)
      ;; Preserve enough state to undo an advice installer that signals after
      ;; changing the function definition.
      (when (advice-member-p function symbol)
        (cl-pushnew (cons symbol function)
                    majutsu-forge--installed-advices
                    :test #'equal)))))

(defun majutsu-forge--add-advices ()
  "Install Forge advice used by `majutsu-forge-mode'."
  (when (fboundp 'forge-refresh-buffer)
    (majutsu-forge--add-owned-advice
     'forge-refresh-buffer :after #'majutsu-forge--after-forge-refresh))
  (when (fboundp 'forge--insert-pullreq-commits)
    (majutsu-forge--add-owned-advice
     'forge--insert-pullreq-commits :around
     #'majutsu-forge--insert-pullreq-commits-around)))

(defun majutsu-forge--remove-advices ()
  "Remove Forge advice installed by `majutsu-forge-mode'."
  (dolist (entry majutsu-forge--installed-advices)
    (when (fboundp (car entry))
      (advice-remove (car entry) (cdr entry))))
  (setq majutsu-forge--installed-advices nil))

(defun majutsu-forge--cleanup-installation ()
  "Remove global state installed by `majutsu-forge-mode'."
  (when (or majutsu-forge--sections-installed
            majutsu-forge--installed-section-hooks)
    (majutsu-forge--remove-section-hooks))
  (majutsu-forge--remove-owned-hooks)
  (when majutsu-forge--bindings-installed
    (majutsu-forge--remove-mode-bindings))
  (majutsu-forge--restore-bindings)
  (setq majutsu-forge--bindings-installed nil)
  (majutsu-forge--remove-advices))

;;; Minor mode

;;;###autoload
(define-minor-mode majutsu-forge-mode
  "Toggle Forge integration in Majutsu buffers.

This mode reuses Forge's database, commands, and topic sections in Majutsu
log buffers.  It is intended to work best in colocated jj/Git repositories."
  :global t
  :group 'majutsu-forge
  (if majutsu-forge-mode
      (condition-case err
          (progn
            (majutsu-forge--require)
            (when majutsu-forge-add-default-sections
              (majutsu-forge--add-section-hooks)
              (majutsu-forge--add-owned-hook
               'majutsu-log-mode-hook #'majutsu-forge--connect-database-once)
              (majutsu-forge--add-owned-hook
               'majutsu-log-mode-hook #'majutsu-forge--init-buffer)
              (majutsu-forge--add-owned-hook
               'majutsu-refresh-buffer-hook #'majutsu-forge--expand-sections))
            (when majutsu-forge-add-default-bindings
              (majutsu-forge--add-mode-bindings))
            (majutsu-forge--add-advices)
            (majutsu-forge--request-majutsu-refresh nil))
        (error
         (majutsu-forge--cleanup-installation)
         (setq majutsu-forge-mode nil)
         (signal (car err) (cdr err))))
    (majutsu-forge--cleanup-installation)
    (majutsu-forge--request-majutsu-refresh nil)))

(provide 'majutsu-forge)
;;; majutsu-forge.el ends here
