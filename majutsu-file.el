;;; majutsu-file.el --- Finding files  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Portions of blob/file visiting workflow are adapted from:
;; - Magit `lisp/magit-files.el` (commit c800f79c2061621fde847f6a53129eca0e8da728)
;;   Copyright (C) 2008-2026 The Magit Project Contributors

;;; Commentary:

;; Support jj file commands and blob buffers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'majutsu)

(declare-function magit-find-file "magit-files" (rev file))

(defvar majutsu-find-file-hook nil
  "Hook run after creating a blob buffer.")

(defvar majutsu-file--list-cache nil
  "Alist cache of file lists keyed by revset string.")

(defvar-local majutsu-buffer-blob-revset nil
  "Input revset string for this blob buffer.")
(defvar-local majutsu-buffer-blob-revision nil
  "Resolved single change-id for this blob buffer.")
(defvar-local majutsu-buffer-blob-change-id nil
  "Resolved single change-id for this blob buffer.")
(defvar-local majutsu-buffer-blob-commit-id nil
  "Resolved single commit-id for this blob buffer.")
(defvar-local majutsu-buffer-blob-path nil
  "Relative path for this blob buffer.")
(defvar-local majutsu-buffer-blob-root nil
  "Repository root for this blob buffer.")
(defvar-local majutsu-buffer-blob-content-hash nil
  "Hash of blob buffer content after last successful refresh.")

(put 'majutsu-buffer-blob-revset 'permanent-local t)
(put 'majutsu-buffer-blob-revision 'permanent-local t)
(put 'majutsu-buffer-blob-change-id 'permanent-local t)
(put 'majutsu-buffer-blob-commit-id 'permanent-local t)
(put 'majutsu-buffer-blob-path 'permanent-local t)
(put 'majutsu-buffer-blob-root 'permanent-local t)
(put 'majutsu-buffer-blob-content-hash 'permanent-local t)

(defvar-local majutsu-blob-edit--original-content nil
  "Original blob content snapshot before entering editable mode.")

(defvar-local majutsu-blob-edit--original-point nil
  "Point position snapshot before entering editable blob mode.")

(defcustom majutsu-blob-edit-cursor-type 'hollow
  "Cursor type used while `majutsu-blob-edit-mode' is active."
  :group 'majutsu
  :type 'symbol)

(defvar-local majutsu-blob-edit--saved-cursor-type nil
  "Cursor type before entering `majutsu-blob-edit-mode'.")

(defvar-local majutsu-blob-edit--saved-evil-normal-state-cursor nil
  "Saved `evil-normal-state-cursor' before editable blob mode.")

(defvar-local majutsu-blob-edit--saved-evil-normal-state-cursor-valid nil
  "Whether `majutsu-blob-edit--saved-evil-normal-state-cursor' is valid.")

(defvar-local majutsu-blob-edit--saved-blob-mode-enabled nil
  "Whether `majutsu-blob-mode' was enabled before entering editable mode.")

(defun majutsu-blob-edit--set-cursor-hint (cursor)
  "Set editable-mode cursor hint to CURSOR.
When Evil is active, also update `evil-normal-state-cursor'."
  (setq-local cursor-type cursor)
  (when (and (bound-and-true-p evil-local-mode)
             (boundp 'evil-normal-state-cursor))
    (setq-local evil-normal-state-cursor cursor)))

(defun majutsu-blob-edit--save-cursor-state ()
  "Save current cursor state before entering editable blob mode."
  (setq-local majutsu-blob-edit--saved-cursor-type cursor-type)
  (if (and (bound-and-true-p evil-local-mode)
           (boundp 'evil-normal-state-cursor))
      (progn
        (setq-local majutsu-blob-edit--saved-evil-normal-state-cursor evil-normal-state-cursor)
        (setq-local majutsu-blob-edit--saved-evil-normal-state-cursor-valid t))
    (setq-local majutsu-blob-edit--saved-evil-normal-state-cursor-valid nil)))

(defun majutsu-blob-edit--restore-cursor-state ()
  "Restore cursor state after leaving editable blob mode."
  (setq-local cursor-type majutsu-blob-edit--saved-cursor-type)
  (when (and majutsu-blob-edit--saved-evil-normal-state-cursor-valid
             (boundp 'evil-normal-state-cursor))
    (setq-local evil-normal-state-cursor majutsu-blob-edit--saved-evil-normal-state-cursor)
    (setq-local majutsu-blob-edit--saved-evil-normal-state-cursor-valid nil)))

(add-hook 'majutsu-find-file-hook #'majutsu-blob-mode)

(defun majutsu-blob-edit--apply-diffedit (content)
  "Apply CONTENT to the blob revision through non-interactive `jj diffedit'."
  (let* ((root majutsu-buffer-blob-root)
         (default-directory root)
         (file majutsu-buffer-blob-path)
         (rev majutsu-buffer-blob-revision)
         (temp-file (make-temp-file "majutsu-blob-edit-"))
         (editor-config (majutsu-jj--editor-command-config
                         "ui.diff-editor"
                         (concat "$right/" file)
                         (list "cp" temp-file)))
         (args (list "diffedit"
                     "--config" editor-config
                     "--from" (concat rev "-")
                     "--to" rev
                     "--"
                     file))
         exit)
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert content))
          (setq exit (apply #'majutsu-run-jj args)))
      (ignore-errors (delete-file temp-file)))
    exit))

(defvar-keymap majutsu-blob-edit-mode-map
  :doc "Keymap for `majutsu-blob-edit-mode'."
  "C-x C-q" #'majutsu-blob-edit-exit
  "C-c C-c" #'majutsu-blob-edit-finish
  "C-c C-k" #'majutsu-blob-edit-abort)

(defun majutsu-blob-edit--write-contents ()
  "Apply blob edits through `jj diffedit` and finish save.
This function is used from `write-contents-functions' in editable blob mode."
  (if (not majutsu-blob-edit-mode)
      nil
    (unless (and majutsu-buffer-blob-root
                 majutsu-buffer-blob-path
                 majutsu-buffer-blob-revision)
      (user-error "Missing blob context for editable save"))
    (let ((current (buffer-substring-no-properties (point-min) (point-max))))
      (if (equal current majutsu-blob-edit--original-content)
          (progn
            (set-buffer-modified-p nil)
            (majutsu-blob-edit-mode -1)
            t)
        (let ((exit (majutsu-blob-edit--apply-diffedit current)))
          (if (zerop exit)
              (progn
                (when-let* ((rev-info (majutsu-file--resolve-single-rev-info
                                       majutsu-buffer-blob-revset))
                            (new-change (plist-get rev-info :change-id)))
                  (setq majutsu-buffer-blob-revision new-change)
                  (setq majutsu-buffer-blob-change-id new-change)
                  (setq majutsu-buffer-blob-commit-id (plist-get rev-info :commit-id))
                  (majutsu-file--refresh-buffer-name))
                (setq majutsu-blob-edit--original-content current)
                (set-buffer-modified-p nil)
                (majutsu-blob-edit-mode -1)
                (setq-local majutsu-buffer-blob-content-hash
                            (secure-hash 'sha1 current))
                (message "Blob edits applied via jj diffedit")
                t)
            (user-error "Failed to apply blob edits (exit %s)" exit)))))))

(defun majutsu-blob-edit-start ()
  "Enter editable blob mode.
Edits are applied to the revision through jj diffedit when saving."
  (interactive)
  (unless (and (bound-and-true-p majutsu-blob-mode)
               majutsu-buffer-blob-root
               majutsu-buffer-blob-path
               majutsu-buffer-blob-revision)
    (user-error "Not in a blob buffer"))
  (unless majutsu-blob-edit-mode
    (majutsu-blob-edit-mode 1)))

(defun majutsu-blob-edit-finish ()
  "Save and apply editable blob changes."
  (interactive)
  (unless majutsu-blob-edit-mode
    (user-error "Blob editable mode is not active"))
  (if (majutsu-blob-edit--changed-p)
      (progn
        ;; `save-buffer' skips write hooks if modified flag is nil.
        ;; In editable blob buffers this can happen after undo-like edits,
        ;; so force the modified flag when content differs from snapshot.
        (unless (buffer-modified-p)
          (set-buffer-modified-p t))
        (save-buffer))
    (majutsu-blob-edit-mode -1)
    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)
    (message "(No changes need to be saved)")))

(defun majutsu-blob-edit--changed-p ()
  "Return non-nil when blob edit buffer content differs from snapshot."
  (not (equal (buffer-substring-no-properties (point-min) (point-max))
              (or majutsu-blob-edit--original-content ""))))

(defun majutsu-blob-edit-exit ()
  "Exit editable blob mode.
If there are unsaved changes, prompt to save or abort, like `wdired-exit'."
  (interactive)
  (unless majutsu-blob-edit-mode
    (user-error "Blob editable mode is not active"))
  (if (majutsu-blob-edit--changed-p)
      (if (y-or-n-p (format "Buffer %s modified; save changes? "
                            (current-buffer)))
          (majutsu-blob-edit-finish)
        (majutsu-blob-edit-abort))
    (majutsu-blob-edit-mode -1)
    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)
    (message "(No changes need to be saved)")))

(defun majutsu-blob-edit-abort ()
  "Abort editable blob changes and restore snapshot."
  (interactive)
  (unless majutsu-blob-edit-mode
    (user-error "Blob editable mode is not active"))
  (let ((inhibit-read-only t)
        (restore-point (or majutsu-blob-edit--original-point (point-min))))
    (erase-buffer)
    (insert (or majutsu-blob-edit--original-content ""))
    (goto-char (min restore-point (point-max)))
    (set-buffer-modified-p nil))
  (majutsu-blob-edit-mode -1)
  (message "Blob edits aborted"))

(define-minor-mode majutsu-blob-edit-mode
  "Minor mode for editable blob buffers.
Saving in this mode applies changes through `jj diffedit'."
  :lighter " BlobEdit"
  :keymap majutsu-blob-edit-mode-map
  (if majutsu-blob-edit-mode
      (progn
        (setq-local majutsu-blob-edit--original-content
                    (buffer-substring-no-properties (point-min) (point-max)))
        (setq-local majutsu-blob-edit--original-point (point))
        (setq-local majutsu-blob-edit--saved-blob-mode-enabled
                    (bound-and-true-p majutsu-blob-mode))
        (when majutsu-blob-edit--saved-blob-mode-enabled
          (majutsu-blob-mode -1))
        (majutsu-blob-edit--save-cursor-state)
        (majutsu-blob-edit--set-cursor-hint majutsu-blob-edit-cursor-type)
        (setq buffer-read-only nil)
        (add-hook 'write-contents-functions #'majutsu-blob-edit--write-contents nil t)
        (message "Editable blob mode enabled. Save to apply; C-x C-q to exit; C-c C-k to abort."))
    (remove-hook 'write-contents-functions #'majutsu-blob-edit--write-contents t)
    (majutsu-blob-edit--restore-cursor-state)
    (when majutsu-blob-edit--saved-blob-mode-enabled
      (majutsu-blob-mode 1)
      (setq-local majutsu-blob-edit--saved-blob-mode-enabled nil))
    (setq buffer-read-only t)))

(defun majutsu-file--normalize-revset (revset)
  "Normalize REVSET to a single revision using jj revset functions."
  (format "exactly(latest(%s), 1)" revset))

(defun majutsu-file--short-id (id)
  "Return a shortened version of change ID or commit ID.
Takes first 8 characters, or full string if shorter."
  (if (> (length id) 8)
      (substring id 0 8)
    id))

(defun majutsu-file--resolve-single-rev-info (revset)
  "Resolve REVSET and return revision metadata plist.
Return nil when REVSET does not resolve to a single revision.
The returned plist contains :change-id and :commit-id."
  (let* ((normalized (majutsu-file--normalize-revset revset))
         (lines (majutsu-jj-lines "log" "-r" normalized
                                 "-T" "change_id ++ \"\\n\" ++ commit_id"
                                 "--no-graph" "--limit" "1"))
         (change-id (string-trim (or (car lines) "")))
         (commit-id (string-trim (or (cadr lines) ""))))
    (unless (string-empty-p change-id)
      (list :change-id change-id
            :commit-id (unless (string-empty-p commit-id) commit-id)))))

(defun majutsu-file--resolve-single-rev (revset)
  "Resolve REVSET to a single revision string.
Uses `latest` and `exactly` to enforce a single target."
  (plist-get (majutsu-file--resolve-single-rev-info revset) :change-id))

(defun majutsu-file--resolve-single-commit (revset)
  "Resolve REVSET to a single commit string."
  (let* ((normalized (majutsu-file--normalize-revset revset))
         (result (string-trim
                  (or (majutsu-jj-string "log" "-r" normalized
                                         "-T" "commit_id" "--no-graph" "--limit" "1")
                      ""))))
    ;; Return nil when revset yields no result.
    (unless (string-empty-p result)
      result)))

(defun majutsu-file--list (revset root)
  "Return list of file paths for REVSET in ROOT.
Results are cached in `majutsu-file--list-cache`."
  (let* ((normalized (majutsu-file--normalize-revset revset))
         (cache-key (cons root normalized))
         (cached (assoc cache-key majutsu-file--list-cache)))
    (if cached
        (cdr cached)
      (let* ((default-directory root)
             (paths (majutsu-jj-lines "file" "list" "-r" normalized)))
        paths))))

(defun majutsu-file-list (&optional revset)
  "Return list of file paths for REVSET (default \"@\")."
  (majutsu-file--list (or revset "@") (majutsu-file--root)))

(defun majutsu-read-files (prompt initial-input history &optional list-fn)
  "Read multiple files with completion.
PROMPT, INITIAL-INPUT, HISTORY are standard reader args.
LIST-FN defaults to `majutsu-file-list'."
  (let ((root (majutsu-file--root)))
    (majutsu-completing-read-multiple
     prompt
     (funcall (or list-fn #'majutsu-file-list))
     nil nil
     (or initial-input (majutsu-file--path-at-point root))
     history)))

(defun majutsu-file--buffer-name (revset path)
  "Return a blob buffer name for REVSET and PATH."
  (format "%s@~%s~" path revset))

(defun majutsu-file--root ()
  "Return repo root for current buffer."
  (or (majutsu--buffer-root) (majutsu-toplevel default-directory)))

(defun majutsu-file--relative-path (root path)
  "Return PATH relative to ROOT."
  (file-relative-name (expand-file-name path root) root))

(defun majutsu-file--path-at-point (root)
  "Return path from context or nil."
  (or (magit-section-value-if 'jj-file)
      (majutsu-file-at-point)
      (when-let* ((file buffer-file-name))
        (majutsu-file--relative-path root file))))

(defun majutsu-file--read-path (revset root &optional default)
  "Prompt for a file path from REVSET.
DEFAULT is the initial file choice when present in REVSET file list."
  (let* ((paths (majutsu-file--list revset root))
         (default (or default (majutsu-file--path-at-point root))))
    (when (and default (not (member default paths)))
      (setq default nil))
    (completing-read "Find file: " paths nil t nil nil default)))

(defun majutsu-file--diff-range-value (range prefix)
  "Return the value in RANGE for argument starting with PREFIX."
  (when range
    (when-let* ((arg (seq-find (lambda (item) (string-prefix-p prefix item)) range)))
      (substring arg (length prefix)))))

(defun majutsu-file--diff-revset ()
  "Return the revset implied by the current diff buffer, if any."
  (when (derived-mode-p 'majutsu-diff-mode)
    (let* ((range majutsu-buffer-diff-range)
           (removed (eq (char-after (line-beginning-position)) ?-))
           (from (majutsu-file--diff-range-value range "--from="))
           (to (majutsu-file--diff-range-value range "--to="))
           (revisions (majutsu-file--diff-range-value range "--revisions=")))
      (cond
       ((and range (equal (car range) "-r") (cadr range)) (cadr range))
       (revisions revisions)
       (from (if (and removed from) from (or to from)))
       (t "@")))))

(defun majutsu-file--default-revset ()
  "Return default revset for the current context."
  (or (majutsu-file--diff-revset)
      (when-let* ((value (magit-section-value-if 'jj-commit)))
        (substring-no-properties value))
      "@"))

(defun majutsu-find-file-read-args (prompt)
  "Read revset and file path for PROMPT."
  (let* ((root (majutsu-file--root))
         (revset (majutsu-read-revset prompt (majutsu-file--default-revset)))
         (default-path (majutsu-file--path-at-point root))
         (path (majutsu-file--read-path revset root default-path)))
    (list revset path)))

(defun majutsu-find-file-noselect (rev file &optional revert)
  "Read FILE from REV into a buffer and return the buffer.
REV is a revset, or nil to visit the file directly from the filesystem.
FILE may be absolute, or relative to the repository root.
Non-nil REVERT means to revert the buffer.  If `ask-revert', then
only after asking.  A non-nil value for REVERT is ignored if REV is nil."
  (let* ((root (majutsu-file--root))
         (file-abs (if (file-name-absolute-p file)
                       file
                     (expand-file-name file root)))
         (file-rel (file-relative-name file-abs root))
         (defdir (file-name-directory file-abs)))
    (when (and rev (not (file-in-directory-p file-abs root)))
      (user-error "File is outside repository: %s" file))
    (if (null rev)
        ;; Visit filesystem file directly
        (progn
          (unless (file-exists-p file-abs)
            (user-error "File no longer exists: %s" file-abs))
          (find-file-noselect file-abs))
      ;; Visit blob from revision
      (let* ((rev-info (or (majutsu-file--resolve-single-rev-info rev)
                           (user-error "Revset does not resolve to a single revision")))
             (resolved (plist-get rev-info :change-id))
             (commit-id (plist-get rev-info :commit-id))
             (short-rev (majutsu-file--short-id resolved))
             (buf-name (majutsu-file--buffer-name short-rev file-rel))
             (buffer (get-buffer-create buf-name)))
        (with-current-buffer buffer
          (when (or revert
                    (eq revert 'ask-revert)
                    (not majutsu-buffer-blob-path))
            (when (or (not (eq revert 'ask-revert))
                      (y-or-n-p (format "%s already exists; revert it? "
                                        (buffer-name))))
              (setq majutsu-buffer-blob-root root)
              (setq majutsu-buffer-blob-revset rev)
              (setq majutsu-buffer-blob-revision resolved)
              (setq majutsu-buffer-blob-change-id resolved)
              (setq majutsu-buffer-blob-commit-id commit-id)
              (setq majutsu-buffer-blob-path file-rel)
              (setq default-directory (if (file-exists-p defdir) defdir root))
              (setq-local revert-buffer-function #'majutsu-file-revert-buffer)
              (majutsu-file-revert-buffer nil t)
              (run-hooks 'majutsu-find-file-hook))))
        buffer))))

(defun majutsu-file--current-content-hash ()
  "Return the hash for current buffer text."
  (secure-hash 'sha1 (current-buffer)))

(defun majutsu-file--refresh-buffer-name ()
  "Refresh blob buffer name using current path and change-id."
  (when (and majutsu-buffer-blob-path majutsu-buffer-blob-revision)
    (let ((expected (majutsu-file--buffer-name
                     (majutsu-file--short-id majutsu-buffer-blob-revision)
                     majutsu-buffer-blob-path)))
      (unless (equal (buffer-name) expected)
        (rename-buffer expected t)))))

(defun majutsu-file-revert-buffer (_ignore-auto noconfirm)
  "Revert the current blob buffer content.
If revision metadata moved, preserve location heuristically:
- same change-id + same commit-id: keep current location
- same change-id + different commit-id: offset line across revisions
- different change-id: jump to beginning"
  (let* ((local-drift (and majutsu-buffer-blob-content-hash
                           (not (equal majutsu-buffer-blob-content-hash
                                       (majutsu-file--current-content-hash)))))
         (modified (or (buffer-modified-p) local-drift)))
    (when (or noconfirm
              (not modified)
              (y-or-n-p "Revert blob buffer? "))
      (unless majutsu-buffer-blob-root
        (user-error "Not in a blob buffer"))
      (when majutsu-blob-edit-mode
        (user-error "Finish or abort blob edit mode before refresh"))
      (let* ((inhibit-read-only t)
             (default-directory majutsu-buffer-blob-root)
             (path majutsu-buffer-blob-path)
             (line (line-number-at-pos))
             (col (current-column))
             (old-change (or majutsu-buffer-blob-change-id majutsu-buffer-blob-revision))
             (old-commit majutsu-buffer-blob-commit-id)
             (rev-info (or (majutsu-file--resolve-single-rev-info majutsu-buffer-blob-revset)
                           (user-error "Revset no longer resolves to a single revision")))
              (new-change (plist-get rev-info :change-id))
              (new-commit (plist-get rev-info :commit-id))
              (target-line line)
              (target-col col)
              (reload-needed t))
        (cond
         ((and (equal old-change new-change)
               majutsu-buffer-blob-content-hash
               (equal old-commit new-commit)
               (not modified))
           ;; Nothing changed remotely and nothing changed locally.
           (setq reload-needed nil))
         ((and old-change old-commit
               (equal old-change new-change)
               new-commit)
          (setq target-line
                (or (condition-case nil
                        (majutsu-diff-visit--offset
                         majutsu-buffer-blob-root path old-commit new-commit line)
                      (error line))
                    line)))
         ((not (equal old-change new-change))
          (setq target-line 1
                target-col 0)))
        (if (not reload-needed)
            nil
          (erase-buffer)
          (majutsu-jj-insert "file" "show" "-r"
                             (or new-commit (majutsu-file--normalize-revset majutsu-buffer-blob-revset))
                             (majutsu-jj-fileset-quote path))
          (let ((buffer-file-name (expand-file-name path default-directory))
                (after-change-major-mode-hook
                 (seq-difference after-change-major-mode-hook
                                 '(global-diff-hl-mode-enable-in-buffer
                                   global-diff-hl-mode-enable-in-buffers)
                                 #'eq)))
            (normal-mode (not enable-local-variables)))
          (setq majutsu-buffer-blob-revision new-change)
          (setq majutsu-buffer-blob-change-id new-change)
          (setq majutsu-buffer-blob-commit-id new-commit)
          (majutsu-file--refresh-buffer-name)
          (setq buffer-read-only t)
          (set-buffer-modified-p nil)
          (setq-local majutsu-buffer-blob-content-hash (majutsu-file--current-content-hash))
          (majutsu-blob-mode 1)
          (majutsu-file--goto-line-col (or target-line 1) (or target-col 0)))))))

;;;###autoload
(defun majutsu-find-file (revset path)
  "View PATH from REVSET in a blob buffer."
  (interactive (majutsu-find-file-read-args "Find file"))
  (majutsu-find-file--internal revset path #'pop-to-buffer-same-window))

;;;###autoload
(defun majutsu-find-file-at-point ()
  "View file at point from the relevant revision."
  (interactive)
  (let* ((root (majutsu-file--root))
         (revset (majutsu-file--default-revset))
         (path (or (majutsu-file--path-at-point root)
                   (majutsu-file--read-path revset root))))
    (majutsu-find-file revset path)))

(defun majutsu-bury-or-kill-buffer (&optional bury-buffer)
  "Bury the current buffer if displayed in multiple windows, else kill it.
With a prefix argument BURY-BUFFER only bury the buffer even if it is only
displayed in a single window."
  (interactive "P")
  (if (or bury-buffer (cdr (get-buffer-window-list nil nil t)))
      (bury-buffer)
    (kill-buffer)))

(defun majutsu-find-file--internal (rev file fn)
  "Visit FILE from REV using FN to display the buffer.
REV is a revset, or nil to visit the file directly from the filesystem.
Preserves line/column position when jumping between revisions."
  (let* ((root (majutsu-file--root))
         (rel-file (majutsu-file--relative-path root file))
         (visited-file (when majutsu-buffer-blob-path
                         majutsu-buffer-blob-path))
         (from-rev majutsu-buffer-blob-revision)
         line col buf)
    ;; Capture position if visiting same file
    (when (and visited-file (equal visited-file rel-file))
      (setq line (line-number-at-pos))
      (setq col (current-column))
      ;; Adjust line when jumping to different revision
      (when (and line from-rev)
        (let ((to-rev (or rev "@")))
          (unless (equal from-rev to-rev)
            (setq line (majutsu-diff-visit--offset root rel-file from-rev to-rev line))))))
    ;; Get or create buffer using majutsu-find-file-noselect
    (setq buf (majutsu-find-file-noselect rev rel-file))
    ;; Display and position
    (funcall fn buf)
    (when line
      (with-current-buffer buf
        (majutsu-file--goto-line-col line (or col 0))))
    buf))

(defun majutsu-blob-visit-file ()
  "Visit the workspace version of the current blob's file.
Preserves line/column position from the blob buffer."
  (interactive)
  (unless (and (bound-and-true-p majutsu-blob-mode)
               majutsu-buffer-blob-root
               majutsu-buffer-blob-path)
    (user-error "Not in a blob buffer"))
  ;; Pass nil as rev to visit filesystem file directly
  (majutsu-find-file--internal nil
                               majutsu-buffer-blob-path
                               #'pop-to-buffer-same-window))

(defun majutsu-blob-visit-magit ()
  "Visit the current blob in `magit-blob-mode'."
  (interactive)
  (unless (and (bound-and-true-p majutsu-blob-mode)
               majutsu-buffer-blob-root
               majutsu-buffer-blob-path
               majutsu-buffer-blob-revset)
    (user-error "Not in a blob buffer"))
  (let* ((line (line-number-at-pos))
         (col (current-column))
         (default-directory majutsu-buffer-blob-root))
    (when (and (not majutsu-buffer-blob-commit-id)
               majutsu-buffer-blob-revset)
      (when-let* ((rev-info (majutsu-file--resolve-single-rev-info majutsu-buffer-blob-revset))
                  (new-change (plist-get rev-info :change-id)))
        (setq majutsu-buffer-blob-revision new-change)
        (setq majutsu-buffer-blob-change-id new-change)
        (setq majutsu-buffer-blob-commit-id (plist-get rev-info :commit-id))
        (majutsu-file--refresh-buffer-name)))
    (let ((commit majutsu-buffer-blob-commit-id))
      (unless (and commit (not (string-empty-p commit)))
        (user-error "Revset does not resolve to a single commit"))
      (unless (fboundp 'magit-find-file)
        (require 'magit-files))
      (let ((buf (magit-find-file commit majutsu-buffer-blob-path)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (majutsu-file--goto-line-col line col)))))))

;; TODO: move path condition to majutsu-file-*-change
(defun majutsu-file--revset-for-files (revset path direction)
  "Build a revset for PATH and DIRECTION relative to REVSET.
DIRECTION should be either \='prev or \='next."
  (let* ((file-set (format "files(%s)" (majutsu-jj-fileset-quote path))))
    (pcase direction
      ('prev (format "::%s-&%s" revset file-set))
      ('next (format "roots(%s+::&%s)" revset file-set))
      (_ (user-error "Unknown direction")))))

(defun majutsu-file--commit-info (revset)
  "Return commit info (description and age) for REVSET."
  (let* ((lines (majutsu-jj-lines
                 "log" "-r" revset "--no-graph" "-T"
                 "coalesce(description.first_line(), \"(no desc)\") ++ \"\\n\" ++ committer.timestamp().ago()")))
    (when (>= (length lines) 2)
      (let* ((summary (car lines))
             (timestamp (cadr lines)))
        (cons summary timestamp)))))

(defun majutsu-file--goto-line-col (line col)
  "Move point to LINE and COL in current buffer."
  (widen)
  (goto-char (point-min))
  (forward-line (max 0 (1- line)))
  (move-to-column col))

(defun majutsu-file-prev-change (revset path)
  "Return the previous change-id modifying PATH before REVSET."
  (let* ((query (majutsu-file--revset-for-files revset path 'prev))
         (raw (majutsu-jj-string "log" "-r" query "-G"
                                 "--limit" "1" "-T" "change_id"))
         (result (and raw (string-trim raw))))
    (when (and (stringp result) (not (string-empty-p result)))
      result)))

(defun majutsu-file-next-change (revset path)
  "Return the next change-id modifying PATH after REVSET."
  (let* ((query (majutsu-file--revset-for-files revset path 'next))
         (raw (majutsu-jj-string "log" "-r" query "-G" "--reversed"
                                 "--limit" "1" "-T" "change_id"))
         (result (and raw (string-trim raw))))
    (when (and (stringp result) (not (string-empty-p result)))
      result)))

(defun majutsu-blob-previous ()
  "Visit previous blob that modified current file."
  (interactive)
  (unless (and majutsu-buffer-blob-revision majutsu-buffer-blob-path)
    (user-error "Not in a blob buffer"))
  (let* ((root majutsu-buffer-blob-root)
         (from-rev majutsu-buffer-blob-revision)
         (path majutsu-buffer-blob-path)
         (line (line-number-at-pos))
         (col (current-column)))
    (if-let* ((prev (majutsu-file-prev-change from-rev path)))
        (let ((target-line (majutsu-diff-visit--offset root path from-rev prev line)))
          (majutsu-find-file prev path)
          (majutsu-file--goto-line-col target-line col)
          (majutsu-blob--show-commit-info prev))
      (user-error "You have reached the beginning of time"))))

(defun majutsu-blob-next ()
  "Visit next blob that modified current file."
  (interactive)
  (unless (and majutsu-buffer-blob-revision majutsu-buffer-blob-path)
    (user-error "Not in a blob buffer"))
  (let* ((root majutsu-buffer-blob-root)
         (from-rev majutsu-buffer-blob-revision)
         (path majutsu-buffer-blob-path)
         (line (line-number-at-pos))
         (col (current-column)))
    (if-let* ((next (majutsu-file-next-change from-rev path)))
        (let ((target-line (majutsu-diff-visit--offset root path from-rev next line)))
          (majutsu-find-file next path)
          (majutsu-file--goto-line-col target-line col)
          (majutsu-blob--show-commit-info next))
      (user-error "You have reached the end of time"))))

(defun majutsu-blob--show-commit-info (revset)
  "Show commit info for REVSET in the echo area."
  (when-let* ((info (majutsu-file--commit-info revset)))
    (message "%s (%s)" (car info) (cdr info))))

(defvar-keymap majutsu-blob-mode-map
  :doc "Keymap for `majutsu-blob-mode'."
  "p" #'majutsu-blob-previous
  "n" #'majutsu-blob-next
  "q" #'majutsu-bury-or-kill-buffer
  "V" #'majutsu-blob-visit-file
  "C-c m" #'majutsu-blob-visit-magit
  "b" #'majutsu-annotate-addition
  "C-x C-q" #'majutsu-blob-edit-start
  "e" #'majutsu-blob-edit-start
  "g" #'revert-buffer
  ;; RET visits the revision (edit)
  "<remap> <majutsu-visit-thing>" #'majutsu-edit-changeset)

(define-minor-mode majutsu-blob-mode
  "Enable Majutsu features in blob buffers.

When called directly from a file buffer, open the @ blob for that file."
  :keymap majutsu-blob-mode-map
  (when (and majutsu-blob-mode
             (not (and majutsu-buffer-blob-root
                       majutsu-buffer-blob-path)))
    (let ((file buffer-file-name))
      (setq majutsu-blob-mode nil)
      (if file
          (let* ((root (majutsu-file--root))
                 (path (majutsu-file--relative-path root file)))
            (majutsu-find-file "@" path))
        (user-error "Buffer is not visiting a file")))))

;;; _
(provide 'majutsu-file)
;;; majutsu-file.el ends here
