;;; majutsu-file.el --- Finding files  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;;; Commentary:

;; Support jj file commands and blob buffers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'majutsu)

(defvar majutsu-find-file-hook nil
  "Hook run after creating a blob buffer.")

(defvar majutsu-file--list-cache nil
  "Alist cache of file lists keyed by revset string.")

(defvar-local majutsu-buffer-blob-revset nil
  "Input revset string for this blob buffer.")
(defvar-local majutsu-buffer-blob-revision nil
  "Resolved single revision (change-id or commit-id) for this blob buffer.")
(defvar-local majutsu-buffer-blob-path nil
  "Relative path for this blob buffer.")
(defvar-local majutsu-buffer-blob-root nil
  "Repository root for this blob buffer.")

(put 'majutsu-buffer-blob-revset 'permanent-local t)
(put 'majutsu-buffer-blob-revision 'permanent-local t)
(put 'majutsu-buffer-blob-path 'permanent-local t)
(put 'majutsu-buffer-blob-root 'permanent-local t)

(add-hook 'majutsu-find-file-hook #'majutsu-blob-mode)

(defun majutsu-file--normalize-revset (revset)
  "Normalize REVSET to a single revision using jj revset functions."
  (format "exactly(latest(%s), 1)" revset))

(defun majutsu-file--short-id (id)
  "Return a shortened version of change ID or commit ID.
Takes first 8 characters, or full string if shorter."
  (if (> (length id) 8)
      (substring id 0 8)
    id))

(defun majutsu-file--resolve-single-rev (revset)
  "Resolve REVSET to a single revision string.
Uses `latest` and `exactly` to enforce a single target."
  (let* ((normalized (majutsu-file--normalize-revset revset))
         (result (string-trim
                  (or (majutsu-jj-string "log" "-r" normalized
                                         "-T" "change_id" "--no-graph" "--limit" "1")
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

(defun majutsu-file--read-path (revset root)
  "Prompt for a file path from REVSET."
  (let* ((paths (majutsu-file--list revset root))
         (default (majutsu-file--path-at-point root)))
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
         (path (or (majutsu-file--path-at-point root)
                   (majutsu-file--read-path revset root))))
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
      (let* ((resolved (or (majutsu-file--resolve-single-rev rev)
                           (user-error "Revset does not resolve to a single revision")))
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
              (setq majutsu-buffer-blob-path file-rel)
              (setq default-directory (if (file-exists-p defdir) defdir root))
              (setq-local revert-buffer-function #'majutsu-file-revert-buffer)
              (majutsu-file-revert-buffer nil t)
              (run-hooks 'majutsu-find-file-hook))))
        buffer))))

(defun majutsu-file-revert-buffer (_ignore-auto noconfirm)
  "Revert the current blob buffer content."
  (when (or noconfirm
            (not (buffer-modified-p))
            (y-or-n-p "Revert blob buffer? "))
    (let* ((inhibit-read-only t)
           (default-directory majutsu-buffer-blob-root)
           (revset (majutsu-file--normalize-revset majutsu-buffer-blob-revset))
           (path majutsu-buffer-blob-path))
      (erase-buffer)
      (majutsu-jj-insert "file" "show" "-r" revset (majutsu-jj-fileset-quote path))
      (let ((buffer-file-name (expand-file-name path default-directory))
            (after-change-major-mode-hook
             (seq-difference after-change-major-mode-hook
                             '(global-diff-hl-mode-enable-in-buffer
                               global-diff-hl-mode-enable-in-buffers)
                             #'eq)))
        (normal-mode (not enable-local-variables)))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))))

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
  "b" #'majutsu-annotate-addition
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
