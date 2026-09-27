;;; majutsu-resolve.el --- Pinned text conflict workbench -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Drafts are private text files, not working-copy files.  Saving a draft does
;; not run jj.  Only explicit apply rewrites the pinned revision.  Structural
;; choices, path creation/deletion and whole-side selection are not supported.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'majutsu-mode)
(require 'majutsu-file)
(require 'majutsu-file-tree)
(require 'majutsu-conflict)
(require 'majutsu-interactive)

(declare-function majutsu-file-apply-contents "majutsu-file" (commit change contents))

(cl-defstruct (majutsu-resolve--file (:constructor majutsu-resolve--make-file))
  path conflict baseline contents buffer temporary applied)

(defclass majutsu-resolve-conflict-section (magit-section)
  ((conflict :initform nil)
   (source :initform nil))
  "A parsed conflict and the exact text from which its positions came.")

(defclass majutsu-resolve-side-section (magit-section) ()
  "A snapshot of a positive or negative conflict term.")

(defclass majutsu-resolve-change-section (majutsu-resolve-side-section) ()
  "A before/after change whose value identifies its positive term.")

(defvar-local majutsu-resolve--commit nil)
(defvar-local majutsu-resolve--change nil)
(defvar-local majutsu-resolve--records nil)
(defvar-local majutsu-resolve--directory nil)
(defvar-local majutsu-resolve--unverified nil)
(defvar-local majutsu-resolve--closing nil)
(defvar-local majutsu-resolve--workbench nil)
(defvar-local majutsu-resolve--path nil)

(defvar majutsu-resolve-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map majutsu-mode-map)
    (define-key map (kbd "RET") #'majutsu-resolve-visit-file)
    (define-key map (kbd "C-c C-c") #'majutsu-resolve-apply)
    (define-key map (kbd "C-c C-k") #'majutsu-resolve-discard)
    (define-key map (kbd "C-c C-n") #'majutsu-resolve-next-file)
    map))

(define-derived-mode majutsu-resolve-mode majutsu-mode "Majutsu Resolve"
  "Review backend conflicts separately from private pending drafts.
Only UTF-8 text content updates are supported, not structural choices."
  :group 'majutsu
  (add-hook 'kill-buffer-query-functions #'majutsu-resolve--can-close nil t)
  (add-hook 'kill-buffer-hook #'majutsu-resolve--cleanup nil t))

(cl-defmethod majutsu-buffer-value (&context (major-mode majutsu-resolve-mode))
  majutsu-resolve--commit)

(defvar majutsu-resolve-draft-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'majutsu-resolve-apply)
    (define-key map (kbd "C-c C-k") #'majutsu-resolve-discard)
    (define-key map (kbd "C-c C-o") #'majutsu-resolve-show)
    (define-key map (kbd "C-c C-n") #'majutsu-resolve-next-file)
    map))

(define-minor-mode majutsu-resolve-draft-mode
  "Private conflict draft: save locally, apply explicitly with C-c C-c."
  :lighter " Resolve draft"
  :keymap majutsu-resolve-draft-mode-map)

(defun majutsu-resolve--output (&rest args)
  "Return exact stdout bytes for ARGS, signaling on command failure."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion)
          (coding-system-for-write 'utf-8-unix))
      (unless (zerop (apply #'majutsu-jj-insert "--color=never" args))
        (user-error "Cannot read jj resolve state (%s); drafts retained"
                    (car args))))
    (buffer-string)))

(defun majutsu-resolve--validate-text (text)
  "Reject TEXT containing NUL, raw bytes or non-Unicode characters."
  (when (seq-some (lambda (char)
                    (or (= char 0) (> char #x10ffff)
                        (and (not (multibyte-string-p text)) (> char 127))
                        (<= #xd800 char #xdfff)))
                  text)
    (user-error "Resolve supports Unicode text only (no NUL or binary bytes)"))
  text)

(defun majutsu-resolve--read-text (commit path)
  "Read PATH at COMMIT without losing invalid UTF-8 bytes."
  (let* ((bytes (majutsu-resolve--output
                 "file" "show" "-T" "" "-r" commit "--"
                 (majutsu-jj-fileset-quote path)))
         (text (decode-coding-string bytes 'utf-8-unix)))
    (majutsu-resolve--validate-text text)
    (unless (equal bytes (encode-coding-string text 'utf-8-unix))
      (user-error "Cannot edit %s: content is not valid UTF-8" path))
    text))

(defun majutsu-resolve--owner ()
  "Return the live workbench owning the current buffer."
  (cond ((derived-mode-p 'majutsu-resolve-mode) (current-buffer))
        ((buffer-live-p majutsu-resolve--workbench) majutsu-resolve--workbench)
        (t (user-error "This buffer has no live resolve workbench"))))

(defun majutsu-resolve--text (record)
  "Return current draft text in RECORD, including unsaved edits."
  (if (buffer-live-p (majutsu-resolve--file-buffer record))
      (with-current-buffer (majutsu-resolve--file-buffer record)
        (save-restriction
          (widen)
          (buffer-substring-no-properties (point-min) (point-max))))
    (majutsu-resolve--file-contents record)))

(defun majutsu-resolve--pending-p (record &optional text)
  "Whether RECORD has content changes, using TEXT when already available."
  (and (majutsu-resolve--file-baseline record)
       (not (equal (majutsu-resolve--file-baseline record)
                   (or text (majutsu-resolve--text record))))))

(defun majutsu-resolve--update-conflicts (entries)
  "Update backend status from successfully queried ENTRIES, retaining drafts."
  (dolist (cell majutsu-resolve--records)
    (setf (majutsu-resolve--file-conflict (cdr cell)) nil))
  (dolist (entry entries)
    (let* ((path (plist-get entry :path))
           (record (cdr (assoc path majutsu-resolve--records))))
      (unless record
        (setq record (majutsu-resolve--make-file :path path))
        (setq majutsu-resolve--records
              (append majutsu-resolve--records (list (cons path record)))))
      (setf (majutsu-resolve--file-conflict record) entry))))

(defun majutsu-resolve--parse-tree-conflicts ()
  "Parse conflicts together with their materialized snapshot/change groups."
  (mapcar (lambda (conflict)
            (cons conflict (majutsu-conflict-display-parts conflict)))
          (majutsu-conflict-parse-buffer)))

(defun majutsu-resolve--file-data (record)
  "Return (TEXT . ENTRIES) without creating or saving a draft.
Each entry is (CONFLICT . PARTS), preserving jj's display grouping."
  (let* ((text (if (majutsu-resolve--file-baseline record)
                   (majutsu-resolve--text record)
                 (majutsu-resolve--read-text
                  majutsu-resolve--commit (majutsu-resolve--file-path record))))
         (buffer (majutsu-resolve--file-buffer record))
         (conflicts
          (if (buffer-live-p buffer)
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  (majutsu-resolve--parse-tree-conflicts)))
            (with-temp-buffer
              (insert text)
              (majutsu-resolve--parse-tree-conflicts)))))
    (when (and (not (majutsu-resolve--file-baseline record))
               (majutsu-resolve--file-conflict record)
               (null conflicts))
      (user-error "Cannot edit %s: not a supported text marker conflict (structural/binary choices are unsupported)"
                  (majutsu-resolve--file-path record)))
    (cons text conflicts)))

(defun majutsu-resolve--insert-side (kind index cell prefix last role)
  "Insert term CELL as ROLE, with its lazily expanded snapshot."
  (let* ((label (car cell))
         (text (cdr cell))
         (body-prefix (concat prefix (if last "    " "│   ")))
         (face (if (eq kind 'add)
                   (majutsu-conflict--side-face index)
                 'smerge-base)))
    (magit-insert-section
        (majutsu-resolve-side-section (list :kind kind :index index) t)
      (magit-insert-heading
        (format "%s%s%s%s%s\n"
                prefix (if last "└── " "├── ") role
                (if (or (null label) (string-empty-p label)) ""
                  (concat ": " (majutsu-file-tree-escape-label label)))
                (cond ((string-empty-p text) " [empty text]")
                      ((not (string-suffix-p "\n" text)) " [no final newline]")
                      (t ""))))
      (magit-insert-section-body
        (let ((start (point)))
          (insert (if (string-empty-p text) "(empty text)\n" text))
          (unless (bolp) (insert "\n"))
          (add-text-properties
           start (point)
           (list 'line-prefix body-prefix 'wrap-prefix body-prefix
                 'font-lock-face face)))))))

(defun majutsu-resolve--insert-part (conflict part text prefix last)
  "Insert PART of CONFLICT as a snapshot or a paired before/after change."
  (if (eq (plist-get part :kind) 'snapshot)
      (let* ((term (plist-get part :term))
             (kind (plist-get term :kind))
             (index (plist-get term :index)))
        (majutsu-resolve--insert-side
         kind index (majutsu-conflict-term-cell conflict term) prefix last
         (if (eq kind 'add)
             (format "Snapshot (+), Side %d" (1+ index))
           (format "Subtract (-), term %d" (1+ index)))))
    (let* ((before (plist-get part :before))
           (after (plist-get part :after))
           (index (plist-get after :index))
           (label (car (majutsu-conflict-term-cell conflict after)))
           (body-prefix (concat prefix (if last "    " "│   "))))
      (magit-insert-section (majutsu-resolve-change-section after t)
        (magit-insert-heading
          (format "%s%sChanges to Side %d%s\n"
                  prefix (if last "└── " "├── ") (1+ index)
                  (if (or (null label) (string-empty-p label)) ""
                    (concat ": " (majutsu-file-tree-escape-label label)))))
        (magit-insert-section-body
          (when-let* ((begin (plist-get part :diff-begin)))
            (let ((start (point)) end)
              (insert (substring text (1- begin) (1- (plist-get part :diff-end))))
              (setq end (point))
              (add-text-properties start end
                                   (list 'line-prefix body-prefix
                                         'wrap-prefix body-prefix))
              (save-excursion
                (goto-char start)
                (while (< (point) end)
                  (add-text-properties
                   (point) (line-end-position)
                   (list 'font-lock-face
                         (pcase (char-after)
                           (?+ 'diff-added) (?- 'diff-removed) (_ 'diff-context))))
                  (forward-line 1)))))
          (majutsu-resolve--insert-side
           'remove (plist-get before :index)
           (majutsu-conflict-term-cell conflict before) body-prefix nil "Before (-)")
          (majutsu-resolve--insert-side
           'add index (majutsu-conflict-term-cell conflict after) body-prefix t
           (format "After (+), Side %d" (1+ index))))))))

(defun majutsu-resolve--insert-conflict (entry index text prefix last)
  "Insert conflict ENTRY with its snapshots and before/after changes."
  (let ((conflict (car entry))
        (body-prefix (concat prefix (if last "    " "│   "))))
    (magit-insert-section section (majutsu-resolve-conflict-section index)
      (oset section conflict conflict)
      (oset section source text)
      (magit-insert-heading
        (format "%s%sConflict %d (%d sides)\n"
                prefix (if last "└── " "├── ") index
                (length (majutsu-conflict-adds conflict))))
      (cl-loop for tail on (cdr entry)
               do (majutsu-resolve--insert-part
                   conflict (car tail) text body-prefix (null (cdr tail)))))))

(defun majutsu-resolve--insert-file (record prefix body-prefix)
  "Insert RECORD's file heading, parsed conflicts, and side snapshots."
  (let* ((path (majutsu-resolve--file-path record))
         problem
         (data (condition-case err
                   (majutsu-resolve--file-data record)
                 (user-error (setq problem (error-message-string err)) nil)))
         (text (car data))
         (conflicts (cdr data)))
    (magit-insert-section (jj-file path)
      (magit-insert-heading
        (format "%s%s  [backend: %s; draft: %s]\n"
                prefix
                (majutsu-file-tree-escape-label (file-name-nondirectory path))
                (if-let* ((conflict (majutsu-resolve--file-conflict record)))
                    (format "%d-sided conflict" (plist-get conflict :sides))
                  "resolved")
                (cond ((majutsu-resolve--pending-p record text) "pending")
                      ((majutsu-resolve--file-applied record) "applied")
                      ((majutsu-resolve--file-baseline record) "unchanged")
                      (t "not opened"))))
      (cond
       (problem
        (insert body-prefix "└── "
                (propertize (majutsu-file-tree-escape-label problem) 'face 'warning)
                "\n"))
       ((null conflicts)
        (insert body-prefix "└── No parsed conflict blocks in current draft\n"))
       (t
        (cl-loop for tail on conflicts for index from 1
                 do (majutsu-resolve--insert-conflict
                     (car tail) index text body-prefix (null (cdr tail)))))))))

(defun majutsu-resolve--ancestor (type)
  "Return the nearest section at point matching TYPE, including ancestors."
  (let ((section (magit-current-section)))
    (while (and section (not (magit-section-match type section)))
      (setq section (oref section parent)))
    section))

(defun majutsu-resolve-refresh-buffer ()
  "Refresh the exact pin and show current draft conflicts without replacing text."
  (majutsu-resolve--update-conflicts
   (majutsu-jj-conflicted-files majutsu-resolve--commit))
  (magit-insert-section (resolve)
    (magit-insert-heading "Text conflict workbench\n")
    (insert (format "Commit: %s\nChange: %s\n"
                    majutsu-resolve--commit majutsu-resolve--change))
    (insert "RET edit/jump  TAB fold/unfold  C-c C-c apply file/directory\n"
            "C-u C-c C-c apply all  C-c C-k discard drafts  C-c C-n next file\n"
            "Tree: snapshots + changes; "
            (substitute-command-keys "\\[majutsu-refresh]")
            " / C-c C-o refresh after editing.\n"
            "Save writes private drafts only. An empty draft means an empty file.\n"
            "Text only: no deletion, rename, new paths, or structural choices.\n")
    (when majutsu-resolve--unverified
      (insert (propertize
               "UNVERIFIED: drafts retained; discard/close and reopen before applying.\n"
               'face 'error)))
    (insert "\n")
    (majutsu-file-tree-insert
     (majutsu-file-tree-build (mapcar #'cdr majutsu-resolve--records)
                              #'majutsu-resolve--file-path)
     #'majutsu-resolve--insert-file)))

;;;###autoload
(defun majutsu-resolve-setup-buffer (revision)
  "Open a workbench pinned to the single REVISION and return its buffer."
  (let* ((default-directory (file-name-as-directory (majutsu-file--root)))
         (ids (split-string
               (majutsu-resolve--output
                "log" "--no-graph" "-r" (format "exactly(%s, 1)" revision)
                "-T" "commit_id ++ \"\\n\" ++ change_id ++ \"\\n\"")
               "\n" t))
         (commit (car ids))
         (change (cadr ids)))
    (unless (and (= (length ids) 2)
                 (string-match-p "\\`[0-9a-f]+\\'" commit)
                 (string-match-p "\\`[k-z]+\\'" change))
      (user-error "Revision must identify exactly one commit"))
    (if-let* ((existing (majutsu-get-mode-buffer 'majutsu-resolve-mode commit)))
        (progn (majutsu-display-buffer existing) existing)
      (majutsu-setup-buffer 'majutsu-resolve-mode t
        (majutsu-resolve--commit commit)
        (majutsu-resolve--change change)))))

;;;###autoload
(defun majutsu-resolve (&optional revision)
  "Open the text conflict workbench for REVISION or the revision at point."
  (interactive
   (let ((revision
          (or majutsu-resolve--commit
              (and (buffer-live-p majutsu-resolve--workbench)
                   (buffer-local-value 'majutsu-resolve--commit
                                       majutsu-resolve--workbench))
              (bound-and-true-p majutsu-buffer-blob-commit-id)
              (majutsu-revision-at-point)
              "@")))
     (list (if current-prefix-arg
               (majutsu-read-revset "Resolve revision" :default revision)
             revision))))
  (majutsu-resolve-setup-buffer (or revision "@")))

(defun majutsu-resolve--save ()
  "Save only this draft's private file, never its repository path."
  (let* ((owner (majutsu-resolve--owner))
         (path majutsu-resolve--path)
         (record (with-current-buffer owner
                   (cdr (assoc path majutsu-resolve--records))))
         (text (majutsu-resolve--validate-text (majutsu-resolve--text record)))
         (coding-system-for-write 'utf-8-unix))
    (unless (equal buffer-file-name (majutsu-resolve--file-temporary record))
      (user-error "Drafts can only be saved to their private file"))
    (write-region text nil (majutsu-resolve--file-temporary record) nil 'silent)
    (setf (majutsu-resolve--file-contents record) text)
    (set-visited-file-modtime)
    (set-buffer-modified-p nil)
    t))

(defun majutsu-resolve--remember-draft ()
  "Keep killed draft text in its workbench, including saved pending changes."
  (when (buffer-live-p majutsu-resolve--workbench)
    (let* ((path majutsu-resolve--path)
           (record (with-current-buffer majutsu-resolve--workbench
                     (cdr (assoc path majutsu-resolve--records)))))
      (when record
        (setf (majutsu-resolve--file-contents record) (majutsu-resolve--text record)
              (majutsu-resolve--file-buffer record) nil)))))

(defun majutsu-resolve--no-revert (&rest _args)
  "Require explicit discard instead of losing unsaved draft changes."
  (user-error "Use C-c C-k to discard this private draft explicitly"))

(defun majutsu-resolve-visit-file (&optional path)
  "Open PATH's private draft, or visit the file, conflict, or side at point.
PATH is a raw repository-relative path.  A side entry navigates to its
materialized marker region; it does not select that side as a resolution."
  (interactive)
  (let* ((owner (majutsu-resolve--owner))
         (conflict-section
          (and (not path)
               (majutsu-resolve--ancestor 'majutsu-resolve-conflict-section)))
         (term (and conflict-section
                    (magit-section-value-if 'majutsu-resolve-side-section)))
         (path (or path majutsu-resolve--path
                   (when-let* ((section (majutsu-resolve--ancestor 'jj-file)))
                     (oref section value))
                   (with-current-buffer owner
                     (completing-read "Conflict file: " majutsu-resolve--records nil t))))
         buffer)
    (with-current-buffer owner
      (let ((record (cdr (assoc path majutsu-resolve--records))))
        (unless record (user-error "Path is not in this workbench: %s" path))
        (unless (majutsu-resolve--file-baseline record)
          (let ((text (car (majutsu-resolve--file-data record))))
            (setf (majutsu-resolve--file-baseline record) text
                  (majutsu-resolve--file-contents record) text)))
        (when (and conflict-section
                   (not (equal (oref conflict-section source)
                               (majutsu-resolve--text record))))
          (majutsu-refresh-buffer)
          (user-error "Draft changed; tree refreshed, select the conflict or side again"))
        (setq buffer (majutsu-resolve--file-buffer record))
        (unless (buffer-live-p buffer)
          (unless majutsu-resolve--directory
            (setq majutsu-resolve--directory (make-temp-file "majutsu-resolve-" t)))
          (let* ((temporary
                  (or (majutsu-resolve--file-temporary record)
                      (make-temp-file
                       (expand-file-name "draft-" majutsu-resolve--directory)
                       nil (concat "-" (file-name-nondirectory path)))))
                 (root default-directory)
                 (coding-system-for-write 'utf-8-unix))
            (write-region (majutsu-resolve--file-contents record) nil temporary nil 'silent)
            (setq buffer (generate-new-buffer (format "*resolve: %s*" path)))
            (setf (majutsu-resolve--file-temporary record) temporary
                  (majutsu-resolve--file-buffer record) buffer)
            (with-current-buffer buffer
              (insert (majutsu-resolve--file-contents record))
              (setq buffer-file-name temporary)
              (let ((enable-local-variables nil)) (set-auto-mode))
              (setq-local default-directory root
                          majutsu-resolve--workbench owner
                          majutsu-resolve--path path
                          buffer-file-coding-system 'utf-8-unix
                          make-backup-files nil
                          auto-save-default nil
                          revert-buffer-function #'majutsu-resolve--no-revert
                          header-line-format "Private text draft: C-x C-s saves locally; C-c C-c applies; C-c C-k discards; C-c C-o returns")
              (auto-save-mode -1)
              (add-hook 'write-contents-functions #'majutsu-resolve--save nil t)
              (add-hook 'kill-buffer-hook #'majutsu-resolve--remember-draft nil t)
              (majutsu-conflict-mode 1)
              (majutsu-resolve-draft-mode 1)
              (set-visited-file-modtime)
              (set-buffer-modified-p nil)
              (goto-char (point-min)))))))
    (pop-to-buffer buffer)
    (when conflict-section
      (with-current-buffer buffer
        (widen)
        (majutsu-conflict--goto-term (oref conflict-section conflict) term)))
    buffer))

(defun majutsu-resolve-show ()
  "Return to this draft's workbench without changing its pin."
  (interactive)
  (let ((owner (majutsu-resolve--owner)))
    (majutsu-display-buffer owner)
    (with-current-buffer owner (majutsu-refresh-buffer))
    owner))

(defun majutsu-resolve--scope (all)
  "Return records in the current scope, or every record when ALL."
  (let* ((owner (majutsu-resolve--owner))
         (paths (unless all
                  (if majutsu-resolve--path (list majutsu-resolve--path)
                    (let ((section (majutsu-resolve--ancestor '(jj-file jj-directory))))
                      (cond ((magit-section-match 'jj-file section)
                             (list (oref section value)))
                            ((magit-section-match 'jj-directory section)
                             (majutsu-file-tree-files section))
                            (t (user-error "Select a file/directory, or use a prefix for all drafts"))))))))
    (with-current-buffer owner
      (if all (mapcar #'cdr majutsu-resolve--records)
        (mapcar (lambda (path) (cdr (assoc path majutsu-resolve--records))) paths)))))

(defun majutsu-resolve--assert-visible ()
  "Refuse applying a pin no longer in the visible change-id set."
  (unless (member majutsu-resolve--commit
                  (split-string
                   (majutsu-resolve--output
                    "log" "--no-graph" "-r"
                    (format "change_id(%s)" majutsu-resolve--change)
                    "-T" "commit_id ++ \"\\n\"") "\n" t))
    (user-error "Pinned commit is no longer visible; drafts retained, close and reopen")))

(defun majutsu-resolve--assert-workspace-clean (records)
  "Reject unsaved real workspace buffers for the paths in RECORDS."
  (let ((files (mapcar (lambda (record)
                         (expand-file-name (majutsu-resolve--file-path record)
                                           default-directory)) records)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and buffer-file-name (buffer-modified-p)
                   (seq-some (lambda (file)
                               (or (equal file buffer-file-name)
                                   (file-equal-p file buffer-file-name))) files))
          (user-error "Save or revert workspace buffer %s before applying" (buffer-name)))))))

(defun majutsu-resolve--write-private (record text)
  "Write TEXT to RECORD's existing private file."
  (when (majutsu-resolve--file-temporary record)
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region text nil (majutsu-resolve--file-temporary record) nil 'silent))))

(defun majutsu-resolve--install-text (record text &optional written)
  "Install verified TEXT as RECORD's baseline; WRITTEN skips disk writing."
  (unless written (majutsu-resolve--write-private record text))
  (setf (majutsu-resolve--file-baseline record) text
        (majutsu-resolve--file-contents record) text)
  (when (buffer-live-p (majutsu-resolve--file-buffer record))
    (with-current-buffer (majutsu-resolve--file-buffer record)
      (save-restriction
        (widen)
        (unless (equal text (buffer-substring-no-properties (point-min) (point-max)))
          (let ((position (point))
                (inhibit-read-only t)
                (inhibit-modification-hooks t))
            (erase-buffer)
            (insert text)
            (goto-char (min position (point-max))))))
      (majutsu-conflict-mode 1)
      (set-visited-file-modtime)
      (set-buffer-modified-p nil))))

(defun majutsu-resolve-apply (&optional all)
  "Apply pending drafts in scope in one operation; ALL applies every draft.
Return the verified resulting full commit id.  Failures retain drafts and
block blind retries; closing and reopening is required after uncertainty."
  (interactive "P")
  (let ((owner (majutsu-resolve--owner))
        (records (seq-filter #'majutsu-resolve--pending-p (majutsu-resolve--scope all))))
    (with-current-buffer owner
      (when majutsu-resolve--unverified
        (user-error "Previous apply is unverified; discard/close and reopen before retrying"))
      (when records
        (let ((contents (mapcar (lambda (record)
                                  (cons (majutsu-resolve--file-path record)
                                        (majutsu-resolve--validate-text
                                         (majutsu-resolve--text record)))) records))
              (root default-directory)
              (coding-system-for-write 'utf-8-unix)
              before attempted verified)
          (majutsu-process-register-interactive-session root owner)
          (unwind-protect
              (progn
                (majutsu-resolve--assert-workspace-clean records)
                (setq before (majutsu-jj-operation-id root t))
                (unless before (user-error "Cannot read repository operation; drafts retained"))
                (majutsu-resolve--assert-visible)
                (setq attempted t
                      majutsu-resolve--unverified t)
                (let* ((old majutsu-resolve--commit)
                       (result (majutsu-file-apply-contents old majutsu-resolve--change contents))
                       (conflicts (majutsu-jj-conflicted-files result))
                       readback)
                  (dolist (cell contents)
                    (let* ((path (car cell))
                           (text (majutsu-resolve--read-text result path)))
                      (when (string-empty-p
                             (majutsu-resolve--output
                              "diff" "--from" old "--to" result "--summary" "--"
                              (majutsu-jj-fileset-quote path)))
                        (user-error "jj did not change %s (structural or marker-only no-op); drafts retained, result unverified" path))
                      (unless (or (seq-find (lambda (entry) (equal path (plist-get entry :path))) conflicts)
                                  (equal text (cdr cell)))
                        (user-error "Readback differs for %s; drafts retained, result unverified" path))
                      (push (cons path text) readback)))
                  ;; All backend checks succeed before any draft is replaced.
                  ;; Disk errors must not discard any in-memory draft.
                  (dolist (record records)
                    (majutsu-resolve--write-private
                     record (cdr (assoc (majutsu-resolve--file-path record) readback))))
                  (dolist (record records)
                    (majutsu-resolve--install-text
                     record (cdr (assoc (majutsu-resolve--file-path record) readback)) t)
                    (setf (majutsu-resolve--file-applied record) t))
                  (setq majutsu-resolve--commit result)
                  (majutsu-resolve--update-conflicts conflicts)
                  (setq majutsu-resolve--unverified nil verified t)))
            (majutsu-process-unregister-interactive-session root owner)
            (when attempted
              (majutsu-interactive-complete-repository-operation root owner before))
            (when verified (majutsu-refresh-buffer)))))
      majutsu-resolve--commit)))

(defun majutsu-resolve-discard (&optional all)
  "Discard un-applied draft changes in scope, or all with ALL.
This never undoes a jj operation, including an unverified apply."
  (interactive "P")
  (let ((owner (majutsu-resolve--owner))
        (records (seq-filter #'majutsu-resolve--pending-p (majutsu-resolve--scope all))))
    (when (and records
               (or (not (called-interactively-p 'interactive))
                   (yes-or-no-p "Discard these private drafts? This does not undo jj operations. ")))
      (dolist (record records)
        (majutsu-resolve--install-text record (majutsu-resolve--file-baseline record))))
    (with-current-buffer owner (majutsu-refresh-buffer))))

(defun majutsu-resolve-next-file ()
  "Visit the next backend-conflicted file, wrapping around the workbench."
  (interactive)
  (let* ((owner (majutsu-resolve--owner))
         (path (or majutsu-resolve--path
                   (when-let* ((section (majutsu-resolve--ancestor 'jj-file)))
                     (oref section value))))
         (paths (with-current-buffer owner
                  (mapcar #'car
                          (seq-filter (lambda (cell) (majutsu-resolve--file-conflict (cdr cell)))
                                      majutsu-resolve--records))))
         (next (or (cadr (member path paths)) (car paths))))
    (unless next (user-error "No remaining backend conflicts"))
    (with-current-buffer owner (majutsu-resolve-visit-file next))))

(defun majutsu-resolve--can-close ()
  "Ask before closing a workbench with pending or unverified drafts."
  (or majutsu-resolve--closing
      (and (not majutsu-resolve--unverified)
           (not (seq-some (lambda (cell) (majutsu-resolve--pending-p (cdr cell)))
                          majutsu-resolve--records)))
      (yes-or-no-p "Discard private resolve drafts and close? This cannot undo jj operations. ")))

(defun majutsu-resolve--cleanup ()
  "Clean private files only after the workbench close query has succeeded."
  (setq majutsu-resolve--closing t)
  (dolist (cell majutsu-resolve--records)
    (when-let* ((buffer (majutsu-resolve--file-buffer (cdr cell)))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))
  (when majutsu-resolve--directory
    (delete-directory majutsu-resolve--directory t)))

(provide 'majutsu-resolve)
;;; majutsu-resolve.el ends here
