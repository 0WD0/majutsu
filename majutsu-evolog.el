;;; majutsu-evolog.el --- JJ evolution log views for Majutsu  -*- lexical-binding: t; -*-

;;; Commentary:

;; This library renders `jj evolog' output as Majutsu sections and provides a
;; detail buffer for a single evolution entry's `inter_diff()' patch.

;;; Code:

(require 'majutsu)
(require 'majutsu-diff)
(require 'majutsu-graph-record)
(require 'seq)
(require 'subr-x)

(declare-function majutsu-jj-buffer-string "majutsu-jj" (&rest args))
(declare-function majutsu-jj-wash "majutsu-jj" (washer keep-error &rest args))

(defconst majutsu-evolog--field-separator majutsu-graph-record-field-separator
  "Field separator used by Majutsu evolog templates.")

(defconst majutsu-evolog--record-name "evolog"
  "Graph record protocol name for evolog entries.")

(defconst majutsu-evolog--entry-template
  (majutsu-graph-record-template
   majutsu-evolog--record-name
   '([:commit :change_id]
     [:label "change_id" [:commit :change_id :short]]
     [:commit :commit_id]
     [:label "commit_id" [:commit :commit_id :short]]
     [:label "author email" [:commit :author :email]]
     [:label "time ago" [:commit :committer :timestamp :ago]]
     [:if [:commit :hidden] "hidden" ""]
     [:if [:commit :conflict] "conflict" ""]
     [:if [:commit :empty] "empty" ""]
     [:if [:commit :root] "root" ""]
     [:coalesce
      [:if [:commit :description]
          [:commit :description :first_line]]
      [:if [:commit :empty] "(empty)"]
      "(no description set)"]
     [:if [:operation] [:operation :id] ""]
     [:if [:operation] [:label "operation_id" [:operation :id :short]] ""]
     [:if [:operation]
         [:operation :description :first_line]
       ""])
   'CommitEvolutionEntry)
  "Template used by `majutsu-evolog'.")

(defconst majutsu-evolog--inter-diff-template
  "self.inter_diff().git()"
  "Template used for a single evolog entry patch.")

(defconst majutsu-evolog--read-only-global-args
  '("--at-op=@" "--ignore-working-copy")
  "Top-level jj arguments for read-only evolog queries.")

(defvar-local majutsu-evolog--revset nil
  "Revset used for the current evolog buffer.")

(defvar-local majutsu-evolog--args nil
  "Arguments used for the current evolog buffer.")

(defvar-local majutsu-evolog--cached-entries nil
  "Parsed entries for the current evolog buffer.")

(defvar-local majutsu-evolog-show--commit-id nil
  "Full commit id used for the current evolog entry buffer.")

(defun majutsu-evolog--machine-field (field)
  "Return FIELD without text properties or surrounding whitespace."
  (string-trim (substring-no-properties (or field ""))))

(defun majutsu-evolog--display-field (field)
  "Return FIELD as a display string, preserving text properties."
  (or field ""))

(defun majutsu-evolog--nonempty-field-p (field)
  "Return non-nil when FIELD is present and non-empty."
  (and field
       (not (string-empty-p
             (string-trim (substring-no-properties field))))))

(defun majutsu-evolog--split-record (payload expected)
  "Split PAYLOAD into EXPECTED fields, or return nil when malformed."
  (majutsu-graph-record-split-fields payload expected))

(defun majutsu-evolog--parse-entry-record (record)
  "Parse one evolog graph RECORD into a plist."
  (when-let* ((fields (majutsu-evolog--split-record (plist-get record :payload) 14)))
    (pcase-let ((`(,change-id ,change-id-short ,commit-id ,commit-id-short
                   ,author-email ,time-ago ,hidden ,conflict ,empty ,root
                   ,description ,operation-id ,operation-id-short
                   ,operation-description)
                 fields))
      (list :graph-prefix (majutsu-evolog--display-field
                           (plist-get record :graph-prefix))
            :suffix-lines (plist-get record :suffix-lines)
            :change-id (majutsu-evolog--machine-field change-id)
            :change-id-short (majutsu-evolog--machine-field change-id-short)
            :change-id-short-display (majutsu-evolog--display-field change-id-short)
            :commit-id (majutsu-evolog--machine-field commit-id)
            :commit-id-short (majutsu-evolog--machine-field commit-id-short)
            :commit-id-short-display (majutsu-evolog--display-field commit-id-short)
            :author-email (majutsu-evolog--display-field author-email)
            :time-ago (majutsu-evolog--display-field time-ago)
            :hidden (majutsu-evolog--nonempty-field-p hidden)
            :conflict (majutsu-evolog--nonempty-field-p conflict)
            :empty (majutsu-evolog--nonempty-field-p empty)
            :root (majutsu-evolog--nonempty-field-p root)
            :description (majutsu-evolog--display-field description)
            :operation-id (majutsu-evolog--machine-field operation-id)
            :operation-id-short (majutsu-evolog--machine-field operation-id-short)
            :operation-id-short-display (majutsu-evolog--display-field operation-id-short)
            :operation-description (majutsu-evolog--display-field operation-description)))))

(defun majutsu-evolog--parse-entry-line (line)
  "Parse one evolog LINE into a plist."
  (when-let* ((record (majutsu-graph-record-parse-line
                       majutsu-evolog--record-name line)))
    (majutsu-evolog--parse-entry-record record)))

(defun majutsu-evolog--parse-output (output)
  "Parse evolog OUTPUT into entry plists."
  (delq nil
        (mapcar #'majutsu-evolog--parse-entry-line
                (split-string (or output "") "\n" t))))

(defun majutsu-evolog--command-args (revset &optional args)
  "Return jj arguments for evolog REVSET with ARGS."
  (append majutsu-evolog--read-only-global-args
          '("evolog")
          (or args majutsu-evolog--args)
          (list "-r" revset "-T" majutsu-evolog--entry-template)))

(defun majutsu-evolog--show-command-args (commit-id)
  "Return jj arguments for the inter-diff of COMMIT-ID."
  (append majutsu-evolog--read-only-global-args
          (list "evolog" "--no-graph" "--limit=1"
                "-r" (format "commit_id(%s)" commit-id)
                "-T" majutsu-evolog--inter-diff-template)))

(defun majutsu-parse-evolog-entries (&optional buf output)
  "Parse evolog entries in BUF or OUTPUT.
When OUTPUT is nil, run jj in BUF or the current buffer and cache the result."
  (with-current-buffer (or buf (current-buffer))
    (if (and majutsu-evolog--cached-entries (not output))
        majutsu-evolog--cached-entries
      (let ((entries (majutsu-evolog--parse-output
                      (or output
                          (apply #'majutsu-jj-buffer-string
                                 (majutsu-evolog--command-args
                                  (or majutsu-evolog--revset "@")))))))
        (unless output
          (setq majutsu-evolog--cached-entries entries))
        entries))))

(defun majutsu-evolog--line-string ()
  "Return the current line, preserving text properties and omitting newline."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun majutsu-evolog--delete-line ()
  "Delete the current line, including its trailing newline when present."
  (delete-region (line-beginning-position)
                 (min (point-max) (1+ (line-end-position)))))

(defun majutsu-evolog--insert-field (label value)
  "Insert LABEL and VALUE when VALUE is non-empty."
  (when (majutsu-evolog--nonempty-field-p value)
    (insert (propertize (concat label ": ") 'face 'magit-section-heading)
            value
            "\n")))

(defun majutsu-evolog--entry-flags (entry)
  "Return a display string for ENTRY flags."
  (string-join
   (delq nil
         (list (and (plist-get entry :hidden) "hidden")
               (and (plist-get entry :conflict) "conflict")
               (and (plist-get entry :empty) "empty")
               (and (plist-get entry :root) "root")))
   " "))

(defun majutsu-evolog--format-entry-heading (entry)
  "Return the heading line for one evolog ENTRY."
  (concat
   (plist-get entry :graph-prefix)
   (string-join
    (delq nil
          (list (plist-get entry :change-id-short-display)
                (plist-get entry :commit-id-short-display)
                (let ((flags (majutsu-evolog--entry-flags entry)))
                  (unless (string-empty-p flags)
                    (propertize flags 'face 'font-lock-warning-face)))
                (plist-get entry :description)))
    " ")))

(defun majutsu-evolog--insert-entry-body (entry)
  "Insert multiline evolog body for ENTRY."
  (majutsu-evolog--insert-field "Change" (plist-get entry :change-id))
  (majutsu-evolog--insert-field "Commit" (plist-get entry :commit-id))
  (majutsu-evolog--insert-field "Author" (plist-get entry :author-email))
  (majutsu-evolog--insert-field "Committed" (plist-get entry :time-ago))
  (when (majutsu-evolog--nonempty-field-p (plist-get entry :operation-id))
    (majutsu-evolog--insert-field "Operation" (plist-get entry :operation-id-short-display))
    (majutsu-evolog--insert-field "Operation id" (plist-get entry :operation-id))
    (majutsu-evolog--insert-field "Operation description"
                                  (plist-get entry :operation-description))))

(defun majutsu-evolog--wash-entry ()
  "Wash the current raw evolog record into one `jj-evolog-entry' section."
  (when-let* ((record (majutsu-graph-record-parse-at-point
                       majutsu-evolog--record-name))
              (entry (majutsu-evolog--parse-entry-record record)))
    (let ((beg (plist-get record :beg))
          (end (plist-get record :end)))
      (delete-region beg end)
      (goto-char beg)
      (magit-insert-section (jj-evolog-entry entry)
        (magit-insert-heading (majutsu-evolog--format-entry-heading entry))
        (dolist (line (plist-get entry :suffix-lines))
          (insert line "\n"))
        (majutsu-evolog--insert-entry-body entry))
      entry)))

(defun majutsu-evolog--wash-output (_args)
  "Wash raw `jj evolog` output in the current narrowed region."
  (let (entries)
    (goto-char (point-min))
    (while (not (eobp))
      (if-let* ((entry (majutsu-evolog--wash-entry)))
          (push entry entries)
        (magit-delete-line)))
    (setq majutsu-evolog--cached-entries (nreverse entries))))

(defun majutsu-evolog--insert-entries ()
  "Insert evolog entries for the current buffer."
  (magit-insert-section (jj-evolog majutsu-evolog--revset)
    (magit-insert-heading (format "Evolution Log %s" majutsu-evolog--revset))
    (setq majutsu-evolog--cached-entries nil)
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

(defun majutsu-evolog--entry-at-point ()
  "Return the evolog entry at point, or nil."
  (magit-section-value-if 'jj-evolog-entry))

(defun majutsu-evolog-show-at-point ()
  "Show the inter-diff patch for the evolog entry at point."
  (interactive)
  (if-let* ((entry (majutsu-evolog--entry-at-point))
            (commit-id (plist-get entry :commit-id)))
      (majutsu-evolog-show commit-id)
    (user-error "No evolog entry at point")))

(defvar-keymap majutsu-evolog-mode-map
  :doc "Keymap for `majutsu-evolog-mode'."
  :parent majutsu-mode-map
  "RET" 'majutsu-evolog-show-at-point
  "d" 'majutsu-evolog-show-at-point)

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

(defun majutsu-evolog-show-refresh-buffer ()
  "Refresh the current evolog entry patch buffer."
  (interactive)
  (majutsu--assert-mode 'majutsu-evolog-show-mode)
  (let ((commit-id (or majutsu-evolog-show--commit-id
                       (user-error "No evolog entry commit id"))))
    (magit-insert-section (jj-evolog-show commit-id)
      (magit-insert-heading (format "Evolution Patch %s" commit-id))
      (apply #'majutsu-jj-wash
             (lambda (_args)
               (goto-char (point-min))
               (if (= (point-min) (point-max))
                   (insert (propertize "No patch\n" 'face 'shadow))
                 (majutsu-diff-wash-diffs '("--git"))))
             'wash-anyway
             (majutsu-evolog--show-command-args commit-id)))))

(defvar-keymap majutsu-evolog-show-mode-map
  :doc "Keymap for `majutsu-evolog-show-mode'."
  :parent majutsu-diff-mode-map)

(define-derived-mode majutsu-evolog-show-mode majutsu-diff-mode "Majutsu Evolog Show"
  "Major mode for viewing one jj evolution patch."
  :group 'majutsu
  (setq-local line-number-mode nil)
  (setq-local revert-buffer-function #'majutsu-refresh-buffer))

(defun majutsu-evolog-show--buffer-name (commit-id)
  "Return buffer name for evolog entry COMMIT-ID."
  (let* ((root (majutsu--toplevel-safe))
         (repo (file-name-nondirectory (directory-file-name root))))
    (format "*majutsu-evolog-show: %s:%s*" repo
            (if (> (length commit-id) 12)
                (substring commit-id 0 12)
              commit-id))))

;;;###autoload
(defun majutsu-evolog-show (commit-id)
  "Show the evolution patch for entry COMMIT-ID."
  (interactive
   (list (or (when-let* ((entry (majutsu-evolog--entry-at-point)))
               (plist-get entry :commit-id))
             (read-string "Full commit id: "))))
  (let ((root (majutsu--toplevel-safe)))
    (majutsu-setup-buffer #'majutsu-evolog-show-mode nil
      :buffer (majutsu-evolog-show--buffer-name commit-id)
      :directory root
      (majutsu-evolog-show--commit-id commit-id))))

(provide 'majutsu-evolog)
;;; majutsu-evolog.el ends here
