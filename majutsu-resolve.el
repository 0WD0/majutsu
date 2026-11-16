;;; majutsu-resolve.el --- Conflicts of a revision, as a section -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; A conflicted revision gets a Conflicts section wherever it is shown: its
;; files and each file's conflicts.  A conflict is shown as jj records it, a
;; snapshot and changes from one text to another, each term under the label
;; jj gave it.  Resolving there is one `jj resolve'; `jj undo' takes it back.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'magit-section)
(require 'majutsu-mode)
(require 'majutsu-file)
(require 'majutsu-file-tree)
(require 'majutsu-conflict)

(declare-function majutsu-diff--metadata-revision "majutsu-diff" ())
(declare-function majutsu-diff-revset "majutsu-diff"
                  (revset &optional args range filesets))
(declare-function majutsu-blob-edit-start "majutsu-file" ())

;;; Sections

(defvar-keymap majutsu-resolve-section-map
  :doc "Keymap for the sections of a revision's conflicts."
  "<remap> <majutsu-visit-thing>" #'majutsu-resolve-visit)

(defclass majutsu-resolve-conflicts-section (magit-section)
  ((keymap :initform 'majutsu-resolve-section-map))
  "The conflicts of one commit; the value is its commit id.")

(defclass majutsu-resolve-conflict-section (magit-section)
  ((keymap :initform 'majutsu-resolve-section-map))
  "One conflict of a file; the value is its number, from one.")

(defclass majutsu-resolve-term-section (magit-section)
  ((keymap :initform 'majutsu-resolve-section-map))
  "A term of a conflict; the value is a term plist accepted by
`majutsu-conflict-term-cell'.")

(defun majutsu-resolve--highlight-heading (section)
  "Highlight SECTION's heading only, then its children.
The default highlight paints the whole subtree and so replaces the faces
that tell the sides apart."
  (magit-section-highlight-range
   (oref section start)
   (or (oref section content) (oref section end)))
  (mapc #'magit-section-highlight (oref section children)))

(cl-defmethod magit-section-highlight ((section majutsu-resolve-conflict-section))
  (majutsu-resolve--highlight-heading section))

(cl-defmethod magit-section-highlight ((section majutsu-resolve-term-section))
  (majutsu-resolve--highlight-heading section))

;;; Reading

(defun majutsu-resolve--lines (&rest args)
  "Return the lines jj prints for ARGS, or nil when it fails."
  (majutsu--with-no-color
    (with-temp-buffer
      (when (eq 0 (apply #'majutsu--jj-insert nil args))
        (split-string (buffer-string) "\n" t)))))

(defun majutsu-resolve--commit (revset)
  "Return the commit id of REVSET if it names one conflicted commit."
  (pcase (majutsu-resolve--lines
          "log" "--no-graph" "-r" revset
          "-T" "commit_id ++ if(conflict, \" conflict\") ++ \"\\n\"")
    (`(,line) (and (string-suffix-p " conflict" line)
                   (car (split-string line " "))))))

(defun majutsu-resolve--origins (commit)
  "Return a line for each earlier change COMMIT's conflicts come from.
These are the first conflicted commits of the conflicted history COMMIT
belongs to; a commit that introduces its conflicts has none."
  (majutsu-resolve--lines
   "log" "--no-graph"
   "-r" (format "roots(reachable(%s, conflicts())) ~ %s" commit commit)
   "-T" "change_id.shortest(8) ++ \" \" ++ description.first_line() ++ \"\\n\""))

(defconst majutsu-resolve--files-template
  (let ((types "%s.map(|value| if(value, value.file_type(), \"absent\")).join(\",\")"))
    (concat "if(self.conflict(), concat("
            (format types "self.added_values()") ", \"\\x1F\", "
            (format types "self.removed_values()") ", \"\\x1F\", "
            "self.path(), \"\\0\"))"))
  "Template listing the conflicted files of a commit with their terms.")

(defun majutsu-resolve--files (commit &optional path)
  "Return the conflicted files of COMMIT, or only PATH, as plists.
`:adds' and `:removes' hold for each term what it is, in the order jj
records them: \"file\", \"symlink\", ..., or \"absent\"."
  (majutsu--with-no-color
    (with-temp-buffer
      (unless (eq 0 (apply #'majutsu--jj-insert nil
                           "file" "list" "-r" commit
                           "-T" majutsu-resolve--files-template
                           (and path (list "--" (majutsu-jj-fileset-quote path)))))
        (user-error "Cannot list the conflicts of %s" commit))
      (mapcar (lambda (record)
                (pcase-let ((`(,adds ,removes ,path)
                             (majutsu--split-fields record "\x1f" 3)))
                  (list :path path
                        :adds (split-string adds ",")
                        :removes (split-string removes ","))))
              (split-string (buffer-string) "\0" t)))))

(defun majutsu-resolve--text (commit path)
  "Return PATH at COMMIT with jj's conflict markers, or nil if not text.
The markers are snapshots, which show every term as it is and in the
order jj records them; its diff markers pair them by a heuristic."
  (let* ((bytes (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (let ((coding-system-for-read 'no-conversion))
                    (unless (eq 0 (majutsu-jj-insert
                                   "--color=never"
                                   "--config" "ui.conflict-marker-style=snapshot"
                                   "file" "show" "-T" ""
                                   "-r" commit "--"
                                   (majutsu-jj-fileset-quote path)))
                      (user-error "Cannot read %s at %s" path commit)))
                  (buffer-string)))
         (text (decode-coding-string bytes 'utf-8-unix)))
    (and (not (string-search "\0" text))
         (equal bytes (encode-coding-string text 'utf-8-unix))
         text)))

(defmacro majutsu-resolve--with-text (text &rest body)
  "Run BODY in a buffer holding TEXT, with its conflicts bound to `conflicts'."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,text)
     (let ((conflicts (majutsu-conflict-parse-buffer)))
       ,@body)))

(defun majutsu-resolve--diff (from to)
  "Return the lines of TO compared with those of FROM.
Each is a cons of ?+, ?- or a space, and the line."
  (let* ((old (vconcat (split-string from "\n" t)))
         (new (vconcat (split-string to "\n" t)))
         (head 0)
         (old-end (length old))
         (new-end (length new))
         lines)
    ;; What the two share at either end needs no table.
    (while (and (< head old-end) (< head new-end)
                (equal (aref old head) (aref new head)))
      (setq head (1+ head)))
    (while (and (> old-end head) (> new-end head)
                (equal (aref old (1- old-end)) (aref new (1- new-end))))
      (setq old-end (1- old-end) new-end (1- new-end)))
    (let* ((rows (- old-end head))
           (columns (- new-end head))
           ;; common[i][j]: longest common subsequence of what follows.
           (common (make-vector (1+ rows) nil))
           (i 0) (j 0))
      (dotimes (row (1+ rows))
        (aset common row (make-vector (1+ columns) 0)))
      (cl-loop for row downfrom (1- rows) to 0
               do (cl-loop for column downfrom (1- columns) to 0
                           do (aset (aref common row) column
                                    (if (equal (aref old (+ head row))
                                               (aref new (+ head column)))
                                        (1+ (aref (aref common (1+ row)) (1+ column)))
                                      (max (aref (aref common (1+ row)) column)
                                           (aref (aref common row) (1+ column)))))))
      (dotimes (index head)
        (push (cons ?\s (aref old index)) lines))
      (while (or (< i rows) (< j columns))
        (cond ((and (< i rows) (< j columns)
                    (equal (aref old (+ head i)) (aref new (+ head j))))
               (push (cons ?\s (aref old (+ head i))) lines)
               (setq i (1+ i) j (1+ j)))
              ((and (< i rows)
                    (or (= j columns)
                        (>= (aref (aref common (1+ i)) j)
                            (aref (aref common i) (1+ j)))))
               (push (cons ?- (aref old (+ head i))) lines)
               (setq i (1+ i)))
              (t
               (push (cons ?+ (aref new (+ head j))) lines)
               (setq j (1+ j)))))
      (cl-loop for index from old-end below (length old)
               do (push (cons ?\s (aref old index)) lines)))
    (nreverse lines)))

;;; Inserting

(defun majutsu-resolve--insert-heading (prefix text)
  "Insert TEXT as a section heading behind the display-only PREFIX.
Point never enters the prefix and copying the heading yields TEXT."
  (let ((start (point)))
    (magit-insert-heading text)
    (add-text-properties start (point)
                         (list 'line-prefix prefix 'wrap-prefix prefix))))

(defun majutsu-resolve--label (conflict term)
  "Return what jj calls the TERM of CONFLICT.
jj labels a term with the revision it is from and what that revision was
to the command which made the conflict.  A conflict from before jj wrote
labels has none; then a term goes by its number, as in jj's messages."
  (let ((label (car (majutsu-conflict-term-cell conflict term))))
    (if (or (null label) (string-empty-p label))
        (format (if (eq (plist-get term :kind) 'add) "side #%d" "base #%d")
                (1+ (plist-get term :index)))
      (majutsu-file-tree-escape-label label))))

(defun majutsu-resolve--insert-term (conflict term what entry prefix &optional from)
  "Insert the TERM of CONFLICT behind PREFIX.
WHAT says how: `snapshot' shows its text, `from' names it as where a
change starts and keeps its text folded, and `to' shows its text as a
change from the term FROM.  ENTRY is the file's, and tells whether the
term has a file at all."
  (let* ((text (cdr (majutsu-conflict-term-cell conflict term)))
         (body (concat prefix "  "))
         (properties (list 'line-prefix body 'wrap-prefix body)))
    (magit-insert-section (majutsu-resolve-term-section term (eq what 'from))
      (majutsu-resolve--insert-heading
       prefix
       (concat (pcase what ('from "from ") ('to "to ") (_ ""))
               (majutsu-resolve--label conflict term)
               (propertize
                (cond ((equal "absent"
                              (nth (plist-get term :index)
                                   (plist-get entry
                                              (if (eq (plist-get term :kind) 'add)
                                                  :adds
                                                :removes))))
                       "  (no file)")
                      ((string-empty-p text) "  (empty)")
                      ((not (string-suffix-p "\n" text)) "  (no final newline)")
                      (t ""))
                'font-lock-face 'shadow)))
      (if (eq what 'to)
          (pcase-dolist (`(,sign . ,line)
                         (majutsu-resolve--diff
                          (cdr (majutsu-conflict-term-cell conflict from)) text))
            (insert (apply #'propertize (format "%c%s\n" sign line)
                           'font-lock-face (pcase sign
                                             (?+ 'diff-added)
                                             (?- 'diff-removed)
                                             (_ 'diff-context))
                           properties)))
        (let ((start (point)))
          (insert text)
          (unless (bolp) (insert "\n"))
          (add-text-properties
           start (point)
           (append properties '(font-lock-face diff-context))))))))

(defun majutsu-resolve--insert-conflict (conflict number total entry prefix)
  "Insert CONFLICT, the NUMBER-th of TOTAL in the file of ENTRY, behind PREFIX.
jj records a conflict as a snapshot and then changes, each from one term
to the next.  Where there is one such change, the snapshot is a change
from the same term, and both are shown as that: what each did to the
text they started from."
  (magit-insert-section (majutsu-resolve-conflict-section number)
    (majutsu-resolve--insert-heading
     prefix
     (propertize (format "Conflict %d of %d" number total)
                 'font-lock-face 'magit-section-secondary-heading))
    (cl-flet ((insert-term (kind index what &optional from)
                (majutsu-resolve--insert-term
                 conflict (list :kind kind :index index) what entry
                 (concat prefix "  ")
                 (and from (list :kind 'remove :index from)))))
      (if (cdr (majutsu-conflict-removes conflict))
          (progn
            (insert-term 'add 0 'snapshot)
            (cl-loop for _ in (majutsu-conflict-removes conflict) for index from 0
                     do (insert-term 'remove index 'from)
                     do (insert-term 'add (1+ index) 'to index)))
        (insert-term 'remove 0 'from)
        (insert-term 'add 0 'to 0)
        (insert-term 'add 1 'to 0)))))

(defun majutsu-resolve--insert-file (commit entry heading-prefix body-prefix)
  "Insert the conflicted file ENTRY of COMMIT as a leaf of the file tree.
HEADING-PREFIX and BODY-PREFIX are the tree's decoration."
  (let* ((path (plist-get entry :path))
         (text (majutsu-resolve--text commit path))
         (conflicts (and text (majutsu-resolve--with-text text conflicts))))
    (magit-insert-section section (jj-file path)
      (oset section keymap 'majutsu-resolve-section-map)
      (majutsu-resolve--insert-heading
       heading-prefix
       (concat (propertize (majutsu-file-tree-escape-label
                            (file-name-nondirectory path))
                           'font-lock-face 'magit-diff-file-heading
                           'help-echo path)
               (propertize
                (format "  %d sides%s" (length (plist-get entry :adds))
                        (if conflicts "" ", not text"))
                'font-lock-face 'shadow)))
      (cl-loop for conflict in conflicts for number from 1
               do (majutsu-resolve--insert-conflict
                   conflict number (length conflicts) entry body-prefix)))))

;;;###autoload
(defun majutsu-resolve-insert-conflicts (&optional revset)
  "Insert the conflicts of REVSET, if it names one conflicted commit.
REVSET defaults to the single revision a diff buffer shows, or to `@'."
  (when-let* ((revset (or revset
                          (if (derived-mode-p 'majutsu-diff-mode)
                              (majutsu-diff--metadata-revision)
                            "@")))
              (commit (majutsu-resolve--commit revset))
              (files (majutsu-resolve--files commit)))
    (magit-insert-section section (majutsu-resolve-conflicts-section commit)
      (magit-insert-heading
        (concat
         (format "Conflicts (%d %s)" (length files)
                 (if (cdr files) "files" "file"))
         (when-let* ((origins (majutsu-resolve--origins commit)))
           (propertize (concat "  from " (string-join origins ", "))
                       'font-lock-face 'shadow))))
      (majutsu-file-tree-insert
       (majutsu-file-tree-build files (lambda (entry) (plist-get entry :path)))
       (apply-partially #'majutsu-resolve--insert-file commit)
       nil t)
      ;; Files and directories would otherwise paint over the changes.
      (magit-map-sections
       (lambda (child)
         (when (magit-section-match '(jj-file jj-directory) child)
           (oset child selective-highlight t)))
       section)
      (insert "\n"))))

;;; Point

(defun majutsu-resolve--ancestor (class)
  "Return the section at point or its nearest ancestor of CLASS."
  (let ((section (magit-current-section)))
    (while (and section (not (cl-typep section class)))
      (setq section (oref section parent)))
    section))

(defun majutsu-resolve--at-point ()
  "Return what point is on in a Conflicts section, as a plist.
The keys are `:commit', `:path', `:number' and `:term'; those below the
section at point are nil.  Return nil outside such a section."
  (when-let* ((root (majutsu-resolve--ancestor 'majutsu-resolve-conflicts-section)))
    (let ((file (majutsu-resolve--ancestor 'majutsu-file-section))
          (conflict (majutsu-resolve--ancestor 'majutsu-resolve-conflict-section))
          (term (majutsu-resolve--ancestor 'majutsu-resolve-term-section)))
      (list :commit (oref root value)
            :path (and file (oref file value))
            :number (and conflict (oref conflict value))
            :term (and term (oref term value))))))

;;; Writing

(defun majutsu-resolve--run (commit &rest args)
  "Run `jj resolve' on COMMIT with ARGS, then refresh.
COMMIT must still be visible: resolving a commit that was rewritten since
it was shown would bring it back as a divergent change."
  (unless (eq 0 (apply #'majutsu-call-jj "resolve"
                       "-r" (format "%s & ::visible_heads()" commit)
                       args))
    (majutsu-refresh)
    (user-error "jj did not resolve; see the process buffer"))
  (majutsu-refresh))

(defun majutsu-resolve--write (commit path text)
  "Make TEXT the contents of the conflicted PATH at COMMIT.
Conflict markers left in TEXT stay conflicts."
  (let ((file (make-nearby-temp-file "majutsu-resolve-"))
        (coding-system-for-write 'utf-8-unix))
    (unwind-protect
        (progn
          (write-region text nil file nil 'silent)
          ;; jj hands its editor the file to edit as the last argument.
          (majutsu-resolve--run
           commit "--tool" ":editor"
           "--config" (majutsu-jj--toml-array-config
                       "ui.editor"
                       (list "cp" (majutsu-convert-filename-for-jj file)))
           "--" (majutsu-jj-fileset-quote path)))
      (delete-file file))))

(defun majutsu-resolve--choices (conflict)
  "Return an alist from descriptions to what CONFLICT can become."
  (append
   (cl-loop for kind in '(add remove)
            for cells in (list (majutsu-conflict-adds conflict)
                               (majutsu-conflict-removes conflict))
            append (cl-loop for cell in cells for index from 0
                            collect (cons (majutsu-resolve--label
                                           conflict (list :kind kind :index index))
                                          (cdr cell))))
   '(("All sides, one after the other" . all))))

(defun majutsu-resolve--take-in-conflict (commit path number term)
  "Settle conflict NUMBER of PATH at COMMIT with TERM, or ask with what."
  (let ((text (majutsu-resolve--text commit path)))
    (majutsu-resolve--write
     commit path
     (majutsu-resolve--with-text text
       (let* ((conflict (or (nth (1- number) conflicts)
                            (user-error "%s has no conflict %d any more"
                                        path number)))
              (choice (if term
                          (cdr (majutsu-conflict-term-cell conflict term))
                        (let ((choices (majutsu-resolve--choices conflict)))
                          (cdr (assoc (completing-read
                                       (format "Conflict %d becomes: " number)
                                       choices nil t)
                                      choices))))))
         (goto-char (majutsu-conflict-begin-pos conflict))
         (if (eq choice 'all)
             (majutsu-conflict-keep-all)
           (majutsu-conflict-resolve-with conflict choice))
         (buffer-string))))))

(defun majutsu-resolve--sides (commit path count)
  "Return what jj calls each of the COUNT sides of PATH at COMMIT.
Every file of a commit has the same sides, so where PATH is not text the
labels come from a file that is."
  (or (seq-some
       (lambda (path)
         (when-let* ((text (majutsu-resolve--text commit path))
                     (conflict (car (majutsu-resolve--with-text text conflicts))))
           (cl-loop for index below count
                    collect (majutsu-resolve--label
                             conflict (list :kind 'add :index index)))))
       (cons path (mapcar (lambda (entry) (plist-get entry :path))
                          (majutsu-resolve--files commit))))
      (cl-loop for index from 1 to count collect (format "side #%d" index))))

(defun majutsu-resolve--take-file (commit path)
  "Settle every conflict of PATH at COMMIT with one of its sides."
  (let* ((count (length (plist-get (car (majutsu-resolve--files commit path))
                                   :adds)))
         (sides (majutsu-resolve--sides commit path count))
         (side (seq-position
                sides
                (completing-read (format "%s becomes: " path) sides nil t))))
    (if (= count 2)
        ;; jj can take a whole side, also of a file that is not text or
        ;; that one side lacks, but only where there are two.
        (majutsu-resolve--run commit "--tool" (if (= side 0) ":ours" ":theirs")
                              "--" (majutsu-jj-fileset-quote path))
      (let ((text (or (majutsu-resolve--text commit path)
                      (user-error "jj takes a whole side only of a two-sided conflict"))))
        (majutsu-resolve--write
         commit path
         (majutsu-resolve--with-text text
           (while conflicts
             (majutsu-conflict-resolve-with
              (car conflicts)
              (cdr (nth side (majutsu-conflict-adds (car conflicts)))))
             (setq conflicts (majutsu-conflict-parse-buffer)))
           (buffer-string)))))))

;;; Commands

(defun majutsu-resolve--goto-conflicts ()
  "Move to the Conflicts section of the current buffer, if it has one."
  (when-let* ((section (seq-find
                        (lambda (child)
                          (cl-typep child 'majutsu-resolve-conflicts-section))
                        (oref magit-root-section children))))
    (goto-char (oref section start))
    (magit-section-update-highlight)
    section))

;;;###autoload
(defun majutsu-resolve (&optional revision)
  "Resolve at point, or show the conflicts of REVISION.
On a term of a conflict, the conflict becomes that text.  On a conflict,
ask what it becomes; on a file, ask which side all of it becomes.  Each
is one jj operation.  On the heading of conflicts that come from an
earlier change, show that change at its conflicts.

Anywhere else, show REVISION, the revision at point or `@', at its
conflicts.  With a prefix argument, read the revision."
  (interactive
   (list (and (or current-prefix-arg (not (majutsu-resolve--at-point)))
              (let ((default (or (bound-and-true-p majutsu-buffer-blob-commit-id)
                                 (majutsu-revision-at-point)
                                 "@")))
                (if current-prefix-arg
                    (majutsu-read-revset "Resolve revision" :default default)
                  default)))))
  (if revision
      (progn
        (unless (majutsu-resolve--commit revision)
          (user-error "%s is not one conflicted revision" revision))
        (majutsu-diff-revset revision)
        (majutsu-resolve--goto-conflicts))
    (pcase-let (((map :commit :path :number :term)
                 (majutsu-resolve--at-point)))
      (cond (number (majutsu-resolve--take-in-conflict commit path number term)
                    (majutsu-resolve--goto-rest path))
            (path (majutsu-resolve--take-file commit path)
                  (majutsu-resolve--goto-rest path))
            (t (majutsu-resolve (majutsu-resolve--read-origin commit)))))))

(defun majutsu-resolve--goto-rest (path)
  "Move to what is left to resolve: in PATH, or else in the revision."
  (when-let* ((root (majutsu-resolve--goto-conflicts)))
    (magit-map-sections
     (lambda (section)
       (when (and (cl-typep section 'majutsu-file-section)
                  (equal (oref section value) path))
         (goto-char (oref (or (car (oref section children)) section) start))))
     root)
    (magit-section-update-highlight)))

(defun majutsu-resolve--read-origin (commit)
  "Return the change COMMIT's conflicts come from, asking if there are several."
  (let ((origins (or (majutsu-resolve--origins commit)
                     (user-error "Move to a file, a conflict or one of its terms"))))
    (car (split-string
          (if (cdr origins)
              (completing-read "Conflicts come from: " origins nil t)
            (car origins))
          " "))))

(defvar-local majutsu-resolve--shown-in nil
  "The buffer whose Conflicts section this file was visited from.")

(defun majutsu-resolve--refresh-shown ()
  "Refresh the conflicts this file was visited from, which saving it changed."
  (when (buffer-live-p majutsu-resolve--shown-in)
    (with-current-buffer majutsu-resolve--shown-in
      (majutsu-refresh))))

(defun majutsu-resolve-visit ()
  "Visit the conflicted file at point to edit its markers.
The working copy's file is visited as it is; another revision's opens as
an editable blob, and saving that resolves the conflict there.  Saving
either refreshes the conflicts shown here."
  (interactive)
  (pcase-let (((map :commit :path :number :term) (majutsu-resolve--at-point))
              (shown-in (current-buffer)))
    (unless path
      (user-error "Move to a file, a conflict or one of its terms"))
    (let ((buffer
           (if (equal commit (car (majutsu-resolve--lines
                                   "log" "--no-graph" "-r" "@" "-T" "commit_id")))
               (find-file-noselect (expand-file-name path (majutsu-file--root)))
             (majutsu-find-file-noselect commit path))))
      (pop-to-buffer buffer)
      (setq majutsu-resolve--shown-in shown-in)
      (add-hook 'after-save-hook #'majutsu-resolve--refresh-shown nil t)
      (when majutsu-buffer-blob-path
        (majutsu-blob-edit-start))
      (majutsu-conflict-ensure-mode)
      (if-let* ((conflict (and number
                               (nth (1- number) (majutsu-conflict-parse-buffer)))))
          (majutsu-conflict--goto-term conflict term)
        (majutsu-conflict-goto-nearest)))))

;;; _
(provide 'majutsu-resolve)
;;; majutsu-resolve.el ends here
