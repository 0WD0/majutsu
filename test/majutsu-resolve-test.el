;;; majutsu-resolve-test.el --- Conflict workbench integration tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Exercise the public workbench commands against isolated real jj repositories.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-resolve)
(require 'majutsu-jj-integration)

(defmacro majutsu-resolve-test--with-repo (repo &rest body)
  "Run BODY in REPO and clean up every buffer created by the test."
  (declare (indent 1) (debug (symbol body)))
  `(majutsu-jj-integration-with-repo ,repo
     (let ((existing-buffers (buffer-list)))
       (unwind-protect
           (save-window-excursion ,@body)
         (dolist (buffer (cl-set-difference (buffer-list) existing-buffers))
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (set-buffer-modified-p nil)
               (let ((kill-buffer-query-functions nil))
                 (kill-buffer buffer)))))))))

(defun majutsu-resolve-test--id (repo)
  "Return the full working-copy commit ID in REPO."
  (string-trim
   (majutsu-jj-integration-output
    repo "log" "-r" "@" "--no-graph" "-T" "commit_id")))

(defun majutsu-resolve-test--conflict (repo paths &optional sides writer base-text)
  "Create inherited conflicts in REPO at PATHS, returning the child ID.
SIDES defaults to two.  WRITER, when supplied, writes a side given the
repository, relative path, and one-based side number.  BASE-TEXT defaults
to a single base line."
  (dolist (path paths)
    (let ((file (majutsu-jj-integration-file repo path)))
      (make-directory (file-name-directory file) t)
      (majutsu-jj-integration-write-text file (or base-text "base\n"))))
  (majutsu-jj-integration-call repo "describe" "-m" "base")
  (let ((base (majutsu-resolve-test--id repo))
        parents)
    (dotimes (index (or sides 2))
      (majutsu-jj-integration-call repo "new" base)
      (dolist (path paths)
        (if writer
            (funcall writer repo path (1+ index))
          (majutsu-jj-integration-write-text
           (majutsu-jj-integration-file repo path)
           (format "side %d\n" (1+ index)))))
      (majutsu-jj-integration-call repo "describe" "-m" (format "side %d" index))
      (push (majutsu-resolve-test--id repo) parents))
    (apply #'majutsu-jj-integration-call repo "new" (nreverse parents))
    ;; The child inherits the conflict without introducing its own diff.
    (majutsu-jj-integration-call repo "new" "@")
    (majutsu-resolve-test--id repo)))

(defun majutsu-resolve-test--replace (buffer text)
  "Replace BUFFER's contents with TEXT as one undoable edit."
  (with-current-buffer buffer
    (undo-boundary)
    (atomic-change-group
      (erase-buffer)
      (insert text))
    (undo-boundary)))

(defun majutsu-resolve-test--section (section type value)
  "Find the descendant of SECTION having TYPE and VALUE."
  (magit-section--opportunistic-wash section)
  (if (and (magit-section-match type section)
           (equal (oref section value) value))
      section
    (cl-loop for child in (oref section children)
             thereis (majutsu-resolve-test--section child type value))))

(defun majutsu-resolve-test--select-side (workbench path conflict kind index)
  "Select a KIND term at zero-based INDEX under numbered CONFLICT in PATH."
  (with-current-buffer workbench
    (let* ((file (should (majutsu-resolve-test--section
                         magit-root-section 'jj-file path)))
           (block (should (majutsu-resolve-test--section
                          file 'majutsu-resolve-conflict-section conflict)))
           (side (should (majutsu-resolve-test--section
                         block 'majutsu-resolve-side-section
                         (list :kind kind :index index)))))
      (goto-char (oref side start))
      side)))

(ert-deftest majutsu-resolve/integration-conflict-tree-navigation-and-scope ()
  "Nested entries navigate safely after edits and apply only their own file."
  (majutsu-resolve-test--with-repo repo
    (let* ((path "src/你好.txt")
           (context (concat (mapconcat (lambda (index) (format "context %d" index))
                                       (number-sequence 1 12) "\n") "\n"))
           (pin (majutsu-resolve-test--conflict
                 repo (list path "outside.txt") 3
                 (lambda (directory relative side)
                   (majutsu-jj-integration-write-text
                    (majutsu-jj-integration-file directory relative)
                    (concat (unless (= side 2) (format "first ◆ 你好 %d\n" side))
                            context (format "second café %d\n" side))))
                 (concat "first base\n" context "second base\n")))
           (operation (majutsu-jj-operation-id))
           (workbench (majutsu-resolve-setup-buffer pin))
           (outside-native (majutsu-jj-integration-output
                            repo "file" "show" "-T" "" "-r" pin "outside.txt"))
           draft baseline outside)
      ;; Inspecting the tree neither creates editor drafts nor writes to jj.
      (with-current-buffer workbench
        (dolist (entry majutsu-resolve--records)
          (should-not (majutsu-resolve--file-temporary (cdr entry)))
          (should-not (buffer-live-p (majutsu-resolve--file-buffer (cdr entry)))))
        (should (= 2 (length (oref (majutsu-resolve-test--section
                                   magit-root-section 'jj-file path) children)))))
      (should (equal operation (majutsu-jj-operation-id)))
      ;; The deleted first region is an empty side, not a deleted file.
      (majutsu-resolve-test--select-side workbench path 1 'add 1)
      (with-current-buffer workbench
        (let ((conflict (oref (majutsu-resolve--ancestor
                              'majutsu-resolve-conflict-section) conflict)))
          (should (equal "" (cdr (nth 1 (majutsu-conflict-adds conflict)))))))
      (majutsu-resolve-test--select-side workbench path 1 'add 2)
      (setq draft (with-current-buffer workbench
                    (call-interactively #'majutsu-resolve-visit-file))
            baseline (with-current-buffer draft (buffer-string)))
      (with-current-buffer draft
        (should (equal (majutsu-conflict--term-at-point (majutsu-conflict-at-point))
                       '(:kind add :index 2)))
        (majutsu-conflict-keep-side 3))
      ;; Resolving an earlier block invalidates every later source position.
      ;; Refuse the stale entry and refresh, rather than jumping to a new block.
      (majutsu-resolve-test--select-side workbench path 2 'add 1)
      (with-current-buffer workbench
        (should-error (majutsu-resolve-visit-file) :type 'user-error)
        (should (= 1 (length (oref (majutsu-resolve-test--section
                                   magit-root-section 'jj-file path) children)))))
      (majutsu-resolve-test--select-side workbench path 1 'add 1)
      (with-current-buffer workbench (majutsu-resolve-visit-file))
      (with-current-buffer draft
        (let ((conflict (majutsu-conflict-at-point)))
          (should (equal (majutsu-conflict--term-at-point conflict)
                         '(:kind add :index 1)))
          (should (equal (cdr (nth 1 (majutsu-conflict-adds conflict)))
                         "second café 2\n"))))
      (setq outside (with-current-buffer workbench
                      (majutsu-resolve-visit-file "outside.txt")))
      (majutsu-resolve-test--replace outside "outside resolution\n")
      (majutsu-resolve-test--select-side workbench path 1 'remove 0)
      (with-current-buffer workbench (majutsu-resolve-discard))
      (should (equal baseline (with-current-buffer draft (buffer-string))))
      (should (equal "outside resolution\n" (with-current-buffer outside (buffer-string))))
      ;; Applying from a side means applying the enclosing file's draft,
      ;; not choosing that side or including a neighboring pending draft.
      (majutsu-resolve-test--select-side workbench path 1 'add 2)
      (with-current-buffer workbench (majutsu-resolve-visit-file))
      (with-current-buffer draft (majutsu-conflict-keep-side 3))
      (with-current-buffer workbench (majutsu-refresh-buffer))
      (majutsu-resolve-test--select-side workbench path 1 'add 0)
      (let* ((result (with-current-buffer workbench (majutsu-resolve-apply)))
             (actual (majutsu-jj-integration-output
                      repo "file" "show" "-T" "" "-r" result "--"
                      (majutsu-jj-fileset-quote path))))
        (should-not (equal pin result))
        (should (string-prefix-p "first ◆ 你好 3\n" actual))
        (with-temp-buffer
          (insert actual)
          (should (= 1 (length (majutsu-conflict-parse-buffer)))))
        (should (equal outside-native
                       (majutsu-jj-integration-output
                        repo "file" "show" "-T" "" "-r" result "outside.txt"))))
      (majutsu-resolve-test--select-side workbench path 1 'add 0)
      (should (eq outside (with-current-buffer workbench
                            (majutsu-resolve-next-file)))))))

(ert-deftest majutsu-resolve/integration-inherited-multiside-directory-drafts ()
  "Saved drafts stay private; collapsed directory apply preserves other conflicts."
  (majutsu-resolve-test--with-repo repo
    (let* ((paths '("docs/first$right\nname.el" "docs/second.txt" "outside.txt"))
           (pin (majutsu-resolve-test--conflict
                 repo paths 3
                 (lambda (directory path side)
                   (majutsu-jj-integration-write-text
                    (majutsu-jj-integration-file directory path)
                    (format ";; ◆ ○ 你好 café\n(message \"side %d\")\n" side)))))
           (workbench (majutsu-resolve-setup-buffer "@"))
           (drafts (mapcar (lambda (path)
                             (with-current-buffer workbench
                               (majutsu-resolve-visit-file path)))
                           paths))
           (first (nth 0 drafts))
           (second (nth 1 drafts))
           (outside (nth 2 drafts))
           (second-baseline (with-current-buffer second (buffer-string)))
           (outside-baseline (with-current-buffer outside (buffer-string)))
           (operation (majutsu-jj-operation-id)))
      (should (equal (majutsu-jj-integration-output
                      repo "diff" "--summary" "-r" pin)
                     ""))
      (dolist (path paths)
        (should (equal (majutsu-jj-conflicted-files pin (majutsu-jj-fileset-quote path))
                       (list (list :path path :sides 3)))))
      ;; Compare the whole draft against native jj output: some Unicode
      ;; truncations become valid ASCII and would evade an encoding check.
      (cl-mapc
       (lambda (path draft)
         (should (equal (with-current-buffer draft (buffer-string))
                        (majutsu-jj-integration-output
                         repo "file" "show" "-T" "" "-r" pin "--"
                         (majutsu-jj-fileset-quote path)))))
       paths drafts)
      (with-current-buffer first
        (should (derived-mode-p 'emacs-lisp-mode))
        (should majutsu-conflict-mode))
      (cl-mapc #'majutsu-resolve-test--replace
               drafts '("(message \"◆ ○ 你好 café\")\n" "discard this\n" "outside resolved\n"))
      (dolist (draft drafts)
        (with-current-buffer draft
          (save-buffer)
          (should-not (buffer-modified-p))
          (should-not (file-in-directory-p buffer-file-name repo))))
      (should (equal (majutsu-resolve-test--id repo) pin))
      (should (equal (majutsu-jj-operation-id) operation))
      ;; Undo back to baseline means no pending change, despite having saved.
      (with-current-buffer outside
        (let ((last-command nil))
          (undo-only 1))
        (should (equal (buffer-string) outside-baseline))
        (should (equal (majutsu-resolve-apply) pin)))
      (should (equal (majutsu-jj-operation-id) operation))
      ;; Discard must work on saved-but-unapplied contents as well.
      (with-current-buffer second (majutsu-resolve-discard))
      (setq second (with-current-buffer workbench
                     (majutsu-resolve-visit-file "docs/second.txt")))
      (should (equal (with-current-buffer second (buffer-string)) second-baseline))
      (majutsu-resolve-test--replace second "")
      (majutsu-resolve-test--replace outside "outside resolved\n")
      (with-current-buffer second (save-buffer))
      (with-current-buffer outside (save-buffer))
      ;; Move @ while keeping the pinned ancestor visible.  Refresh must not
      ;; reinterpret @ or overwrite any of the private drafts.
      (majutsu-jj-integration-call repo "new" "@")
      (let ((new-pin
             (with-current-buffer workbench
               (majutsu-refresh-buffer)
               (let ((directory (majutsu-resolve-test--section
                                 magit-root-section 'jj-directory "docs/")))
                 (should directory)
                 (goto-char (oref directory start))
                 (magit-section-hide directory)
                 (let ((coding-system-for-write 'utf-16le))
                   (majutsu-resolve-apply))))))
        (should-not (equal new-pin pin))
        (should (equal (majutsu-jj-integration-output
                        repo "file" "show" "-r" new-pin "--"
                        (majutsu-jj-fileset-quote "docs/first$right\nname.el"))
                       "(message \"◆ ○ 你好 café\")\n"))
        ;; Empty draft means an existing empty file, not a deletion.
        (should (equal (majutsu-jj-integration-output
                        repo "file" "show" "-r" new-pin "docs/second.txt") ""))
        (should (equal (majutsu-jj-conflicted-files new-pin)
                       '((:path "outside.txt" :sides 3))))
        (with-current-buffer outside
          (should (equal (buffer-string) "outside resolved\n"))
          (should-not (buffer-modified-p))
          (let ((resolved (majutsu-resolve-apply)))
            (should-not (majutsu-jj-conflicted-files resolved))
            (should (equal (majutsu-jj-integration-output
                            repo "file" "show" "-r" resolved "outside.txt")
                           "outside resolved\n"))))))))

(ert-deftest majutsu-resolve/integration-hidden-target-retains-draft ()
  "An externally rewritten pin must not be silently redirected to its successor."
  (majutsu-resolve-test--with-repo repo
    (let* ((pin (majutsu-resolve-test--conflict repo '("file.txt")))
           (workbench (majutsu-resolve-setup-buffer pin))
           (draft (with-current-buffer workbench
                    (majutsu-resolve-visit-file "file.txt"))))
      (majutsu-resolve-test--replace draft "keep my resolution\n")
      (with-current-buffer draft (save-buffer))
      (majutsu-jj-integration-call repo "describe" "-r" pin "-m" "external rewrite")
      (let ((successor (majutsu-resolve-test--id repo))
            (operation (majutsu-jj-operation-id)))
        (with-current-buffer draft
          (should-error (majutsu-resolve-apply) :type 'user-error)
          (should (equal (buffer-string) "keep my resolution\n")))
        (should (equal (majutsu-jj-operation-id) operation))
        (should (equal (majutsu-resolve-test--id repo) successor))
        (should (equal (majutsu-jj-conflicted-files successor)
                       '((:path "file.txt" :sides 2))))))))

(ert-deftest majutsu-resolve/integration-postwrite-query-failure-blocks-retry ()
  "A real query failure after writing must retain the draft and prevent blind retry."
  (majutsu-resolve-test--with-repo repo
    (let* ((pin (majutsu-resolve-test--conflict repo '("file.txt")))
           (workbench (majutsu-resolve-setup-buffer pin))
           (draft (with-current-buffer workbench
                    (majutsu-resolve-visit-file "file.txt")))
           (apply-contents (symbol-function 'majutsu-file-apply-contents))
           (jj-insert (symbol-function 'majutsu--jj-insert))
           wrote)
      (majutsu-resolve-test--replace draft "resolved but unverified\n")
      (with-current-buffer draft (save-buffer))
      (cl-letf (((symbol-function 'majutsu-file-apply-contents)
                 (lambda (&rest args)
                   (prog1 (apply apply-contents args) (setq wrote t))))
                ((symbol-function 'majutsu--jj-insert)
                 (lambda (return-error &rest args)
                   (if (and wrote (member "file" (flatten-tree args))
                            (member "list" (flatten-tree args)))
                       (funcall jj-insert return-error
                                "file" "list" "--majutsu-test-invalid-option")
                     (apply jj-insert return-error args)))))
        (with-current-buffer draft
          (should-error (majutsu-resolve-apply) :type 'user-error)))
      (should wrote)
      (should (equal (majutsu-jj-integration-output
                      repo "file" "show" "-r" "@" "file.txt")
                     "resolved but unverified\n"))
      (let ((operation (majutsu-jj-operation-id)))
        (with-current-buffer draft
          (should (equal (buffer-string) "resolved but unverified\n"))
          (should-error (majutsu-resolve-apply) :type 'user-error)
          (should (equal (buffer-string) "resolved but unverified\n")))
        (should (equal (majutsu-jj-operation-id) operation))))))

(ert-deftest majutsu-resolve/integration-nontext-conflicts-refuse-editing ()
  "NUL, invalid UTF-8, and file/symlink conflicts cannot become lossy text drafts."
  (majutsu-resolve-test--with-repo repo
    (let* ((paths '("nul.bin" "invalid.txt" "structure.txt"))
           (pin
            (majutsu-resolve-test--conflict
             repo paths 2
             (lambda (root path side)
               (let ((file (majutsu-jj-integration-file root path)))
                 (pcase path
                   ("nul.bin"
                    (majutsu-jj-integration-write-bytes file (unibyte-string 0 side 10)))
                   ("invalid.txt"
                    (majutsu-jj-integration-write-bytes file (unibyte-string 255 side 10)))
                   ("structure.txt"
                    (if (= side 1)
                        (majutsu-jj-integration-write-text file "regular file\n")
                      (delete-file file)
                      (make-symbolic-link "symlink-target" file))))))))
           (workbench (majutsu-resolve-setup-buffer pin))
           (operation (majutsu-jj-operation-id)))
      (dolist (path paths)
        (should (equal (majutsu-jj-conflicted-files pin (majutsu-jj-fileset-quote path))
                       (list (list :path path :sides 2)))))
      (with-current-buffer workbench
        (dolist (path paths)
          (should-error (majutsu-resolve-visit-file path) :type 'user-error)))
      (should (equal (majutsu-resolve-test--id repo) pin))
      (should (equal (majutsu-jj-operation-id) operation)))))

(provide 'majutsu-resolve-test)
;;; majutsu-resolve-test.el ends here
