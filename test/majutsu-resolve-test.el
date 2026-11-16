;;; majutsu-resolve-test.el --- Conflicts section tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Exercise the Conflicts section and its commands against real jj
;; repositories.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-resolve)
(require 'majutsu-diff)
(require 'majutsu-jj-integration)

(defmacro majutsu-resolve-test--with-repo (repo &rest body)
  "Run BODY in REPO and kill every buffer it creates."
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

(defun majutsu-resolve-test--id (repo &optional revset)
  "Return the commit id of REVSET, or of `@', in REPO."
  (string-trim
   (majutsu-jj-integration-output
    repo "log" "-r" (or revset "@") "--no-graph" "-T" "commit_id")))

(defun majutsu-resolve-test--show (repo revset path)
  "Return PATH at REVSET in REPO as jj materializes it."
  (majutsu-jj-integration-output repo "file" "show" "-r" revset path))

(defun majutsu-resolve-test--conflicted (repo revset)
  "Return the conflicted paths of REVSET in REPO."
  (mapcar (lambda (entry) (plist-get entry :path))
          (let ((default-directory (file-name-as-directory repo)))
            (majutsu-jj-conflicted-files revset))))

(defun majutsu-resolve-test--merge (repo files &optional sides)
  "Create a merge of SIDES changes in REPO which conflict in FILES.
FILES is an alist from paths to functions that return the text of the
base, called with 0, and of each side, called with its number.  Leave a
child of the merge as the working copy and return the merge's id."
  (cl-flet ((write (number)
              (pcase-dolist (`(,path . ,text) files)
                (let ((file (majutsu-jj-integration-file repo path))
                      (text (funcall text number)))
                  (make-directory (file-name-directory file) t)
                  (cond ((stringp text)
                         (majutsu-jj-integration-write-bytes
                          file (encode-coding-string text 'utf-8-unix)))
                        ((file-exists-p file) (delete-file file)))))))
    (write 0)
    (majutsu-jj-integration-call repo "describe" "-m" "base")
    (let ((base (majutsu-resolve-test--id repo))
          parents)
      (dotimes (index (or sides 2))
        (majutsu-jj-integration-call repo "new" base)
        (write (1+ index))
        (majutsu-jj-integration-call
         repo "describe" "-m" (format "side %d" (1+ index)))
        (push (majutsu-resolve-test--id repo) parents))
      (apply #'majutsu-jj-integration-call repo "new" "-m" "merge"
             (nreverse parents))
      (prog1 (majutsu-resolve-test--id repo)
        (majutsu-jj-integration-call repo "new" "-m" "child")))))

(defun majutsu-resolve-test--lines (prefix)
  "Return a function giving a base and sides that differ in two lines.
Each text is PREFIX and the number on the first and the last of five lines."
  (lambda (number)
    (format "%s first %d\nb\nc\nd\n%s last %d\n" prefix number prefix number)))

(defmacro majutsu-resolve-test--in-diff (revset &rest body)
  "Run BODY in the diff buffer of REVSET."
  (declare (indent 1) (debug (form body)))
  `(with-current-buffer (majutsu-diff-revset ,revset)
     ,@body))

(defun majutsu-resolve-test--goto (path &optional number term)
  "Move to the file PATH, its conflict NUMBER, or that conflict's TERM."
  (let ((found nil))
    (magit-map-sections
     (lambda (section)
       (when (and (not found)
                  (cl-typep section (cond (term 'majutsu-resolve-term-section)
                                          (number 'majutsu-resolve-conflict-section)
                                          (t 'majutsu-file-section)))
                  (equal (oref section value) (or term number path)))
         (goto-char (oref section start))
         (let ((at (majutsu-resolve--at-point)))
           (when (and at
                      (equal (plist-get at :path) path)
                      (equal (plist-get at :number) number))
             (setq found section)))))
     magit-root-section)
    (should found)))

(defun majutsu-resolve-test--operations (repo)
  "Return the number of operations in REPO."
  (length (split-string
           (majutsu-jj-integration-output
            repo "op" "log" "--no-graph" "-T" "id ++ \"\\n\"")
           "\n" t)))

(ert-deftest majutsu-resolve/section-lists-files-conflicts-and-terms ()
  "A conflicted revision shows its files, their conflicts and their terms."
  (majutsu-resolve-test--with-repo repo
    (let ((merge (majutsu-resolve-test--merge
                  repo `(("src/a.txt" . ,(majutsu-resolve-test--lines "a"))
                         ("b.txt" . ,(majutsu-resolve-test--lines "b"))))))
      (majutsu-resolve-test--in-diff merge
        (let ((root (majutsu-resolve--goto-conflicts)))
          (should root)
          (should (equal (oref root value) merge))
          (should (equal (majutsu-file-tree-files root) '("b.txt" "src/a.txt")))
          ;; The merge introduces the conflict.
          (should-not (string-match-p
                       "from" (buffer-substring (oref root start)
                                                (oref root content)))))
        (majutsu-resolve-test--goto "src/a.txt")
        (should (equal (majutsu-file-at-point) "src/a.txt"))
        (majutsu-resolve-test--goto "src/a.txt" 2 '(:kind add :index 1))
        (should (equal (majutsu-resolve--at-point)
                       (list :commit merge :path "src/a.txt" :number 2
                             :term '(:kind add :index 1))))
        ;; Both sides are shown as changes to the text they started from.
        (should (equal (buffer-substring-no-properties
                        (oref (magit-current-section) content)
                        (oref (magit-current-section) end))
                       "-a last 0\n+a last 2\n"))
        (should (string-match-p "\\`to [k-z]\\{8\\} [0-9a-f]\\{8\\} \"side 2\"$"
                                (buffer-substring-no-properties
                                 (point) (line-end-position))))
        (majutsu-resolve-test--goto "src/a.txt" 1 '(:kind remove :index 0))
        (should (string-match-p "\\`from [k-z]\\{8\\} [0-9a-f]\\{8\\} \"base\"$"
                                (buffer-substring-no-properties
                                 (point) (line-end-position))))
        (should (oref (magit-current-section) hidden)))
      ;; The child inherits the conflict and says where it comes from.
      (majutsu-resolve-test--in-diff "@"
        (let ((root (majutsu-resolve--goto-conflicts)))
          (should (string-match-p
                   "from [k-z]\\{8\\} merge"
                   (buffer-substring-no-properties (oref root start)
                                                   (oref root content)))))))))

(ert-deftest majutsu-resolve/diff ()
  (should (equal (majutsu-resolve--diff "a\nb\nc\n" "a\nB\nc\nd\n")
                 '((?\s . "a") (?- . "b") (?+ . "B") (?\s . "c") (?+ . "d"))))
  (should (equal (majutsu-resolve--diff "" "a\n") '((?+ . "a"))))
  (should (equal (majutsu-resolve--diff "a\nb\n" "b\n")
                 '((?- . "a") (?\s . "b"))))
  (should (equal (majutsu-resolve--diff "a\nb\na\n" "a\n")
                 '((?\s . "a") (?- . "b") (?- . "a")))))

(ert-deftest majutsu-resolve/terms-follow-the-order-jj-records ()
  "A rebase is shown as what the destination and the rebased change did
to the parents; a conflict with more terms as a snapshot and changes."
  (majutsu-resolve-test--with-repo repo
    (cl-flet ((write (text) (majutsu-jj-integration-write-text
                             (majutsu-jj-integration-file repo "f") text))
              (headings ()
                (let (headings)
                  (magit-map-sections
                   (lambda (section)
                     (when (cl-typep section 'majutsu-resolve-term-section)
                       (push (replace-regexp-in-string
                              "[k-z]\\{8\\} [0-9a-f]\\{8\\} " ""
                              (buffer-substring-no-properties
                               (oref section start)
                               (1- (or (oref section content)
                                       (oref section end)))))
                             headings)))
                   (majutsu-resolve--goto-conflicts))
                  (nreverse headings))))
      (write "x\n")
      (majutsu-jj-integration-commit repo "base")
      (write "x-A\n")
      (majutsu-jj-integration-commit repo "A")
      (majutsu-jj-integration-call repo "new" "description(exact:\"base\n\")" "-m" "B")
      (write "x-B\n")
      (majutsu-jj-integration-call repo "rebase" "-r" "@" "-d" "description(exact:\"A\n\")")
      (majutsu-resolve-test--in-diff "@"
        (should (equal (headings)
                       '("from \"base\" (parents of rebased revision)"
                         "to \"A\" (rebase destination)"
                         "to \"B\" (rebased revision)"))))
      ;; A change made on the conflict and moved elsewhere takes the
      ;; conflict's terms with it, labels and all.
      (majutsu-jj-integration-call repo "new" "-m" "C")
      (write "x-C\n")
      (majutsu-jj-integration-call repo "new" "description(exact:\"base\n\")" "-m" "D")
      (write "x-D\n")
      (majutsu-jj-integration-call repo "rebase" "-r" "description(exact:\"C\n\")" "-d" "@")
      (majutsu-resolve-test--in-diff "description(exact:\"C\n\")"
        (should (equal (headings)
                       '("\"D\" (rebase destination)"
                         "from \"B\" (rebased revision)"
                         "to \"base\" (parents of rebased revision)"
                         "from \"A\" (rebase destination)"
                         "to \"C\" (rebased revision)")))))))

(ert-deftest majutsu-resolve/a-side-without-the-file-says-so ()
  (majutsu-resolve-test--with-repo repo
    (majutsu-resolve-test--merge
     repo `(("deleted.txt" . ,(lambda (number)
                                (and (/= number 2) (format "d %d\n" number))))))
    (majutsu-resolve-test--in-diff "@-"
      (majutsu-resolve-test--goto "deleted.txt" 1 '(:kind add :index 1))
      (should (string-suffix-p "\"side 2\"  (no file)"
                               (buffer-substring-no-properties
                                (point) (line-end-position))))
      (majutsu-resolve-test--goto "deleted.txt" 1 '(:kind add :index 0))
      (should-not (string-match-p "no file"
                                  (buffer-substring-no-properties
                                   (point) (line-end-position)))))))

(ert-deftest majutsu-resolve/a-revision-without-conflicts-has-no-section ()
  (majutsu-resolve-test--with-repo repo
    (majutsu-jj-integration-write-text
     (majutsu-jj-integration-file repo "a.txt") "a\n")
    (majutsu-resolve-test--in-diff "@"
      (should-not (majutsu-resolve--goto-conflicts)))))

(ert-deftest majutsu-resolve/taking-a-side-settles-one-conflict ()
  "On a side, the conflict becomes that side in one operation."
  (majutsu-resolve-test--with-repo repo
    (let ((merge (majutsu-resolve-test--merge
                  repo `(("a.txt" . ,(majutsu-resolve-test--lines "a")))))
          (before (majutsu-resolve-test--operations repo)))
      (majutsu-resolve-test--in-diff merge
        (majutsu-resolve-test--goto "a.txt" 1 '(:kind add :index 1))
        (call-interactively #'majutsu-resolve))
      (should (= (majutsu-resolve-test--operations repo) (1+ before)))
      (let ((text (majutsu-resolve-test--show repo "@-" "a.txt")))
        (should (string-prefix-p "a first 2\nb\nc\nd\n<<<<<<<" text))
        (should (string-match-p "conflict 1 of 1" text)))
      ;; The buffer follows the change to its new commit.
      (majutsu-resolve-test--in-diff "@-"
        (let ((root (majutsu-resolve--goto-conflicts)))
          (should (equal (oref root value) (majutsu-resolve-test--id repo "@-"))))
        (majutsu-resolve-test--goto "a.txt" 1 '(:kind remove :index 0))
        (call-interactively #'majutsu-resolve))
      (should (equal (majutsu-resolve-test--show repo "@-" "a.txt")
                     "a first 2\nb\nc\nd\na last 0\n"))
      ;; The child was conflicted only through its parent.
      (should-not (majutsu-resolve-test--conflicted repo "@")))))

(ert-deftest majutsu-resolve/a-conflict-asks-what-it-becomes ()
  (majutsu-resolve-test--with-repo repo
    (let ((merge (majutsu-resolve-test--merge
                  repo `(("a.txt" . ,(lambda (number)
                                       (format "a %d\n" number)))))))
      (majutsu-resolve-test--in-diff merge
        (majutsu-resolve-test--goto "a.txt" 1)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _)
                     (cl-loop for (choice) in choices
                              for expected in '("\\`[k-z]+ [0-9a-f]+ \"side 1\"\\'"
                                                "\\`[k-z]+ [0-9a-f]+ \"side 2\"\\'"
                                                "\\`[k-z]+ [0-9a-f]+ \"base\"\\'"
                                                "\\`All sides, one after the other\\'")
                              do (should (string-match-p expected choice)))
                     (should (= (length choices) 4))
                     "All sides, one after the other")))
          (call-interactively #'majutsu-resolve)))
      (should (equal (majutsu-resolve-test--show repo "@-" "a.txt")
                     "a 1\na 2\n")))))

(ert-deftest majutsu-resolve/a-file-takes-a-whole-side ()
  "A side of a file is taken whole, also where the file is not text
or one side deletes it."
  (majutsu-resolve-test--with-repo repo
    (let ((merge (majutsu-resolve-test--merge
                  repo `(("text.txt" . ,(majutsu-resolve-test--lines "t"))
                         ("binary" . ,(lambda (number)
                                        (format "\0binary %d" number)))
                         ("deleted.txt" . ,(lambda (number)
                                             (and (/= number 2)
                                                  (format "d %d\n" number))))))))
      (dolist (path '("text.txt" "binary" "deleted.txt"))
        (majutsu-resolve-test--in-diff "@-"
          (majutsu-resolve-test--goto path)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _)
                       (should (= (length choices) 2))
                       (should (string-match-p "\"side 2\"\\'" (cadr choices)))
                       (cadr choices))))
            (call-interactively #'majutsu-resolve))))
      (should-not (equal merge (majutsu-resolve-test--id repo "@-")))
      (should-not (majutsu-resolve-test--conflicted repo "@-"))
      (should (equal (majutsu-resolve-test--show repo "@-" "text.txt")
                     "t first 2\nb\nc\nd\nt last 2\n"))
      (should (equal (majutsu-resolve-test--show repo "@-" "binary")
                     "\0binary 2"))
      (should-not (file-exists-p
                   (majutsu-jj-integration-file repo "deleted.txt"))))))

(ert-deftest majutsu-resolve/more-than-two-sides ()
  "With three sides a term and a whole side of a text file can be taken."
  (majutsu-resolve-test--with-repo repo
    (majutsu-resolve-test--merge
     repo `(("a.txt" . ,(majutsu-resolve-test--lines "a"))
            ("b.txt" . ,(majutsu-resolve-test--lines "b"))
            ("binary" . ,(lambda (number) (format "\0binary %d" number))))
     3)
    (majutsu-resolve-test--in-diff "@-"
      (majutsu-resolve-test--goto "a.txt" 2 '(:kind add :index 2))
      (call-interactively #'majutsu-resolve))
    (should (string-suffix-p "\nd\na last 3\n"
                             (majutsu-resolve-test--show repo "@-" "a.txt")))
    (majutsu-resolve-test--in-diff "@-"
      (majutsu-resolve-test--goto "b.txt")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (should (= (length choices) 3))
                   (nth 2 choices))))
        (call-interactively #'majutsu-resolve)))
    (should (equal (majutsu-resolve-test--show repo "@-" "b.txt")
                   "b first 3\nb\nc\nd\nb last 3\n"))
    ;; jj takes a whole side only where there are two.
    (majutsu-resolve-test--in-diff "@-"
      (majutsu-resolve-test--goto "binary")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (nth 2 choices))))
        (should-error (call-interactively #'majutsu-resolve)
                      :type 'user-error)))
    (should (equal (majutsu-resolve-test--conflicted repo "@-")
                   '("a.txt" "binary")))))

(ert-deftest majutsu-resolve/a-rewritten-commit-is-not-resolved ()
  "A section of a commit that has since been rewritten refuses to act."
  (majutsu-resolve-test--with-repo repo
    (let ((merge (majutsu-resolve-test--merge
                  repo `(("a.txt" . ,(majutsu-resolve-test--lines "a"))))))
      (with-temp-buffer
        (majutsu-diff-mode)
        (let ((inhibit-read-only t))
          (magit-insert-section (root)
            (majutsu-resolve-insert-conflicts merge)))
        (majutsu-jj-integration-call repo "describe" "-r" merge "-m" "reworded")
        (majutsu-resolve-test--goto "a.txt" 1 '(:kind add :index 0))
        (should-error (call-interactively #'majutsu-resolve) :type 'user-error))
      (should (equal (majutsu-resolve-test--conflicted repo "@-") '("a.txt")))
      (should (equal (string-trim
                      (majutsu-jj-integration-output
                       repo "log" "--no-graph" "-r" "@-"
                       "-T" "if(divergent, \"divergent\", \"single\")"))
                     "single")))))

(ert-deftest majutsu-resolve/elsewhere-shows-the-conflicts-of-a-revision ()
  (majutsu-resolve-test--with-repo repo
    (let ((merge (majutsu-resolve-test--merge
                  repo `(("a.txt" . ,(majutsu-resolve-test--lines "a"))))))
      (with-temp-buffer
        (majutsu-resolve merge)
        (should (derived-mode-p 'majutsu-diff-mode))
        (should (cl-typep (magit-current-section)
                          'majutsu-resolve-conflicts-section)))
      (should-error (majutsu-resolve "root()") :type 'user-error))))

(ert-deftest majutsu-resolve/visiting-opens-the-file-at-the-term ()
  "The working copy's file is visited; another revision's is an editable blob."
  (majutsu-resolve-test--with-repo repo
    (let ((merge (majutsu-resolve-test--merge
                  repo `(("a.txt" . ,(majutsu-resolve-test--lines "a"))))))
      (majutsu-resolve-test--in-diff "@"
        (majutsu-resolve-test--goto "a.txt" 2 '(:kind add :index 1))
        (majutsu-resolve-visit))
      (with-current-buffer (window-buffer)
        (should (equal buffer-file-name
                       (majutsu-jj-integration-file repo "a.txt")))
        (should majutsu-conflict-mode)
        ;; The term starts at its marker line.
        (should (looking-at-p "\\+\\{7\\} .*\"side 2\"\na last 2$")))
      (majutsu-resolve-test--in-diff merge
        (majutsu-resolve-test--goto "a.txt" 1)
        (majutsu-resolve-visit))
      (with-current-buffer (window-buffer)
        (should majutsu-blob-edit-mode)
        (should (looking-at-p "<<<<<<< conflict 1 of 2"))))))

(ert-deftest majutsu-resolve/saving-a-visited-file-refreshes-the-conflicts ()
  "Editing the markers of a revision by hand settles what is shown."
  (majutsu-resolve-test--with-repo repo
    (let* ((merge (majutsu-resolve-test--merge
                   repo `(("a.txt" . ,(lambda (number) (format "a %d\n" number))))))
           (diff (majutsu-diff-revset "@-")))
      (with-current-buffer diff
        (should (equal (oref (majutsu-resolve--goto-conflicts) value) merge))
        (majutsu-resolve-test--goto "a.txt" 1)
        (majutsu-resolve-visit))
      (with-current-buffer (window-buffer)
        (erase-buffer)
        (insert "a 1 and 2\n")
        (save-buffer))
      (should (equal (majutsu-resolve-test--show repo "@-" "a.txt") "a 1 and 2\n"))
      (with-current-buffer diff
        (should-not (majutsu-resolve--goto-conflicts))))))

(ert-deftest majutsu-resolve/the-heading-leads-to-where-the-conflicts-come-from ()
  (majutsu-resolve-test--with-repo repo
    (let ((merge (majutsu-resolve-test--merge
                  repo `(("a.txt" . ,(majutsu-resolve-test--lines "a"))))))
      (majutsu-resolve-test--in-diff "@"
        (majutsu-resolve--goto-conflicts)
        (call-interactively #'majutsu-resolve))
      (with-current-buffer (window-buffer)
        (should (equal (oref (magit-current-section) value) merge))
        ;; The merge introduces them, so there is nowhere further to go.
        (should-error (call-interactively #'majutsu-resolve) :type 'user-error)))))

(ert-deftest majutsu-resolve/resolving-moves-to-what-is-left ()
  (majutsu-resolve-test--with-repo repo
    (majutsu-resolve-test--merge
     repo `(("a.txt" . ,(majutsu-resolve-test--lines "a"))
            ("b.txt" . ,(majutsu-resolve-test--lines "b"))))
    (majutsu-resolve-test--in-diff "@-"
      (majutsu-resolve-test--goto "a.txt" 1 '(:kind add :index 0))
      (call-interactively #'majutsu-resolve)
      (should (equal (majutsu-resolve--at-point)
                     (list :commit (majutsu-resolve-test--id repo "@-")
                           :path "a.txt" :number 1 :term nil)))
      (majutsu-resolve-test--goto "a.txt" 1 '(:kind add :index 0))
      (call-interactively #'majutsu-resolve)
      ;; Nothing is left in the file; its revision still has conflicts.
      (should (cl-typep (magit-current-section)
                        'majutsu-resolve-conflicts-section)))))

(ert-deftest majutsu-resolve/highlight-keeps-the-faces-of-the-changes ()
  "Highlighting the current section must not paint over the changes."
  (majutsu-resolve-test--with-repo repo
    (majutsu-resolve-test--merge
     repo `(("src/a.txt" . ,(majutsu-resolve-test--lines "a"))))
    (majutsu-resolve-test--in-diff "@-"
      (cl-flet ((change-p (position)
                  (memq (get-text-property position 'font-lock-face)
                        '(diff-added diff-removed diff-context)))
                (highlighted-p (position)
                  (cl-some (lambda (overlay)
                             (and (<= (overlay-start overlay) position)
                                  (< position (overlay-end overlay))))
                           magit-section-highlight-overlays)))
        (let ((root (majutsu-resolve--goto-conflicts)))
          (magit-map-sections
           (lambda (section)
             (goto-char (oref section start))
             (magit-section-update-highlight t)
             (let ((checked 0))
               (cl-loop for position from (oref root start)
                        below (oref root end)
                        when (change-p position)
                        do (cl-incf checked)
                        (should-not (highlighted-p position)))
               (should (> checked 0))))
           root))))))

(ert-deftest majutsu-resolve/tree-decoration-is-display-only ()
  "Copying a heading yields its text, not the tree drawn before it."
  (majutsu-resolve-test--with-repo repo
    (majutsu-resolve-test--merge
     repo `(("src/a.txt" . ,(majutsu-resolve-test--lines "a"))))
    (majutsu-resolve-test--in-diff "@-"
      (majutsu-resolve-test--goto "src/a.txt")
      (should (string-match-p "[│├└]" (get-text-property (point) 'line-prefix)))
      (should (string-prefix-p
               "a.txt  2 sides"
               (buffer-substring-no-properties (point) (line-end-position)))))))

(provide 'majutsu-resolve-test)
;;; majutsu-resolve-test.el ends here
