;;; majutsu-diff-test.el --- Tests for diff section rendering  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for diff section rendering.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu)

(defmacro majutsu-diff-test--with-transient-context (prefix suffix &rest body)
  "Run BODY as if transient PREFIX were reading infix SUFFIX."
  `(cl-letf (((symbol-function 'transient-prefix-object)
              (lambda () (get ,prefix 'transient--prefix)))
             ((symbol-function 'transient-suffix-object)
              (lambda (&optional _command)
                (get ,suffix 'transient--suffix))))
     ,@body))

(defconst majutsu-diff-test--refinable-git-diff
  (string-join
   '("diff --git a/foo b/foo"
     "index 1234567..89abcde 100644"
     "--- a/foo"
     "+++ b/foo"
     "@@ -1 +1 @@"
     "-alpha old omega"
     "+alpha new omega"
     "@@ -10 +10 @@"
     "-bravo old omega"
     "+bravo new omega")
   "\n")
  "Git diff with two hunks that produce word-level refinement overlays.")

(defmacro majutsu-diff-test--with-washed-git-hunks (&rest body)
  "Create a real Majutsu diff buffer with washed Git hunks, then run BODY."
  (declare (indent 0) (debug t))
  ;; Exercise the upstream default that caused issue #42.  Majutsu must
  ;; override it buffer-locally instead of changing the user's global value.
  `(cl-letf (((default-value 'diff-refine) 'font-lock))
     (with-temp-buffer
       (majutsu-diff-mode)
       (setq-local majutsu-diff-backend 'git)
       (let ((inhibit-read-only t)
             (magit-section-inhibit-markers t)
             (majutsu-diff-paint-whitespace nil))
         (magit-insert-section (diffbuf)
           (magit-insert-heading "Diff")
           (majutsu--insert-diff-hunks
            majutsu-diff-test--refinable-git-diff '("--git")))
         ,@body))))

(defun majutsu-diff-test--hunk-sections ()
  "Return all hunk sections in the current buffer in display order."
  (let (hunks)
    (cl-labels ((walk (section)
                  (when (eq (oref section type) 'jj-hunk)
                    (push section hunks))
                  (dolist (child (oref section children))
                    (walk child))))
      (walk magit-root-section))
    (nreverse hunks)))

(defun majutsu-diff-test--fine-overlays (&optional section)
  "Return diff refinement overlays, optionally limited to SECTION."
  (let ((beg (if section (oref section start) (point-min)))
        (end (if section (oref section end) (point-max))))
    (cl-remove-if-not
     (lambda (overlay)
       (eq (overlay-get overlay 'diff-mode) 'fine))
     (overlays-in beg end))))

(defun majutsu-diff-test--font-lock-refine-markers ()
  "Return overlays created by `diff--font-lock-refined'."
  (cl-remove-if-not
   (lambda (overlay)
     (overlay-get overlay 'diff--font-lock-refined))
   (overlays-in (point-min) (point-max))))

(ert-deftest majutsu-diff-inserts-toggleable-sections ()
  "Diff sections should create headings so users can toggle them."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (diff (string-join
                  '("diff --git a/foo b/foo"
                    "index 1234567..89abcde 100644"
                    "--- a/foo"
                    "+++ b/foo"
                    "@@ -1 +1 @@"
                    "-foo"
                    "+bar")
                  "\n")))
      (magit-insert-section (diffbuf)
        (magit-insert-heading "Diff")
        (majutsu--insert-diff-hunks diff))
      (let* ((root magit-root-section)
             (file-section (car (oref root children)))
             (hunk-section (car (oref file-section children))))
        (should (eieio-object-p file-section))
        (should (oref file-section content))
        (should (eieio-object-p hunk-section))
        (should (oref hunk-section content))))))

(ert-deftest majutsu-diff-wash-diffs-parses-diffstat-and-diff ()
  "Diff washer should parse output containing both `--stat' and `--git'."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (output (propertize
                    (string-join
                     '("foo | 1 +"
                       "1 file changed, 1 insertion(+), 0 deletions(-)"
                       "diff --git a/foo b/foo"
                       "index 1234567..89abcde 100644"
                       "--- a/foo"
                       "+++ b/foo"
                       "@@ -1 +1 @@"
                       "-foo"
                       "+bar")
                     "\n")
                    'fontified nil)))
      (magit-insert-section (diffbuf)
        (magit-insert-section (diff-root)
          (insert output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-diff-wash-diffs '("--stat")))))
      (let* ((diff-root (car (oref magit-root-section children)))
             (children (oref diff-root children))
             (diffstat (seq-find (lambda (sec) (eq (oref sec type) 'diffstat))
                                 children))
             (diffstat-file (seq-find (lambda (sec)
                                        (eq (oref sec type) 'jj-file))
                                      (oref diffstat children)))
             (diff-file (seq-find (lambda (sec)
                                    (and (eq (oref sec type) 'jj-file)
                                         (equal (oref sec value) "foo")))
                                  children)))
        (should (eieio-object-p diffstat))
        (should (eieio-object-p diffstat-file))
        (should (seq-find (lambda (sec)
                            (and (eq (oref sec type) 'jj-file)
                                 (equal (oref sec value) "foo")))
                          (oref diffstat children)))
        (should (eieio-object-p diff-file))
        (should (oref diff-file content))
        (should-not (text-properties-at 0 (oref diffstat-file value)))
        (should-not (text-properties-at 0 (oref diff-file value)))))))

(ert-deftest majutsu-jj-wash/failed-diff-keeps-stderr-outside-parser ()
  "Failed `--git --stat' output should keep its tree and append stderr safely."
  (with-temp-buffer
    (magit-section-mode)
    (let* ((inhibit-read-only t)
           (magit-section-inhibit-markers t)
           (real-wash-file (symbol-function 'majutsu-diff-wash-file))
           (wash-file-calls 0)
           (stdout
            (concat
             (string-join
              '("foo | 1 +"
                "1 file changed, 1 insertion(+), 0 deletions(-)"
                "diff --git a/foo b/foo"
                "index 1234567..89abcde 100644"
                "--- a/foo"
                "+++ b/foo"
                "@@ -1 +1 @@"
                "-foo"
                "+bar")
              "\n")
             "\n")))
      (cl-letf (((symbol-function 'majutsu-process-file)
                 (lambda (_program _infile destination _display &rest _args)
                   (insert stdout)
                   (write-region "\e[31mdiff --git malformed diagnostic\e[0m\n"
                                 nil (cadr destination) nil 'silent)
                   1))
                ((symbol-function 'majutsu-diff-wash-file)
                 (lambda ()
                   ;; Fail instead of hanging if a malformed pseudo-header is
                   ;; ever allowed into the real washer's progress loop.
                   (when (> (cl-incf wash-file-calls) 2)
                     (ert-fail "Diff washer stopped advancing"))
                   (funcall real-wash-file))))
        (magit-insert-section (diffbuf)
          (magit-insert-section (diff-root)
            (should (= 1 (majutsu-jj-wash
                           #'majutsu-diff-wash-diffs 'wash-anyway
                           "diff" "--git" "--stat"))))))
      (let* ((diff-root (car (oref magit-root-section children)))
             (children (oref diff-root children))
             (diffstat (seq-find (lambda (section)
                                   (eq (oref section type) 'diffstat))
                                 children))
             (stat-file (and diffstat
                             (seq-find (lambda (section)
                                         (eq (oref section type) 'jj-file))
                                       (oref diffstat children))))
             (diff-file (seq-find (lambda (section)
                                    (and (eq (oref section type) 'jj-file)
                                         (equal (oref section value) "foo")))
                                  children)))
        (should (= wash-file-calls 1))
        (should (eieio-object-p diffstat))
        (should (eieio-object-p stat-file))
        (should (eieio-object-p diff-file))
        (should (string-match-p "jj .*diff .*failed (exit 1)"
                                (buffer-string)))
        (should (string-match-p "diff --git malformed diagnostic"
                                (buffer-string)))
        (should-not (string-match-p (regexp-quote "\e[")
                                    (buffer-string)))))))

(ert-deftest majutsu-diff-file-metadata/preserves-ambiguous-and-quoted-paths ()
  "Structured metadata preserves paths ambiguous or escaped in Git headers."
  (let* ((output (concat
                  "[\"renamed\",\"literal\\\"old.bin\","
                  "\"foo b/bar.bin\"]\n"
                  "[\"copied\",\"source.bin\",\"line\\nbreak.bin\"]\n"))
         (entries (majutsu-diff--parse-file-metadata-output output)))
    (should (equal entries
                   '((:status "renamed" :source "literal\"old.bin"
                      :target "foo b/bar.bin")
                     (:status "copied" :source "source.bin"
                      :target "line\nbreak.bin"))))))

(ert-deftest majutsu-diff-file-metadata/validates-git-c-quoted-controls ()
  "Header validation should understand Git escapes without sourcing paths from them."
  (let* ((path (concat "café\t\"" (string 7)))
         (header (concat
                  "diff --git \"a/caf\\303\\251\\t\\042\\a\" "
                  "\"b/caf\\303\\251\\t\\042\\a\"\n"))
         (entry (list :status "modified" :source path :target path)))
    (should (majutsu-diff--git-header-paths-match-p header entry))
    (should-not (majutsu-diff--git-quoted-token-value "\"\\777\""))))

(ert-deftest majutsu-diff-file-metadata/query-matches-range-and-filesets ()
  "The sidecar query must use the displayed diff's exact range and filesets."
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-jj-insert)
               (lambda (&rest args)
                 (setq seen args)
                 (insert "[\"modified\",\"src/a.el\",\"src/a.el\"]\n")
                 0)))
      (should (equal
               (majutsu-diff--query-file-metadata
                '("--from=A" "--to=B") '("src/a.el"))
               '((:status "modified" :source "src/a.el"
                  :target "src/a.el"))))
      (should (equal (seq-take seen 4)
                     (list "--ignore-working-copy" "diff" "-T"
                           majutsu-diff--file-metadata-template)))
      (should (equal (seq-drop seen 4)
                     '("--from=A" "--to=B" "--" "src/a.el"))))))

(ert-deftest majutsu-diff-file-metadata/count-mismatch-fails-closed ()
  "Metadata is not attached when its count differs from washed file sections."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (magit-section-inhibit-markers t))
      (magit-insert-section (root)
        (insert "diff --git a/one.bin b/one.bin\nnew file mode 100644\nBinary files /dev/null and b/one.bin differ\n")
        (save-restriction
          (narrow-to-region (point-min) (point-max))
          (majutsu-diff-wash-diffs '("--git"))))
      (let ((section (car (oref magit-root-section children))))
        (majutsu-diff--attach-file-metadata
         '((:status "added" :source "one.bin" :target "one.bin")
           (:status "added" :source "two.bin" :target "two.bin")))
        (should-not (majutsu-diff-file-metadata section))))))

(ert-deftest majutsu-diff-file-metadata/reordered-sidecar-fails-closed ()
  "Equal counts must not authorize reordered sidecar entries."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (magit-section-inhibit-markers t))
      (magit-insert-section (root)
        (insert "diff --git a/one.bin b/one.bin\nindex 1..2 100644\nBinary files a/one.bin and b/one.bin differ\n"
                "diff --git a/two.bin b/two.bin\nindex 3..4 100644\nBinary files a/two.bin and b/two.bin differ\n")
        (save-restriction
          (narrow-to-region (point-min) (point-max))
          (majutsu-diff-wash-diffs '("--git"))))
      (let ((sections (oref magit-root-section children)))
        (majutsu-diff--attach-file-metadata
         '((:status "modified" :source "two.bin" :target "two.bin")
           (:status "modified" :source "one.bin" :target "one.bin")))
        (should-not (seq-some #'majutsu-diff-file-metadata sections))))))

(ert-deftest majutsu-diff-file-metadata/raw-order-validation-rejects-reorder ()
  "Pre-wash validation must reject same-count metadata in a different order."
  (with-temp-buffer
    (insert "diff --git a/one.bin b/one.bin\nindex 1..2 100644\nBinary files a/one.bin and b/one.bin differ\n"
            "diff --git a/two.bin b/two.bin\nindex 3..4 100644\nBinary files a/two.bin and b/two.bin differ\n")
    (let* ((one '(:status "modified" :source "one.bin" :target "one.bin"))
           (two '(:status "modified" :source "two.bin" :target "two.bin")))
      (should (majutsu-diff--raw-file-metadata-consistent-p (list one two)))
      (should-not
       (majutsu-diff--raw-file-metadata-consistent-p (list two one))))))

(ert-deftest majutsu-diff-wash-file/accepts-quoted-git-display-header ()
  "A quoted Git display header should still form a section for sidecar binding."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (magit-section-inhibit-markers t))
      (magit-insert-section (root)
        (insert "diff --git \"a/old\\\"name.bin\" \"b/new\\\"name.bin\"\n"
                "similarity index 100%\nrename from old\nrename to new\n")
        (save-restriction
          (narrow-to-region (point-min) (point-max))
          (majutsu-diff-wash-diffs '("--git"))))
      (let ((section (car (oref magit-root-section children)))
            (metadata '(:status "renamed" :source "old\"name.bin"
                        :target "new\"name.bin")))
        (should (oref section header))
        (majutsu-diff--attach-file-metadata (list metadata))
        (should (equal (oref section value) "new\"name.bin"))
        (should (equal (majutsu-diff-file-metadata section) metadata))))))

(ert-deftest majutsu-diff-wash-diffs/malformed-header-always-advances ()
  "A future or malformed `diff --git' header must not trap the washer loop."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (magit-section-inhibit-markers t))
      (magit-insert-section (root)
        (insert "diff --git malformed-header\nunrecognized extended data\n"
                "diff --git a/good.bin b/good.bin\nindex 1..2 100644\n"
                "Binary files a/good.bin and b/good.bin differ\n")
        (save-restriction
          (narrow-to-region (point-min) (point-max))
          (with-timeout (1 (ert-fail "diff washer did not advance"))
            (majutsu-diff-wash-diffs '("--git")))))
      (should (= (length (oref magit-root-section children)) 2)))))

(ert-deftest majutsu-insert-diff/puts-filesets-after-separator ()
  "Diff command construction should pass filesets after --."
  (let (called heading)
    (with-temp-buffer
      (magit-section-mode)
      (majutsu-diff-mode)
      (setq buffer-read-only nil
            majutsu-buffer-diff-args '("--git")
            majutsu-buffer-diff-range '("--from=A" "--to=B")
            majutsu-buffer-diff-filesets '("src/a.el"))
      (cl-letf (((symbol-function 'majutsu-diff--backend-washer)
                 (lambda (&rest _) #'ignore))
                ((symbol-function 'magit-insert-heading)
                 (lambda (text) (setq heading text)))
                ((symbol-function 'majutsu-jj-wash)
                 (lambda (_washer _keep-error args)
                   (setq called args))))
        (majutsu-insert-diff)
        (should (equal called
                       '("diff" "--git" "--from=A" "--to=B"
                         "--" "src/a.el")))
        (should (equal heading
                       "jj diff --git --from=A --to=B -- src/a.el"))))))

(defun majutsu-diff-test--metadata-values (&optional description change-id)
  "Return one list of metadata values for tests.
Use DESCRIPTION and CHANGE-ID when non-nil."
  (list (or change-id "change-id")
        (vector "local")
        (vector (vector "tracked" "origin")
                (vector "ignored" "git"))
        "0123456789abcdef"
        "Arthur Heymans" "arthur@example.test" "author date"
        "Committer" "committer@example.test" "commit date"
        (or description "Summary\n\nBody")))

(defun majutsu-diff-test--encode-metadata-values (values)
  "Encode metadata VALUES as one NUL-framed record."
  (concat (mapconcat #'json-serialize values
                     majutsu-diff--revision-metadata-separator)
          majutsu-diff--revision-metadata-separator))

(defun majutsu-diff-test--metadata-output (&optional description change-id)
  "Return one encoded metadata record for tests.
Use DESCRIPTION and CHANGE-ID when non-nil."
  (majutsu-diff-test--encode-metadata-values
   (majutsu-diff-test--metadata-values description change-id)))

(defun majutsu-diff-test--metadata (&optional description)
  "Return parsed test metadata with DESCRIPTION."
  (majutsu-diff--parse-revision-metadata
   (majutsu-diff-test--metadata-output description)))

(ert-deftest majutsu-diff-metadata-revision/default-and-explicit-single ()
  "Default diffs and explicit revision options select a metadata revset."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range nil)
    (should (equal (majutsu-diff--metadata-revision) "@"))
    (setq-local majutsu-buffer-diff-range '("-r" "mine"))
    (should (equal (majutsu-diff--metadata-revision) "mine"))
    (setq-local majutsu-buffer-diff-range '("--revisions=mine"))
    (should (equal (majutsu-diff--metadata-revision) "mine"))
    (setq-local majutsu-buffer-diff-range '("--revisions" "mine"))
    (should (equal (majutsu-diff--metadata-revision) "mine"))))

(ert-deftest majutsu-diff-metadata-revision/ranges-and-empty-do-not-qualify ()
  "From/to ranges and an empty revision option never select TO metadata."
  (with-temp-buffer
    (majutsu-diff-mode)
    (dolist (range '(("--from=A" "--to=B")
                     ("--from" "A")
                     ("--to=B")
                     ("--revisions=")
                     ("-r" "")))
      (setq-local majutsu-buffer-diff-range range)
      (should-not (majutsu-diff--metadata-revision)))))

(ert-deftest majutsu-diff-parse-revision-metadata/preserves-empty-fields ()
  "JSON parsing preserves empty structured refs and scalar fields."
  (let* ((output (majutsu-diff-test--encode-metadata-values
                  '("change" [] [] "commit" "" "" "author date"
                    "" "" "commit date" "")))
         (fields (majutsu-diff--parse-revision-metadata output)))
    (should fields)
    (should-not (plist-get fields :local-bookmarks))
    (should-not (plist-get fields :remote-bookmarks))
    (should (equal (plist-get fields :author-name) ""))
    (should (equal (plist-get fields :description) ""))))

(ert-deftest majutsu-diff-parse-revision-metadata/preserves-nul-description ()
  "An encoded token transports NUL inside a description safely."
  (let* ((description (concat "Summary" (string 0) "Body"))
         (fields (majutsu-diff--parse-revision-metadata
                  (majutsu-diff-test--metadata-output description))))
    (should fields)
    (should (equal (plist-get fields :description) description))))

(ert-deftest majutsu-diff-parse-revision-metadata/rejects-malformed-bookmarks ()
  "Malformed JSON and remote entries without exact name/remote pairs fail."
  (should-not (majutsu-diff--parse-revision-metadata "not-json"))
  (dolist (case (list (list nil (vector))
                      (list (vector 1) (vector))
                      (list (vector) (vector (vector "name")))
                      (list (vector) (vector (vector "name" nil)))))
    (let ((record (majutsu-diff-test--metadata-values)))
      (setf (nth 1 record) (car case)
            (nth 2 record) (cadr case))
      (should-not (majutsu-diff--parse-revision-metadata
                   (majutsu-diff-test--encode-metadata-values record))))))

(ert-deftest majutsu-diff-parse-revision-metadata/rejects-zero-and-two-matches ()
  "Only one complete metadata record is accepted."
  (should-not (majutsu-diff--parse-revision-metadata ""))
  (let ((record (majutsu-diff-test--metadata-output)))
    (should-not (majutsu-diff--parse-revision-metadata
                 (concat record record)))))

(ert-deftest majutsu-diff-parse-revision-metadata/preserves-labelled-faces ()
  "Label properties should survive decoding for scalars and nested arrays."
  (let* ((values (majutsu-diff-test--metadata-values))
         (faces '(change-face local-face remote-face commit-face
                  author-name-face author-email-face author-date-face
                  committer-name-face committer-email-face committer-date-face
                  description-face))
         (output
          (concat
           (mapconcat
            (lambda (pair)
              (propertize (json-serialize (car pair))
                          'font-lock-face (cadr pair)))
            (cl-mapcar #'list values faces)
            majutsu-diff--revision-metadata-separator)
           majutsu-diff--revision-metadata-separator))
         (fields (majutsu-diff--parse-revision-metadata output)))
    (should (eq (get-text-property 0 'font-lock-face
                                   (plist-get fields :change-id))
                'change-face))
    (should (eq (get-text-property 0 'font-lock-face
                                   (car (plist-get fields :local-bookmarks)))
                'local-face))
    (should (eq (get-text-property
                 0 'font-lock-face
                 (caar (plist-get fields :remote-bookmarks)))
                'remote-face))
    (should (eq (get-text-property 0 'font-lock-face
                                   (plist-get fields :author-name))
                'author-name-face))
    (should (eq (get-text-property 0 'font-lock-face
                                   (plist-get fields :description))
                'description-face))))

(ert-deftest majutsu-diff-parse-revision-metadata/literal-esc-stays-data ()
  "Decoded terminal escapes must not recolor or corrupt adjacent fields."
  (let* ((description (concat "before" (string 27) "[31mRED"
                              (string 27) "[0m" (string 0) "after"))
         (values (majutsu-diff-test--metadata-values description))
         (tokens (mapcar #'json-serialize values)))
    (setf (nth 3 tokens)
          (propertize (nth 3 tokens) 'font-lock-face 'commit-face)
          (nth 10 tokens)
          (propertize (nth 10 tokens) 'font-lock-face 'description-face))
    (let ((fields
           (majutsu-diff--parse-revision-metadata
            (concat (mapconcat #'identity tokens
                               majutsu-diff--revision-metadata-separator)
                    majutsu-diff--revision-metadata-separator))))
      (should (equal (plist-get fields :description) description))
      (should (eq (get-text-property 0 'font-lock-face
                                     (plist-get fields :description))
                  'description-face))
      (should (eq (get-text-property 0 'font-lock-face
                                     (plist-get fields :commit-id))
                  'commit-face)))))

(ert-deftest majutsu-diff-revision-metadata/multi-revset-does-not-display ()
  "A `--revisions' revset resolving to two changes has no metadata."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-args '("--git")
                majutsu-buffer-diff-range '("--revisions=A|B"))
    (let ((record (concat (majutsu-diff-test--metadata-output) "\n")))
      (cl-letf (((symbol-function 'majutsu-jj-insert)
                 (lambda (&rest _args)
                   (insert record record)
                   0)))
        (should-not (majutsu-diff--revision-metadata))))))

(ert-deftest majutsu-diff-query-revision-metadata/uses-local-bookmarks ()
  "Query labelled local and remote bookmark components as structured JSON."
  (let (seen-args seen-global-args)
    (cl-letf (((symbol-function 'majutsu-jj-insert)
               (lambda (&rest args)
                 (setq seen-args args
                       seen-global-args majutsu-jj-global-arguments)
                 1)))
      (should-not (majutsu-diff--query-revision-metadata "@")))
    (let ((template (cadr (member "-T" seen-args))))
      (should (stringp template))
      (should (string-search
               "json(self.local_bookmarks().map(|ref| ref.name()))" template))
      (should (string-search
               "label(\"local_bookmarks map join name\"" template))
      (should (string-search "self.remote_bookmarks().map(|ref|" template))
      (should (string-search "json(ref.name())" template))
      (should (string-search "json(ref.remote())" template))
      (should (string-search "label(\"description\", json(self.description()))"
                             template))
      (should (string-prefix-p "concat(join(\"\\0\"," template))
      (should (string-suffix-p ", \"\\0\")" template))
      (should-not (string-search "separate(" template))
      (let ((cursor 0))
        (dolist (label '("change_id"
                         "local_bookmarks map join name"
                         "remote_bookmarks map join name remote"
                         "commit_id"
                         "author name"
                         "author email"
                         "author timestamp format"
                         "committer name"
                         "committer email"
                         "committer timestamp format"
                         "description"))
          (let ((position (string-search (format "label(\"%s\"" label)
                                         template cursor)))
            (should position)
            (setq cursor (1+ position)))))
      (should (string-search "\\0" template))
      (should (member "--limit" seen-args))
      (should (member "--color=always" seen-global-args))
      (should-not (member "--color=never" seen-global-args)))))

(ert-deftest majutsu-diff-format-refs/filters-git-and-applies-faces ()
  "Ref formatting hides synthetic git and escapes controls to one line."
  (let* ((refs (majutsu-diff--format-refs
                '(:local-bookmarks
                  ("local" "feature space" "literal@local" "sigil*?"
                   "line\nname" "line\\nname")
                  :remote-bookmarks
                  (("tracked" "origin")
                   ("ignored" "git")
                   ("remote only@mark" "up\rstream"))))))
    (should (equal (substring-no-properties refs)
                   (concat "local feature space literal@local sigil*? "
                           "line\\nname line\\\\nname origin/tracked "
                           "up\\rstream/remote only@mark")))
    (should-not (string-match-p "[\n\r]" refs))
    (should (eq (get-text-property 0 'font-lock-face refs)
                'magit-branch-local))
    (should (eq (get-text-property (string-match "origin" refs)
                                   'font-lock-face refs)
                'magit-branch-remote))))

(ert-deftest majutsu-diff-format-refs/preserves-native-faces ()
  "Native bookmark presentation should take precedence over fallback faces."
  (let* ((local (propertize "local" 'font-lock-face 'native-local-face))
         (remote (propertize "upstream" 'font-lock-face 'native-remote-face))
         (name (propertize "topic" 'font-lock-face 'native-remote-face))
         (refs (majutsu-diff--format-refs
                (list :local-bookmarks (list local)
                      :remote-bookmarks (list (list name remote))))))
    (should (eq (get-text-property 0 'font-lock-face refs)
                'native-local-face))
    (should (eq (get-text-property (string-match "upstream" refs)
                                   'font-lock-face refs)
                'native-remote-face))))

(ert-deftest majutsu-diff-revision-message/summary-face-survives-font-lock ()
  "The revision summary uses Magit's managed font-lock face."
  (with-temp-buffer
    (majutsu-diff-mode)
    (let ((inhibit-read-only t)
          (magit-section-inhibit-markers t)
          (metadata (majutsu-diff-test--metadata)))
      (cl-letf (((symbol-function 'majutsu-diff--revision-metadata)
                 (lambda () metadata)))
        (magit-insert-section (diffbuf)
          (majutsu-insert-diff-revision-message)))
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "Summary")
      (let ((face (get-text-property (1- (point)) 'font-lock-face)))
        (should (if (listp face)
                    (memq 'magit-diff-revision-summary face)
                  (eq face 'magit-diff-revision-summary)))))))

(ert-deftest majutsu-diff-revision-sections/render-empty-message-and-headings ()
  "Headers and an empty message are represented by toggleable sections."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t)
          (magit-section-inhibit-markers t)
          (metadata (majutsu-diff-test--metadata "")))
      (cl-letf (((symbol-function 'majutsu-diff--revision-metadata)
                 (lambda () metadata)))
        (magit-insert-section (diffbuf)
          (majutsu-insert-diff-revision-headers)
          (majutsu-insert-diff-revision-message)))
      (let ((children (oref magit-root-section children)))
        (should (equal (mapcar (lambda (section) (oref section type)) children)
                       '(commit-headers commit-message)))
        (dolist (section children)
          (should (oref section content))
          (magit-section-hide section)
          (should (oref section hidden)))
        (should (string-match-p "(no description)" (buffer-string)))))))

(ert-deftest majutsu-diff-revision-metadata/cache-key-invalidates-on-args-and-rev ()
  "Metadata is shared until formatting arguments or revision change."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-args '("--git")
                majutsu-buffer-diff-range '("-r" "A"))
    (let ((count 0))
      (cl-letf (((symbol-function 'majutsu-diff--query-revision-metadata)
                 (lambda (_rev)
                   (cl-incf count)
                   (majutsu-diff-test--metadata))))
        (majutsu-diff--revision-metadata)
        (majutsu-diff--revision-metadata)
        (should (= count 1))
        (setq-local majutsu-buffer-diff-args '("--git" "--stat"))
        (majutsu-diff--revision-metadata)
        (should (= count 2))
        (setq-local majutsu-buffer-diff-range '("-r" "B"))
        (majutsu-diff--revision-metadata)
        (should (= count 3))))))

(ert-deftest majutsu-diff-refresh/queries-once-and-runs-hooks-in-order ()
  "One refresh shares one query across headers and message, before diff."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-args '("--git")
                majutsu-buffer-diff-range '("-r" "A"))
    (let ((count 0)
          order)
      (cl-letf (((symbol-function 'majutsu-diff--query-revision-metadata)
                 (lambda (_rev)
                   (cl-incf count)
                   (majutsu-diff-test--metadata)))
                ((symbol-function 'majutsu-insert-diff-revision-headers)
                 (lambda ()
                   (push 'headers order)
                   (majutsu-diff--revision-metadata)))
                ((symbol-function 'majutsu-insert-diff-revision-message)
                 (lambda ()
                   (push 'message order)
                   (majutsu-diff--revision-metadata)))
                ((symbol-function 'majutsu-insert-diff)
                 (lambda () (push 'diff order))))
        (majutsu-diff-refresh-buffer))
      (should (= count 1))
      (should (equal (nreverse order) '(headers message diff))))))

(ert-deftest majutsu-diff-refresh/metadata-failure-does-not-block-diff ()
  "A failed metadata query is cached as nil and diff insertion continues."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-args '("--git")
                majutsu-buffer-diff-range '("-r" "missing"))
    (let ((count 0)
          diff-inserted)
      (cl-letf (((symbol-function 'majutsu-jj-insert)
                 (lambda (&rest _args)
                   (cl-incf count)
                   1))
                ((symbol-function 'majutsu-insert-diff)
                 (lambda () (setq diff-inserted t))))
        (majutsu-diff-refresh-buffer))
      (should (= count 1))
      (should diff-inserted))))

(ert-deftest majutsu-diff-remembered-args-filters-only-formatting-options ()
  "Only diff formatting options should be remembered per buffer."
  (should (equal (majutsu-diff--remembered-args
                  '("--stat"
                    "-r" "@-"
                    "--from" "main"
                    "--context=5"
                    "--ignore-all-space"))
                 '("--stat" "--context=5" "--ignore-all-space"))))

(ert-deftest majutsu-diff-refresh-buffer-does-not-ansi-wash-git-diff ()
  "Git diff buffers must preserve literal ANSI escapes in file content."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-args '("--git"))
    (setq-local majutsu-buffer-diff-range '("--revisions=@"))
    (let (seen-ansi seen-global-args)
      (cl-letf (((symbol-function 'magit-run-section-hook)
                 (lambda (&rest _)
                   (setq seen-ansi majutsu-process-apply-ansi-colors
                         seen-global-args majutsu-jj-global-arguments))))
        (majutsu-diff-refresh-buffer))
      (should-not seen-ansi)
      (should (member "--color=never" seen-global-args)))))

(ert-deftest majutsu-diff-set-buffer-args-does-not-clear-filesets ()
  "Updating diff args must not clear existing filesets unless requested."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-filesets '("a" "b"))
    (cl-letf (((symbol-function 'majutsu-diff-refresh-buffer) #'ignore))
      (majutsu-diff--set-buffer-args '("--summary")))
    (should (equal majutsu-buffer-diff-filesets '("a" "b")))
    (should (equal majutsu-buffer-diff-args '("--summary")))))

(ert-deftest majutsu-diff-refresh-keeps-transient-filesets ()
  "Refreshing from diff transient should update args, range and filesets."
  (with-temp-buffer
    (majutsu-diff-mode)
    (let (refreshed)
      (cl-letf (((symbol-function 'transient-args)
                 (lambda (&rest _)
                   '(("--stat") ("--from=A" "--to=B") ("src/a.el"))))
                ((symbol-function 'majutsu-diff-refresh-buffer)
                 (lambda () (setq refreshed t)))
                ((symbol-function 'majutsu-repository-config-id) #'ignore))
        (majutsu-diff-refresh))
      (should refreshed)
      (should (equal majutsu-buffer-diff-args '("--stat")))
      (should (equal majutsu-buffer-diff-range '("--from=A" "--to=B")))
      (should (equal majutsu-buffer-diff-filesets '("src/a.el"))))))

(ert-deftest majutsu-diff-transient-revset-completion-args/uses-transient-objects ()
  "Transient revset readers should complete in the matching jj context."
  (majutsu-diff-test--with-transient-context
      'majutsu-diff 'majutsu-diff:--from
    (should (equal (majutsu-transient-revset-completion-args)
                   '("diff" "--from"))))
  (majutsu-diff-test--with-transient-context
      'majutsu-ediff 'majutsu-ediff:--from
    (should (equal (majutsu-transient-revset-completion-args)
                   '("diff" "--from"))))
  (majutsu-diff-test--with-transient-context
      'majutsu-rebase 'majutsu-rebase:--after
    (should (equal (majutsu-transient-revset-completion-args)
                   '("rebase" "--insert-after"))))
  (majutsu-diff-test--with-transient-context
      'majutsu-new 'majutsu-new:-r
    (should (equal (majutsu-transient-revset-completion-args)
                   '("new" "-r"))))
  (majutsu-diff-test--with-transient-context
      'majutsu-squash 'majutsu-squash:--from
    (should (equal (majutsu-transient-revset-completion-args)
                   '("squash" "--from"))))
  (majutsu-diff-test--with-transient-context
      'majutsu-metaedit-transient 'majutsu-metaedit:-r
    (should (equal (majutsu-transient-revset-completion-args)
                   '("metaedit" "-r")))))

(ert-deftest majutsu-diff-transient-revset-completion-args/uses-prefix-jj-command ()
  "Transient revset completion should use the prefix jj command slot."
  (let ((prefix (make-instance 'majutsu-jj-transient-prefix
                               :command 'test-prefix
                               :jj-command '("custom" "command"))))
    (cl-letf (((symbol-function 'transient-prefix-object)
               (lambda () prefix))
              ((symbol-function 'transient-suffix-object)
               (lambda (&optional _command)
                 (get 'majutsu-diff:--from 'transient--suffix))))
      (should (equal (majutsu-transient-revset-completion-args)
                     '("custom" "command" "--from"))))))

(ert-deftest majutsu-diff-transient-revset-completion-args/uses-suffix-argument ()
  "Transient revset readers should read the jj option from the infix object."
  (majutsu-diff-test--with-transient-context
      'majutsu-restore 'majutsu-restore:--changes-in
    (should (equal (majutsu-transient-revset-completion-args)
                   '("restore" "--changes-in"))))
  (majutsu-diff-test--with-transient-context
      'majutsu-split 'majutsu-split:--insert-before
    (should (equal (majutsu-transient-revset-completion-args)
                   '("split" "--insert-before")))))

(ert-deftest majutsu-diff-transient-read-revset/uses-native-completion-context ()
  "Transient revset readers should pass native jj completion context."
  (let (current-prefix-arg seen-default seen-completion-args seen-initial-input)
    (majutsu-diff-test--with-transient-context
        'majutsu-restore 'majutsu-restore:--changes-in
      (cl-letf (((symbol-function 'majutsu-read-optional-revset)
                 (lambda (_prompt default initial-input _history completion-args)
                   (setq seen-default default
                         seen-completion-args completion-args
                         seen-initial-input initial-input)
                   "main")))
        (should (equal (majutsu-transient-read-revset "Changes in: " "old" nil)
                       "main"))
        (should (null seen-default))
        (should (equal seen-completion-args '("restore" "--changes-in")))
        (should (equal seen-initial-input "old"))))))

(ert-deftest majutsu-diff-transient-read-revset/uses-expression-reader-for-revsets ()
  "Revset expression infixes should keep using the expression reader."
  (dolist (case '((majutsu-diff majutsu-diff:-r ("diff" "--revisions"))
                  (majutsu-simplify-parents-transient
                   majutsu-simplify-parents:--source
                   ("simplify-parents" "--source"))
                  (majutsu-rebase
                   majutsu-rebase:--branch
                   ("rebase" "--branch"))))
    (pcase-let ((`(,prefix ,suffix ,expected-completion-args) case)
                (current-prefix-arg nil)
                (seen nil))
      (majutsu-diff-test--with-transient-context prefix suffix
        (cl-letf (((symbol-function 'majutsu-read-optional-revset)
                   (lambda (_prompt default initial-input _history completion-args)
                     (setq seen (list default initial-input completion-args))
                     "main | dev"))
                  ((symbol-function 'majutsu-read-optional-single-revset)
                   (lambda (&rest _args)
                     (ert-fail "Should not use single-revision reader for transient revsets"))))
          (should (equal (majutsu-transient-read-revset "Revset: " "old" nil)
                         "main | dev"))
          (should (equal seen (list nil "old" expected-completion-args))))))))

(ert-deftest majutsu-diff-transient-read-revset/empty-input-clears ()
  "Empty transient revset input should not fall back to context revision."
  (let (current-prefix-arg)
    (majutsu-diff-test--with-transient-context
        'majutsu-diff 'majutsu-diff:-r
      (cl-letf (((symbol-function 'majutsu-read-optional-revset) #'ignore))
        (should-not (majutsu-transient-read-revset "Revset: " nil nil))))))

(ert-deftest majutsu-diff-repo-default-action/is-available ()
  "The diff transient should expose generic repository-local defaults."
  (let ((suffix (transient-get-suffix 'majutsu-diff "W")))
    (should suffix)
    (should (eq (plist-get (cdr suffix) :command)
                'majutsu-transient-save-repository-defaults))))

(ert-deftest majutsu-diff-selection-actions/use-session-buffer-advice ()
  "Point- or repository-sensitive diff actions should use the source buffer."
  (dolist (key '("d" "W" "g"))
    (let* ((suffix (transient-get-suffix 'majutsu-diff key))
           (command (plist-get (cdr suffix) :command))
           (prototype (get command 'transient--suffix)))
      (should suffix)
      (should (eq (oref prototype advice*)
                  #'majutsu--transient-with-selection-buffer)))))

(ert-deftest majutsu-diff--r-argument/uses-native-revset-reader ()
  "The `-r' diff infix should use the native revset reader."
  (cl-letf (((symbol-function 'majutsu-repository-config-id) #'ignore))
    (dolist (prefix '(majutsu-diff majutsu-ediff))
      (let ((obj (seq-find (lambda (suffix)
                             (equal (oref suffix key) "-r"))
                           (transient-suffixes prefix))))
        (should obj)
        (should (eq (oref obj reader) #'majutsu-transient-read-revset))))))

(ert-deftest majutsu-diff-prefix-init/does-not-double-revisions-prefix ()
  "Initializing diff from buffer range should keep one --revisions= prefix."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-args '("--git" "--stat"))
    (setq-local majutsu-buffer-diff-range '("--revisions=abc123"))
    (cl-letf (((symbol-function 'majutsu-repository-config-id) #'ignore))
      (let* ((transient--prefix (transient--init-prefix 'majutsu-diff))
             (transient--suffixes (transient--flatten-suffixes
                                   (transient--init-suffixes 'majutsu-diff))))
        (let ((value (transient-get-value)))
          (should (member "--revisions=abc123" value))
          (should-not (member "--revisions=--revisions=abc123" value)))))))

(ert-deftest majutsu-diff-repeat-revset-reader/splits-comma-values ()
  "Custom repeat revset readers should preserve CRM comma splitting."
  (let ((obj (seq-find (lambda (suffix)
                         (equal (oref suffix key) "-r"))
                       (transient-suffixes 'majutsu-diff))))
    (should (equal (cl-letf (((symbol-function 'majutsu-read-optional-revset)
                              (lambda (&rest _args) "a, b"))
                             ((symbol-function 'transient--show) #'ignore))
                     (majutsu-diff-test--with-transient-context
                         'majutsu-diff 'majutsu-diff:-r
                       (transient-infix-read obj)))
                   '("a" "b")))))

(ert-deftest majutsu-diff-revset-falls-back-to-default-args-when-nil ()
  "`majutsu-diff-revset' should use default formatting args when ARGS is nil."
  (let (captured-args
        captured-range)
    (cl-letf (((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (_mode _locked bindings &rest _kwargs)
                 (setq captured-args (cadr (assq 'majutsu-buffer-diff-args bindings))
                       captured-range (cadr (assq 'majutsu-buffer-diff-range bindings)))
                 (let ((buffer (generate-new-buffer " *majutsu diff revset*")))
                   (with-current-buffer buffer
                     (setq-local majutsu-buffer-diff-args captured-args))
                   buffer))))
      (let ((buffer (majutsu-diff-revset "abc123")))
        (unwind-protect
            (progn
              (should (equal captured-args '("--git" "--stat")))
              (should (equal captured-range '("--revisions=abc123"))))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest majutsu-diff-dwim-uses-transient-args-when-active ()
  "When called from the transient, DWIM should use current transient args."
  (let ((transient-current-command 'majutsu-diff)
        (majutsu-direct-use-buffer-arguments 'never)
        called-args
        called-files
        called-range)
    (cl-letf (((symbol-function 'majutsu-diff-setup-buffer)
               (lambda (args range filesets &rest _)
                 (setq called-args args
                       called-files filesets
                       called-range range)))
              ((symbol-function 'majutsu-diff--dwim)
               (lambda () '(commit . "abc123")))
              ((symbol-function 'transient-args)
               (lambda (&rest _) (list '("--context=9" "--stat") nil nil))))
      (call-interactively #'majutsu-diff-dwim)
      (should (equal called-args '("--context=9" "--stat")))
      (should (equal called-files nil))
      (should (equal called-range '("--revisions=abc123"))))))

(ert-deftest majutsu-diff-dwim-inherits-current-buffer-range ()
  "DWIM should reuse range and filesets from the current diff buffer."
  (let ((majutsu-direct-use-buffer-arguments 'current)
        called-args
        called-files
        called-range)
    (with-temp-buffer
      (majutsu-diff-mode)
      (setq-local majutsu-buffer-diff-args '("--stat"))
      (setq-local majutsu-buffer-diff-range '("--from=main" "--to=dev"))
      (setq-local majutsu-buffer-diff-filesets '("foo" "bar"))
      (cl-letf (((symbol-function 'majutsu-diff-setup-buffer)
                 (lambda (args range filesets &rest _)
                   (setq called-args args
                         called-files filesets
                         called-range range)))
                ((symbol-function 'majutsu-diff--dwim)
                 (lambda () '(commit . "abc123"))))
        (call-interactively #'majutsu-diff-dwim)
        (should (equal called-args '("--stat")))
        (should (equal called-range '("--from=main" "--to=dev")))
        (should (equal called-files '("foo" "bar")))))))

(ert-deftest majutsu-diff-dwim-does-not-inherit-range-from-other-buffer ()
  "DWIM should not reuse range or filesets from other diff buffers."
  (let ((majutsu-direct-use-buffer-arguments 'always)
        called-files
        called-range)
    (let ((diff-buf (generate-new-buffer " *majutsu diff test*")))
      (unwind-protect
          (progn
            (with-current-buffer diff-buf
              (majutsu-diff-mode)
              (setq-local majutsu-buffer-diff-args '("--stat"))
              (setq-local majutsu-buffer-diff-range '("--from=main" "--to=dev"))
              (setq-local majutsu-buffer-diff-filesets '("foo" "bar")))
            (with-temp-buffer
              (cl-letf (((symbol-function 'majutsu-diff-setup-buffer)
                         (lambda (_args range filesets &rest _)
                           (setq called-range range
                                 called-files filesets)))
                        ((symbol-function 'majutsu-diff--dwim)
                         (lambda () '(commit . "abc123"))))
                (call-interactively #'majutsu-diff-dwim)
                (should (equal called-range '("--revisions=abc123")))
                (should (equal called-files nil)))))
        (when (buffer-live-p diff-buf)
          (kill-buffer diff-buf))))))

(ert-deftest majutsu-diff-dwim/prefers-literal-revision-at-point ()
  (cl-letf (((symbol-function 'majutsu-thing-at-point)
             (lambda (_thing &optional _no-properties)
               "main@origin"))
            ((symbol-function 'majutsu-revision-at-point)
             (lambda () "context")))
    (should (equal (majutsu-diff--dwim)
                   '(revision . "main@origin")))))

(ert-deftest majutsu-diff-refine-hunk-default-disabled ()
  "Diff refinement should be disabled by default."
  (with-temp-buffer
    (majutsu-diff-mode)
    (should-not majutsu-diff-refine-hunk)))

(ert-deftest majutsu-diff-toggle-refine-hunk-updates ()
  "Toggling refinement should update local state and trigger refresh."
  (with-temp-buffer
    (majutsu-diff-mode)
    (let ((calls 0))
      (cl-letf (((symbol-function 'majutsu-diff--update-hunk-refinement)
                 (lambda (&rest _)
                   (setq calls (1+ calls)))))
        (should-not majutsu-diff-refine-hunk)
        (majutsu-diff-toggle-refine-hunk nil)
        (should (eq majutsu-diff-refine-hunk t))
        (should (= calls 1))
        (majutsu-diff-toggle-refine-hunk t)
        (should (eq majutsu-diff-refine-hunk 'all))
        (should (= calls 2))
        (majutsu-diff-toggle-refine-hunk t)
        (should (eq majutsu-diff-refine-hunk t))
        (should (= calls 3))
        (majutsu-diff-toggle-refine-hunk nil)
        (should-not majutsu-diff-refine-hunk)
        (should (= calls 4))))))

(ert-deftest majutsu-diff-refinement/majutsu-owns-git-hunks ()
  "Git font-lock must not refine hunks independently of Majutsu.
This is a regression test for issue #42."
  (majutsu-diff-test--with-washed-git-hunks
   (let ((hunks (majutsu-diff-test--hunk-sections)))
     (should (= (length hunks) 2))
     (should (local-variable-p 'diff-refine))
     (should-not diff-refine)
     (should (eq (default-value 'diff-refine) 'font-lock))
     (should-not majutsu-diff-refine-hunk)

     ;; The complete diff-mode keyword set is active for Git coloring, but it
     ;; must not create its own refinement overlays or bookkeeping marker.
     (font-lock-ensure (point-min) (point-max))
     (should-not (majutsu-diff-test--fine-overlays))
     (should-not (majutsu-diff-test--font-lock-refine-markers))

     ;; Majutsu's `t' style refines only the current hunk.
     (let ((current (car hunks))
           (other (cadr hunks)))
       (goto-char (oref current content))
       (majutsu-diff-toggle-refine-hunk)
       (should (eq majutsu-diff-refine-hunk t))
       (should (oref current refined))
       (should-not (oref other refined))
       (should (majutsu-diff-test--fine-overlays current))
       (should-not (majutsu-diff-test--fine-overlays other))
       (should-not (majutsu-diff-test--font-lock-refine-markers))

       ;; Toggling off removes Majutsu's overlays.  A complete re-fontify must
       ;; not resurrect upstream automatic refinement.
       (majutsu-diff-toggle-refine-hunk)
       (should-not majutsu-diff-refine-hunk)
       (should-not (oref current refined))
       (should-not (majutsu-diff-test--fine-overlays))
       (font-lock-flush (point-min) (point-max))
       (font-lock-ensure (point-min) (point-max))
       (should-not (majutsu-diff-test--fine-overlays))
       (should-not (majutsu-diff-test--font-lock-refine-markers))))))

(ert-deftest majutsu-diff-refinement/cleans-up-hidden-hunk ()
  "Disabling refinement cleans overlays from a hidden hunk."
  (majutsu-diff-test--with-washed-git-hunks
   (font-lock-ensure (point-min) (point-max))
   (let ((hunk (car (majutsu-diff-test--hunk-sections))))
     (goto-char (oref hunk content))
     (majutsu-diff-toggle-refine-hunk)
     (should (oref hunk refined))
     (should (majutsu-diff-test--fine-overlays hunk))

     (magit-section-hide hunk)
     (should (oref hunk hidden))
     (majutsu-diff-toggle-refine-hunk)
     (should-not majutsu-diff-refine-hunk)
     (should-not (oref hunk refined))
     (should-not (majutsu-diff-test--fine-overlays hunk))

     (font-lock-flush (point-min) (point-max))
     (font-lock-ensure (point-min) (point-max))
     (should-not (majutsu-diff-test--fine-overlays))
     (should-not (majutsu-diff-test--font-lock-refine-markers)))))

(ert-deftest majutsu-diff-color-words-goto-from-uses-removed-block ()
  "For shared color-words lines, removed block should target old side."
  (cl-letf (((symbol-function 'majutsu-color-words-side-at-point)
             (lambda (&optional _pos) 'removed)))
    (should (majutsu-diff--color-words-goto-from '(:from-line 10 :to-line 12)))))

(ert-deftest majutsu-diff-color-words-goto-from-uses-added-block ()
  "For shared color-words lines, added block should target new side."
  (cl-letf (((symbol-function 'majutsu-color-words-side-at-point)
             (lambda (&optional _pos) 'added)))
    (should-not (majutsu-diff--color-words-goto-from '(:from-line 10 :to-line 12)))))

(ert-deftest majutsu-diff-color-words-goto-from-falls-back-to-line-shape ()
  "When side cannot be inferred, keep line-shape fallback behavior."
  (cl-letf (((symbol-function 'majutsu-color-words-side-at-point)
             (lambda (&optional _pos) nil)))
    (should (majutsu-diff--color-words-goto-from '(:from-line 10)))
    (should-not (majutsu-diff--color-words-goto-from '(:from-line 10 :to-line 12)))))

(ert-deftest majutsu-diff-color-words-column-uses-side-aware-helper ()
  "Color-words column helper should receive side selection."
  (let ((info '(:content-column 4))
        called)
    (cl-letf (((symbol-function 'majutsu-color-words-column-at-point)
               (lambda (goto-from &optional _pos _info)
                 (setq called goto-from)
                 17)))
      (should (= (majutsu-diff--color-words-column info t) 17))
      (should called)
      (should (= (majutsu-diff--color-words-column info nil) 17))
      (should-not called))))

(provide 'majutsu-diff-test)

;;; majutsu-diff-test.el ends here
