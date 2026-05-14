;;; majutsu-bookmark-test.el --- Tests for bookmark helpers  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for bookmark parsing and transient argument behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'majutsu-bookmark)

(defun majutsu-bookmark-test--sections (&optional section)
  "Return SECTION and all descendants, defaulting to `magit-root-section'."
  (let ((section (or section magit-root-section)))
    (cons section
          (apply #'append
                 (mapcar #'majutsu-bookmark-test--sections
                         (oref section children))))))

(cl-defun majutsu-bookmark-test--row
    (heading role name remote tracked commit-id &optional children
             (newline t newline-supplied-p))
  "Return one raw bookmark-list row record.
CHILDREN, when non-nil, is inserted as an explicit child stream before
metadata.  When NEWLINE is non-nil or omitted, append a trailing newline."
  (concat majutsu-row-start-token
          majutsu-row-role-token
          (symbol-name role)
          majutsu-row-role-token
          heading
          majutsu-row-tail-token
          majutsu-row-body-token
          (and children majutsu-row-children-token)
          (or children "")
          majutsu-row-meta-token
          (string-join (list name (or remote "")
                             (if tracked "t" "") (or commit-id ""))
                       majutsu-row-field-separator)
          majutsu-row-end-token
          (if (or (not newline-supplied-p) newline) "\n" "")))

(defun majutsu-bookmark-test--inline-child-stream (&rest rows)
  "Return a transport child stream containing ROWS."
  (concat majutsu-row-push-token
          (apply #'concat rows)
          majutsu-row-pop-token))

(defun majutsu-bookmark-test--ref (name remote tracked heading &optional children newline)
  "Return one bookmark-list ref row record."
  (majutsu-bookmark-test--row
   heading 'bookmark name remote tracked nil children newline))

(defun majutsu-bookmark-test--target (name remote _marker commit-id line &optional children newline)
  "Return one bookmark-list conflict target row record."
  (majutsu-bookmark-test--row
   line 'bookmark-target name remote nil commit-id children newline))

(ert-deftest majutsu-bookmark-split-remote-ref/basic ()
  (should (equal (majutsu--bookmark-split-remote-ref "main@origin")
                 '("main" . "origin"))))

(ert-deftest majutsu-bookmark-split-remote-ref/last-at ()
  (should (equal (majutsu--bookmark-split-remote-ref "foo@bar@origin")
                 '("foo@bar" . "origin"))))

(ert-deftest majutsu-bookmark-split-remote-ref/no-remote ()
  (should (equal (majutsu--bookmark-split-remote-ref "main")
                 '("main" . nil))))

(ert-deftest majutsu-bookmark-section-map/remaps-visit-thing-to-edit ()
  (should (eq (lookup-key majutsu-bookmark-section-map
                          [remap majutsu-visit-thing])
              #'majutsu-edit-changeset)))

(ert-deftest majutsu-bookmark-get-bookmark-names/local-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("main" "feature"))))
      (should (equal (majutsu--get-bookmark-names 'local) '("main" "feature")))
      (should (equal (seq-take seen-args 3) '("bookmark" "list" "--quiet")))
      (should-not (member "--all-remotes" seen-args))
      (should-not (member "--tracked" seen-args))
      (should (member "-T" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "self.primary().name()") template))
        (should (string-match-p (regexp-quote "(!self.primary().remote())") template))
        (should (string-match-p (regexp-quote "self.primary().present()") template))
        (should (string-match-p (regexp-quote "\\n") template))))))

(ert-deftest majutsu-bookmark-get-bookmark-names/remote-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("main@origin" "dev@upstream"))))
      (should (equal (majutsu--get-bookmark-names 'remote) '("main@origin" "dev@upstream")))
      (should (member "--all-remotes" seen-args))
      (should (member "-T" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "self.primary().remote()") template))
        (should (string-match-p (regexp-quote "self.tracked_refs().map(|ref|") template))
        (should (string-match-p (regexp-quote "ref.present()") template))
        (should (string-match-p (regexp-quote "ref.remote()") template))
        (should (string-match-p (regexp-quote "\"@\"") template))))))

(ert-deftest majutsu-bookmark-get-bookmark-names/remote-tracked-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("main@origin"))))
      (should (equal (majutsu--get-bookmark-names 'remote-tracked) '("main@origin")))
      (should (member "--tracked" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "self.tracked_refs().map(|ref|") template))
        (should (string-match-p (regexp-quote "ref.present()") template))
        (should (string-match-p (regexp-quote "ref.remote()") template))
        (should-not (string-match-p (regexp-quote "self.primary().name()") template))))))

(ert-deftest majutsu-bookmark-get-bookmark-names/remote-untracked-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("topic@origin"))))
      (should (equal (majutsu--get-bookmark-names 'remote-untracked) '("topic@origin")))
      (should (member "--all-remotes" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "self.primary().remote()") template))
        (should (string-match-p (regexp-quote "self.primary().present()") template))
        (should-not (string-match-p (regexp-quote "self.tracked_refs().map(|ref|") template))))))

(ert-deftest majutsu-bookmark-forget-name-candidates/include-local-and-remote-bases ()
  (cl-letf (((symbol-function 'majutsu--get-bookmark-names)
             (lambda (&optional scope)
               (pcase scope
                 ('local '("main" "local-only"))
                 ('remote '("main@origin" "remote-only@upstream"))
                 (_ nil)))))
    (should (equal (majutsu--bookmark-forget-name-candidates)
                   '("main" "local-only" "remote-only")))))

(ert-deftest majutsu-bookmark-track-name-candidates/uses-untracked-remote-bases ()
  (cl-letf (((symbol-function 'majutsu--get-bookmark-names)
             (lambda (&optional scope)
               (when (eq scope 'remote-untracked)
                 '("main@origin" "topic@upstream" "main@fork")))))
    (should (equal (majutsu--bookmark-track-name-candidates)
                   '("main" "topic")))))

(ert-deftest majutsu-bookmark-untrack-name-candidates/uses-tracked-remote-bases ()
  (cl-letf (((symbol-function 'majutsu--get-bookmark-names)
             (lambda (&optional scope)
               (when (eq scope 'remote-tracked)
                 '("main@origin" "topic@upstream")))))
    (should (equal (majutsu--bookmark-untrack-name-candidates)
                   '("main" "topic")))))

(ert-deftest majutsu-bookmark-completion-template/emits-line-delimiters ()
  (should (string-match-p (regexp-quote "\\n") majutsu-bookmark--completion-template)))

(ert-deftest majutsu-bookmark-list-template/uses-typed-commitref-fields ()
  (let ((template (majutsu-bookmark--list-template)))
    (should-not (string-match-p "format_commit_ref" template))
    (should-not (string-match-p "format_commit_summary_with_refs" template))
    (should-not (string-match-p ":map-join" template))
    (should (string-match-p (regexp-quote "self.primary().name()") template))
    (should (string-match-p (regexp-quote "self.primary().tracked()") template))
    (should (string-match-p (regexp-quote "self.primary().removed_targets().map(|target|") template))
    (should (string-match-p (regexp-quote "self.primary().added_targets().map(|target|") template))
    (should (string-match-p (regexp-quote "self.tracked_refs().map(|ref|") template))
    (should (string-match-p (regexp-quote "ref.removed_targets().map(|target|") template))
    (should (string-match-p (regexp-quote "ref.added_targets().map(|target|") template))
    (should-not (string-match-p (regexp-quote "self.name()") template))
    (should-not (string-match-p (regexp-quote "self.remote()") template))
    (should-not (string-match-p (regexp-quote "commit.name()") template))
    (should-not (string-match-p (regexp-quote "commit.remote()") template))
    (should (string-match-p (regexp-quote ".join(\"\")") template))
    (should (string-match-p (regexp-quote "change_id().shortest(8)") template))
    (should (string-match-p (regexp-quote "commit_id().shortest(8)") template))
    (should (string-match-p (regexp-quote "description().first_line()") template))
    (should (string-match-p (regexp-quote "label(\"bookmark\"") template))
    (should (string-match-p (regexp-quote "label(\"conflict\"") template))
    (should (string-match-p (regexp-quote "bookmark-target") template))
    (should (string-match-p (regexp-quote "\\x1E") template))
    (should (string-match-p (regexp-quote "\\x1D") template))))

(ert-deftest majutsu-bookmark-list-template/uses-custom-display-templates ()
  (let ((majutsu-bookmark--compiled-template-cache nil)
        (majutsu-bookmark-list-template-group-heading
         '[:separate " :: " [:primary :majutsu-bookmark-list-name] "CUSTOM-HEADING"])
        (majutsu-bookmark-list-template-commit-summary
         '["CUSTOM-SUMMARY " [:commit_id :shortest 4]]))
    (let ((template (majutsu-bookmark--list-template)))
      (should (string-match-p (regexp-quote "CUSTOM-HEADING") template))
      (should (string-match-p (regexp-quote "CUSTOM-SUMMARY ") template))
      (should (string-match-p (regexp-quote "commit_id().shortest(4)") template)))))

(ert-deftest majutsu-bookmark-list-template/uses-target-heading-columns ()
  (let ((majutsu-bookmark--compiled-template-cache nil)
        (majutsu-bookmark-list-template-commit-summary
         '["TARGET " [:commit_id :shortest 4]]))
    (let* ((compiled (majutsu-bookmark--ensure-list-template))
           (template (plist-get compiled :template)))
      (should-not (string-match-p (regexp-quote ":adopt-previous") template))
      (should (equal (mapcar (lambda (column) (plist-get column :field))
                             (majutsu-row-module-columns compiled 'heading))
                     '(heading target-marker target-summary)))
      (should (string-match-p (regexp-quote "  -") template))
      (should (string-match-p (regexp-quote "  +") template))
      (should (string-match-p (regexp-quote "TARGET ") template))
      (should (string-match-p (regexp-quote "commit_id().shortest(4)") template)))))

(ert-deftest majutsu-bookmark-row-output/attaches-targets-and-remote-state ()
  (let* ((compiled (majutsu-bookmark--ensure-list-template))
         (output
          (majutsu-bookmark-test--ref
           "dev" "" nil "dev: rymwrkkn b052a92d summary"
           (majutsu-bookmark-test--inline-child-stream
            (majutsu-bookmark-test--ref
             "dev" "origin" t
             "  @origin (ahead by 2 commits, behind by at least 10 commits): sxwtxlqt/1 4bb5dd3b (hidden) summary"
             (majutsu-bookmark-test--inline-child-stream
              (majutsu-bookmark-test--target
               "dev" "origin" "+" "4bb5dd3b"
               "  + sxwtxlqt/1 4bb5dd3b (hidden) summary"
               nil nil))
             nil))))
         parsed roots dev origin target)
    (with-temp-buffer
      (insert output)
      (setq parsed (majutsu-row-read-buffer compiled)))
    (setq roots (plist-get parsed :roots)
          dev (car roots)
          origin (car (plist-get dev :children))
          target (car (plist-get origin :children)))
    (should (= (length roots) 1))
    (should (eq (majutsu-row-role dev) 'bookmark))
    (should (eq (majutsu-row-role target) 'bookmark-target))
    (should (equal (majutsu-row-column dev 'name) "dev"))
    (should (equal (majutsu-row-column origin 'remote) "origin"))
    (should (majutsu-row-column origin 'tracked))
    (should (equal (majutsu-row-entry-id target compiled)
                   "dev@origin#4bb5dd3b"))
    (should (equal (majutsu-row-column target 'commit-id) "4bb5dd3b"))
    (should (equal (majutsu-row-column target 'heading)
                   "  + sxwtxlqt/1 4bb5dd3b (hidden) summary"))))

(ert-deftest majutsu-bookmark-wash-list/nests-tracked-remotes-under-local-bookmark ()
  (let* ((output
          (majutsu-bookmark-test--ref
           "dev" "" nil "dev: rymwrkkn b052a92d summary"
           (majutsu-bookmark-test--inline-child-stream
            (majutsu-bookmark-test--ref
             "dev" "origin" t
             "  @origin (ahead by 2 commits): sxwtxlqt/1 4bb5dd3b (hidden) summary"
             nil nil)))))
    (with-temp-buffer
      (majutsu-bookmark-list-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (bookmark-list)
          (insert output)
          (majutsu-bookmark--wash-list nil))
        (let* ((sections (majutsu-bookmark-test--sections))
               (dev (seq-find (lambda (section)
                                (and (eq (oref section type) 'jj-bookmark)
                                     (equal (oref section value) "dev")))
                              sections))
               origin)
          (should dev)
          (magit-section-show dev)
          (setq origin
                (seq-find (lambda (section)
                            (and (eq (oref section type) 'jj-bookmark)
                                 (equal (oref section value) "dev@origin")))
                          (oref dev children)))
          (should origin)
          (should-not (seq-find (lambda (section)
                                  (and (eq (oref section type) 'jj-bookmark)
                                       (equal (oref section value) "@origin")))
                                sections))
          (should (string-match-p "^dev:" (buffer-string)))
          (should (string-match-p "^  @origin" (buffer-string))))))))

(ert-deftest majutsu-bookmark-wash-list/sectionizes-conflicted-targets ()
  (let* ((output
          (majutsu-bookmark-test--ref
           "topic" "" nil "topic (conflicted):"
           (majutsu-bookmark-test--inline-child-stream
            (majutsu-bookmark-test--target
             "topic" "" "-" "old123" "  - old-target"
             nil nil)
            (majutsu-bookmark-test--target
             "topic" "" "+" "new123" "  + new-target"
             nil nil)))))
    (with-temp-buffer
      (majutsu-bookmark-list-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (bookmark-list)
          (insert output)
          (majutsu-bookmark--wash-list nil))
        (let* ((sections (majutsu-bookmark-test--sections))
               (bookmark-sections
                (seq-filter (lambda (section)
                              (eq (oref section type) 'jj-bookmark))
                            sections))
               (commit-sections
                (seq-filter (lambda (section)
                              (eq (oref section type) 'jj-commit))
                            sections)))
          (should (= (length bookmark-sections) 1))
          (should (equal (oref (car bookmark-sections) value) "topic"))
          (magit-section-show (car bookmark-sections))
          (setq sections (majutsu-bookmark-test--sections)
                commit-sections
                (seq-filter (lambda (section)
                              (eq (oref section type) 'jj-commit))
                            sections))
          (should (= (length commit-sections) 2))
          (should (equal (mapcar (lambda (section) (oref section value))
                                 commit-sections)
                         '("old123" "new123")))
          (should (string-match-p (regexp-quote "topic (conflicted):")
                                  (buffer-string)))
          (should-not (string-match-p (regexp-quote "topic (conflicted) (2)")
                                      (buffer-string)))
          (should (string-match-p (regexp-quote "  - old-target") (buffer-string)))
          (should (string-match-p (regexp-quote "  + new-target") (buffer-string)))
          (save-excursion
            (goto-char (point-min))
            (search-forward "  - old-target")
            (should-not (memq 'diff-removed
                              (ensure-list (get-text-property (+ (match-beginning 0) 2)
                                                              'font-lock-face))))
            (goto-char (point-min))
            (search-forward "  + new-target")
            (should-not (memq 'diff-added
                              (ensure-list (get-text-property (+ (match-beginning 0) 2)
                                                              'font-lock-face))))))))))

(ert-deftest majutsu-bookmark-list-refresh-buffer/uses-structured-template ()
  (let (seen-args)
    (with-temp-buffer
      (majutsu-bookmark-list-mode)
      (cl-letf (((symbol-function 'majutsu-jj-wash)
                 (lambda (_washer _keep &rest args)
                   (setq seen-args (flatten-list args)))))
        (majutsu-bookmark-list-refresh-buffer))
      (should (member "-T" seen-args))
      (should (equal (cadr (member "-T" seen-args))
                     (majutsu-bookmark--list-template))))))

(ert-deftest majutsu-bookmark-candidate-data/aggregates-local-and-remote-state ()
  (let ((sep majutsu-bookmark--completion-field-separator))
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest _args)
                 (list (concat "main" sep "" sep "" sep "t" sep "" sep "t")
                       (concat "main" sep "origin" sep "" sep "t" sep "t" sep "t")
                       (concat "feature" sep "fork" sep "t" sep "t" sep "" sep "")))))
      (let* ((payload (majutsu-bookmark-candidate-data '("main" "feature")))
             (entries (plist-get payload :entries))
             (main (gethash "main" entries))
             (feature (gethash "feature" entries)))
        (should (equal (plist-get payload :candidates) '("main" "feature")))
        (should (plist-get main :local))
        (should (plist-get main :synced))
        (should (equal (plist-get main :tracked-remotes) '("origin")))
        (should (plist-get feature :conflict))
        (should (equal (plist-get feature :untracked-remotes) '("fork")))))))

(ert-deftest majutsu-bookmark-advance/builds-default-args ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0)))
      (majutsu-bookmark-advance)
      (should (equal (seq-remove #'null (flatten-tree called))
                     '("bookmark" "advance"))))))

(ert-deftest majutsu-bookmark-advance-to/builds-target-args ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0)))
      (majutsu-bookmark-advance-to "@")
      (should (equal (seq-remove #'null (flatten-tree called))
                     '("bookmark" "advance" "-t" "@"))))))

(ert-deftest majutsu-read-bookmark-name/uses-name-history ()
  (let (seen-history seen-category prewarm)
    (cl-letf (((symbol-function 'majutsu-bookmark-candidate-data)
               (lambda (&optional _candidates _directory)
                 (list :candidates '("main" "feature")
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'majutsu-marginalia-prewarm-candidate-data)
               (lambda (&rest args)
                 (setq prewarm args)))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection _predicate _require-match _initial hist _default)
                 (setq seen-history hist)
                 (let ((metadata (funcall collection "" nil 'metadata)))
                   (setq seen-category (cdr (assq 'category (cdr metadata)))))
                 "main")))
      (should (equal (majutsu-read-bookmark-name "Bookmark") "main"))
      (should (eq seen-history 'majutsu-bookmark-name-history))
      (should (eq seen-category 'majutsu-bookmark))
      (should (eq (car prewarm) 'majutsu-bookmark)))))

(ert-deftest majutsu-bookmark-at-point/uses-exact-bookmark ()
  (cl-letf (((symbol-function 'magit-section-value-if)
             (lambda (type)
               (and (eq type 'jj-bookmark) "main"))))
    (should (equal (majutsu-bookmark-at-point) "main"))))

(ert-deftest majutsu-read-bookmark-name/defaults-to-exact-bookmark-at-point ()
  (let (seen-default)
    (cl-letf (((symbol-function 'majutsu-bookmark-at-point)
               (lambda (&optional _bookmark-type) "main"))
              ((symbol-function 'majutsu-bookmark-candidate-data)
               (lambda (&optional _candidates _directory)
                 (list :candidates '("main")
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'majutsu-marginalia-prewarm-candidate-data)
               (lambda (&rest _args)))
              ((symbol-function 'completing-read)
               (lambda (_prompt _collection _predicate _require-match _initial _hist default)
                 (setq seen-default default)
                 "main")))
      (should (equal (majutsu-read-bookmark-name "Bookmark") "main"))
      (should (equal seen-default "main")))))

(ert-deftest majutsu-read-bookmark-patterns/falls-back-to-revision-bookmark-patterns ()
  (let (seen-default)
    (cl-letf (((symbol-function 'majutsu-bookmark-at-point)
               (lambda (&optional _bookmark-type) nil))
              ((symbol-function 'majutsu--bookmark-patterns-for-revision-at-point)
               (lambda (&optional _bookmark-type) "main,feature"))
              ((symbol-function 'majutsu-bookmark-candidate-data)
               (lambda (&optional _candidates _directory)
                 (list :candidates '("main" "feature")
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'majutsu-marginalia-prewarm-candidate-data)
               (lambda (&rest _args)))
              ((symbol-function 'completing-read-multiple)
               (lambda (_prompt _collection _predicate _require-match _initial _hist default)
                 (setq seen-default default)
                 '("main"))))
      (should (equal (majutsu-read-bookmark-patterns "Bookmarks") '("main")))
      (should (equal seen-default "main,feature")))))

(ert-deftest majutsu-read-bookmark-patterns/filters-empty-input-and-uses-history ()
  (let (seen-history seen-category prewarm)
    (cl-letf (((symbol-function 'majutsu-bookmark-candidate-data)
               (lambda (&optional _candidates _directory)
                 (list :candidates '("main" "feature")
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'majutsu-marginalia-prewarm-candidate-data)
               (lambda (&rest args)
                 (setq prewarm args)))
              ((symbol-function 'majutsu-bookmark-at-point)
               (lambda () "main"))
              ((symbol-function 'completing-read-multiple)
               (lambda (_prompt collection _predicate _require-match _initial hist _default)
                 (setq seen-history hist)
                 (let ((metadata (funcall collection "" nil 'metadata)))
                   (setq seen-category (cdr (assq 'category (cdr metadata)))))
                 '("main" "glob:\"feat*\""))))
      (should (equal (majutsu-read-bookmark-patterns "Bookmarks")
                     '("main" "glob:\"feat*\"")))
      (should (eq seen-history 'majutsu-bookmark-pattern-history))
      (should (eq seen-category 'majutsu-bookmark))
      (should (eq (car prewarm) 'majutsu-bookmark)))))

(ert-deftest majutsu-bookmark-create/builds-multiple-exact-name-args ()
  (let (called)
    (cl-letf (((symbol-function 'magit-section-value-if)
               (lambda (_type) "@"))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0)))
      (majutsu-bookmark-create '("main" "feature"))
      (should (equal (seq-remove #'null (flatten-tree called))
                     '("bookmark" "create" "main" "feature" "-r" "@"))))))

(ert-deftest majutsu-bookmark-advance-patterns/builds-name-args ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0)))
      (majutsu-bookmark-advance-patterns '("main" "glob:\"feat*\""))
      (should (equal (seq-remove #'null (flatten-tree called))
                     '("bookmark" "advance"
                       "main" "glob:\"feat*\""))))))

(ert-deftest majutsu-bookmark-set/builds-multiple-exact-name-args ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0)))
      (majutsu-bookmark-set '("main" "feature") "@")
      (should (equal (seq-remove #'null (flatten-tree called))
                     '("bookmark" "set" "main" "feature" "-r" "@"))))))

(ert-deftest majutsu-bookmark-track/reads-patterns-and-calls-jj ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-remote-candidate-data)
               (lambda (&optional _directory)
                 (list :candidates '("origin" "upstream")
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'completing-read-multiple)
               (lambda (prompt _collection &rest _args)
                 (cond
                  ((string-match-p "Track bookmark name" prompt)
                   '("main" "glob:\"feat*\""))
                  ((string-match-p "Remote.*pattern" prompt)
                   '("origin" "upstream"))
                  (t nil))))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (majutsu-bookmark-track)
      (should (equal (seq-remove #'null (flatten-tree called))
                     '("bookmark" "track"
                       "main" "glob:\"feat*\""
                       "--remote" "origin"
                       "--remote" "upstream"))))))

(ert-deftest majutsu-bookmark-delete/builds-pattern-args ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (majutsu-bookmark-delete '("main" "glob:\"feat*\""))
      (should (equal (seq-remove #'null (flatten-tree called))
                     '("bookmark" "delete"
                       "main" "glob:\"feat*\""))))))

(ert-deftest majutsu-bookmark-forget/builds-pattern-args ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (majutsu-bookmark-forget '("main" "glob:\"feat*\""))
      (should (equal (seq-remove #'null (flatten-tree called))
                     '("bookmark" "forget"
                       "main" "glob:\"feat*\""))))))

(ert-deftest majutsu-bookmark-untrack/builds-remote-args ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (majutsu-bookmark-untrack '("main" "glob:\"ci/*\"") '("origin" "upstream"))
      (should (equal (seq-remove #'null (flatten-tree called))
                     '("bookmark" "untrack"
                       "main" "glob:\"ci/*\""
                       "--remote" "origin"
                       "--remote" "upstream"))))))

;;; majutsu-bookmark-test.el ends here
