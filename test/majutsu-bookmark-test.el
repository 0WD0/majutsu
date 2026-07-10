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
    (heading name remote tracked conflict-details removed-ids added-ids &optional
             (newline t newline-supplied-p))
  "Return one raw bookmark-list row record.
When NEWLINE is non-nil or omitted, append a trailing newline."
  (concat majutsu-row-start-token
          heading
          majutsu-row-tail-token
          majutsu-row-body-token
          (replace-regexp-in-string
           "\n" majutsu-row-field-line-separator (or conflict-details "") t t)
          majutsu-row-meta-token
          (string-join (list name (or remote "")
                             (if tracked "t" "")
                             (string-join (or removed-ids nil)
                                          majutsu-row-field-line-separator)
                             (string-join (or added-ids nil)
                                          majutsu-row-field-line-separator))
                       majutsu-row-field-separator)
          majutsu-row-end-token
          (if (or (not newline-supplied-p) newline) "\n" "")))

(cl-defun majutsu-bookmark-test--ref
    (name remote tracked heading &optional conflict-details removed-ids added-ids
          (newline t newline-supplied-p))
  "Return one bookmark-list ref row record."
  (majutsu-bookmark-test--row
   heading name remote tracked conflict-details removed-ids added-ids
   (if newline-supplied-p newline t)))

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
        (should (string-match-p (regexp-quote "(!self.remote())") template))
        (should (string-match-p (regexp-quote "self.present()") template))
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
        (should (string-match-p (regexp-quote "self.remote()") template))
        (should (string-match-p (regexp-quote "self.present()") template))
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
        (should (string-match-p (regexp-quote "self.tracked()") template))))))

(ert-deftest majutsu-bookmark-get-bookmark-names/remote-untracked-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("topic@origin"))))
      (should (equal (majutsu--get-bookmark-names 'remote-untracked) '("topic@origin")))
      (should (member "--all-remotes" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "(!self.tracked())") template))))))

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
  (let ((majutsu-bookmark--compiled-template-cache nil))
    (let ((template (majutsu-bookmark--list-template)))
    (should-not (string-match-p "format_commit_ref" template))
    (should-not (string-match-p "format_commit_summary_with_refs" template))
    (should-not (string-match-p ":map-join" template))
    (should (string-match-p "self.removed_targets().map" template))
    (should (string-match-p "self.added_targets().map" template))
    (should-not (string-match-p "self.primary()" template))
    (should-not (string-match-p "self.tracked_refs()" template))
    (should (string-match-p (regexp-quote "change_id().shortest(8)") template))
    (should (string-match-p (regexp-quote "commit_id().shortest(8)") template))
    (should (string-match-p (regexp-quote "description().first_line()") template))
    (should (string-match-p (regexp-quote "label(\"bookmark\"") template))
    (should (string-match-p (regexp-quote "label(\"conflict\"") template))
    (should (string-match-p (regexp-quote "\\x1E") template))
    (should (string-match-p (regexp-quote "\\x1D") template)))))

(ert-deftest majutsu-bookmark-list-template/uses-custom-heading-template ()
  (let ((majutsu-bookmark--compiled-template-cache nil)
        (majutsu-bookmark-list-template-heading
         '[:separate " :: " [:majutsu-bookmark-list-name] "CUSTOM-HEADING"]))
    (let ((template (majutsu-bookmark--list-template)))
      (should (string-match-p (regexp-quote "CUSTOM-HEADING") template)))))

(ert-deftest majutsu-bookmark-list-template/uses-flat-row-columns ()
  (let* ((compiled (majutsu-bookmark--ensure-list-template))
         (heading-fields
          (mapcar (lambda (column) (plist-get column :field))
                  (majutsu-row-module-columns compiled 'heading)))
         (metadata-fields
          (mapcar (lambda (column) (plist-get column :field))
                  (majutsu-row-module-columns compiled 'metadata))))
    (should (equal heading-fields '(heading)))
    (should (equal metadata-fields
                   '(name remote tracked removed-target-ids
                          added-target-ids)))))

(ert-deftest majutsu-bookmark-row-output/parses-flat-refs-and-remote-state ()
  (let* ((compiled (majutsu-bookmark--ensure-list-template))
         (output (concat
                  (majutsu-bookmark-test--ref
                   "dev" "" nil "dev: rymwrkkn b052a92d summary")
                  (majutsu-bookmark-test--ref
                   "dev" "origin" t
                   "  @origin (+2/-10+): sxwtxlqt/1 4bb5dd3b (hidden) summary")))
         parsed roots dev origin)
    (with-temp-buffer
      (insert output)
      (setq parsed (majutsu-row-read-buffer compiled)))
    (setq roots (plist-get parsed :roots)
          dev (car roots)
          origin (cadr roots))
    (should (= (length roots) 2))
    (should (equal (majutsu-row-column dev 'name) "dev"))
    (should (equal (majutsu-row-column origin 'remote) "origin"))
    (should (majutsu-row-column origin 'tracked))))

(ert-deftest majutsu-bookmark-row-output/preserves-conflict-details-and-full-ids ()
  (let* ((compiled (majutsu-bookmark--ensure-list-template))
         (output (majutsu-bookmark-test--ref
                  "dev" "origin" t "@origin (conflicted):"
                  "  - old-target\n  + new-target"
                  '("0123456789abcdef" "1111111111111111")
                  '("fedcba9876543210")))
         entry)
    (with-temp-buffer
      (insert output)
      (setq entry (car (plist-get (majutsu-row-read-buffer compiled)
                                  :entries))))
    (should (equal (majutsu-row-render-body entry compiled)
                   "  - old-target\n  + new-target"))
    (should (equal (majutsu-row-column entry 'removed-target-ids)
                   '("0123456789abcdef" "1111111111111111")))
    (should (equal (majutsu-row-column entry 'added-target-ids)
                   '("fedcba9876543210")))))

(ert-deftest majutsu-bookmark-wash-list/nests-tracked-remotes-under-local ()
  (let ((output (concat
                 (majutsu-bookmark-test--ref
                  "dev" "" nil "dev: rymwrkkn b052a92d summary")
                 (majutsu-bookmark-test--ref
                  "dev" "origin" t
                  "  @origin (+2/-10+): sxwtxlqt/1 4bb5dd3b (hidden) summary")
                 (majutsu-bookmark-test--ref
                  "dev" "upstream" t
                  "  @upstream: rymwrkkn b052a92d summary")
                 (majutsu-bookmark-test--ref
                  "dev" "fork" nil
                  "dev@fork: puqnsxqv 8f250d6b summary"))))
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
               origin upstream
               (fork (seq-find (lambda (section)
                                 (and (eq (oref section type) 'jj-bookmark)
                                      (equal (oref section value) "dev@fork")))
                               sections)))
          (should dev)
          (magit-section-show dev)
          (setq sections (majutsu-bookmark-test--sections)
                origin (seq-find
                        (lambda (section)
                          (and (eq (oref section type) 'jj-bookmark)
                               (equal (oref section value) "dev@origin")))
                        sections)
                upstream (seq-find
                          (lambda (section)
                            (and (eq (oref section type) 'jj-bookmark)
                                 (equal (oref section value) "dev@upstream")))
                          sections))
          (should origin)
          (should upstream)
          (should fork)
          (should (eq (oref origin parent) dev))
          (should (eq (oref upstream parent) dev))
          (should (eq (oref fork parent) (oref dev parent)))
          (goto-char (point-min))
          (search-forward "@origin")
          (backward-char)
          (should (equal (majutsu-row-column
                          (majutsu-row-entry-at-point) 'remote)
                         "origin"))
          (should (string-match-p "^dev:" (buffer-string)))
          (should (string-match-p "^  @origin" (buffer-string)))
          (should (string-match-p "^  @upstream" (buffer-string)))
          (should (string-match-p "^dev@fork" (buffer-string))))))))

(ert-deftest majutsu-bookmark-wash-list/nests-tracked-remote-under-deleted-local ()
  (let ((output (concat
                 (majutsu-bookmark-test--ref
                  "gone" "" nil "gone (deleted)")
                 (majutsu-bookmark-test--ref
                  "gone" "origin" t "  @origin: abcdef01 deadbeef old"))))
    (with-temp-buffer
      (majutsu-bookmark-list-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (bookmark-list)
          (insert output)
          (majutsu-bookmark--wash-list nil))
        (let* ((sections (majutsu-bookmark-test--sections))
               (local (seq-find
                       (lambda (section)
                         (and (eq (oref section type) 'jj-bookmark)
                              (equal (oref section value) "gone")))
                       sections))
               remote)
          (should local)
          (magit-section-show local)
          (setq remote
                (seq-find
                 (lambda (section)
                   (and (eq (oref section type) 'jj-bookmark)
                        (equal (oref section value) "gone@origin")))
                 (majutsu-bookmark-test--sections)))
          (should remote)
          (should (eq (oref remote parent) local)))))))

(ert-deftest majutsu-bookmark-wash-list/preserves-custom-tail-columns ()
  (let* ((majutsu-bookmark-list-columns
          (append (list (car majutsu-bookmark-list-columns)
                        '(:field test-tail :module tail
                          :template "TAIL" :face nil))
                  (cdr majutsu-bookmark-list-columns)))
         (majutsu-bookmark--compiled-template-cache nil)
         (output
          (replace-regexp-in-string
           (regexp-quote (concat majutsu-row-tail-token
                                 majutsu-row-body-token))
           (concat majutsu-row-tail-token "TAIL" majutsu-row-body-token)
           (majutsu-bookmark-test--ref
            "dev" "" nil "dev: rymwrkkn b052a92d summary")
           t t)))
    (with-temp-buffer
      (majutsu-bookmark-list-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (bookmark-list)
          (insert output)
          (majutsu-bookmark--wash-list nil))
        (should (string-match-p "TAIL" (buffer-string)))))))

(ert-deftest majutsu-bookmark-wash-list/sectionizes-conflict-targets ()
  (let ((output
         (majutsu-bookmark-test--ref
          "topic" "" nil "topic (conflicted):"
          "  - old-one\n  - old-two\n  + new-one"
          '("0123456789abcdef" "1111111111111111")
          '("fedcba9876543210"))))
    (with-temp-buffer
      (majutsu-bookmark-list-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (bookmark-list)
          (insert output)
          (majutsu-bookmark--wash-list nil))
        (let* ((sections (majutsu-bookmark-test--sections))
               (topic (seq-find (lambda (section)
                                  (and (eq (oref section type) 'jj-bookmark)
                                       (equal (oref section value) "topic")))
                                sections))
               targets)
          (should topic)
          (magit-section-show topic)
          (setq targets
                (seq-filter
                 (lambda (section) (eq (oref section type) 'jj-commit))
                 (majutsu-bookmark-test--sections)))
          (should (equal (mapcar (lambda (section) (oref section value))
                                 targets)
                         '("0123456789abcdef"
                           "1111111111111111"
                           "fedcba9876543210")))
          (should (seq-every-p (lambda (section)
                                 (eq (oref section parent) topic))
                               targets))
          (goto-char (point-min))
          (search-forward "old-one")
          (backward-char)
          (should (equal (magit-section-value-if 'jj-commit)
                         "0123456789abcdef"))
          (should-not (majutsu-row-entry-at-point))
          (should (string-match-p (regexp-quote "  - old-one")
                                  (buffer-string)))
          (should (string-match-p (regexp-quote "  + new-one")
                                  (buffer-string))))))))

(ert-deftest majutsu-bookmark-wash-list/leaves-mismatched-conflict-as-body ()
  (let ((output
         (majutsu-bookmark-test--ref
          "topic" "" nil "topic (conflicted):"
          "  - old-target\n  + new-target"
          '("0123456789abcdef" "1111111111111111")
          '("fedcba9876543210"))))
    (with-temp-buffer
      (majutsu-bookmark-list-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (bookmark-list)
          (insert output)
          (majutsu-bookmark--wash-list nil))
        (let* ((sections (majutsu-bookmark-test--sections))
               (topic (seq-find (lambda (section)
                                  (and (eq (oref section type) 'jj-bookmark)
                                       (equal (oref section value) "topic")))
                                sections)))
          (should topic)
          (magit-section-show topic)
          (let ((targets
                 (seq-filter
                  (lambda (section) (eq (oref section type) 'jj-commit))
                  (majutsu-bookmark-test--sections))))
            (should-not targets)
            (should (string-match-p (regexp-quote "  - old-target")
                                    (buffer-string)))
            (should (string-match-p (regexp-quote "  + new-target")
                                    (buffer-string)))))))))

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
  (let (seen-history seen-category)
    (cl-letf (((symbol-function 'majutsu-bookmark-candidate-data)
               (lambda (&optional _candidates _directory)
                 (list :candidates '("main" "feature")
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection _predicate _require-match _initial hist _default)
                 (setq seen-history hist
                       seen-category (plist-get completion-extra-properties :category))
                 (should (equal collection '("main" "feature")))
                 "main")))
      (should (equal (majutsu-read-bookmark-name "Bookmark") "main"))
      (should (eq seen-history 'majutsu-bookmark-name-history))
      (should (eq seen-category 'majutsu-bookmark)))))

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
              ((symbol-function 'completing-read-multiple)
               (lambda (_prompt _collection _predicate _require-match _initial _hist default)
                 (setq seen-default default)
                 '("main"))))
      (should (equal (majutsu-read-bookmark-patterns "Bookmarks") '("main")))
      (should (equal seen-default "main,feature")))))

(ert-deftest majutsu-read-bookmark-patterns/filters-empty-input-and-uses-history ()
  (let (seen-history seen-category)
    (cl-letf (((symbol-function 'majutsu-bookmark-candidate-data)
               (lambda (&optional _candidates _directory)
                 (list :candidates '("main" "feature")
                       :entries (make-hash-table :test #'equal))))
              ((symbol-function 'majutsu-bookmark-at-point)
               (lambda () "main"))
              ((symbol-function 'completing-read-multiple)
               (lambda (_prompt collection _predicate _require-match _initial hist _default)
                 (setq seen-history hist
                       seen-category (plist-get completion-extra-properties :category))
                 (should (equal collection '("main" "feature")))
                 '("main" "glob:\"feat*\""))))
      (should (equal (majutsu-read-bookmark-patterns "Bookmarks")
                     '("main" "glob:\"feat*\"")))
      (should (eq seen-history 'majutsu-bookmark-pattern-history))
      (should (eq seen-category 'majutsu-bookmark)))))

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

(ert-deftest majutsu-bookmark-delete/interactive-confirms-selection ()
  (let (called confirmed)
    (cl-letf (((symbol-function 'majutsu-read-bookmark-patterns)
               (lambda (&rest _args) '("main")))
              ((symbol-function 'majutsu-confirm)
               (lambda (action prompt)
                 (setq confirmed (list action prompt))
                 t))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message) (lambda (&rest _args) nil)))
      (call-interactively #'majutsu-bookmark-delete)
      (should (equal confirmed
                     '(bookmark-delete
                       "Delete bookmark(s) main and propagate on next push? ")))
      (should (equal (seq-remove #'null (flatten-tree called))
                     '("bookmark" "delete" "main"))))))

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
