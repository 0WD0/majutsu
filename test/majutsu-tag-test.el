;;; majutsu-tag-test.el --- Tests for tag helpers  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for tag completion, parsing, and command dispatch.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-tag)
(require 'majutsu-jj-integration)

(defun majutsu-tag-test--row (name remote display)
  "Encode a native tag DISPLAY with structured NAME and REMOTE."
  (concat majutsu-row-start-token
          display
          majutsu-row-tail-token
          majutsu-row-body-token
          majutsu-row-meta-token
          (json-serialize name) majutsu-row-field-separator
          (json-serialize remote)
          majutsu-row-end-token "\n"))

(ert-deftest majutsu-tag-wash-list/uses-plain-section-values ()
  (with-temp-buffer
    (majutsu-tag-list-mode)
    (let ((inhibit-read-only t)
          (magit-section-inhibit-markers t))
      (magit-insert-section (tag-list)
        (insert (majutsu-tag-test--row "v1.0" "" "v1.0: qpvuntsm"))
        (majutsu-tag--wash-list nil))
      (let* ((sections (cons magit-root-section
                             (append (oref magit-root-section children)
                                     (apply #'append
                                            (mapcar (lambda (section)
                                                      (oref section children))
                                                    (oref magit-root-section children))))))
             (section (seq-find (lambda (section)
                                  (eq (oref section type) 'jj-tag))
                                sections))
             (value (oref section value)))
        (should section)
        (should (equal value "v1.0"))
        (should-not (text-properties-at 0 value))))))

(ert-deftest majutsu-tag-get-tag-names/local-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("v1.0" "v1.1"))))
      (should (equal (majutsu--get-tag-names 'local) '("v1.0" "v1.1")))
      (should (equal (seq-take seen-args 3) '("tag" "list" "--quiet")))
      (should-not (member "--all-remotes" seen-args))
      (should (member "-T" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "(!self.remote())") template))
        (should (string-match-p (regexp-quote "self.present()") template))))))

(ert-deftest majutsu-tag-get-tag-names/remote-args ()
  (let (seen-args)
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest args)
                 (setq seen-args (flatten-list args))
                 '("v1.0@git"))))
      (should (equal (majutsu--get-tag-names 'remote) '("v1.0@git")))
      (should (member "--all-remotes" seen-args))
      (let ((template (cadr (member "-T" seen-args))))
        (should (string-match-p (regexp-quote "self.remote()") template))
        (should (string-match-p (regexp-quote "self.present()") template))
        (should (string-match-p (regexp-quote "\"@\"") template))))))

(ert-deftest majutsu-tag-candidate-data/builds-structured-entries ()
  (let ((sep majutsu-tag--completion-field-separator))
    (cl-letf (((symbol-function 'majutsu-jj-lines)
               (lambda (&rest _args)
                 (list (concat "v1.0" sep "" sep "" sep "t" sep "" sep "t")
                       (concat "v1.0" sep "origin" sep "" sep "t" sep "t" sep "t")
                       (concat "v2.0" sep "fork" sep "t" sep "t" sep "" sep "")))))
      (let* ((payload (majutsu-tag-candidate-data))
             (candidates (plist-get payload :candidates))
             (entries (plist-get payload :entries))
             (v1 (gethash "v1.0" entries))
             (v2 (gethash "v2.0" entries)))
        (should (equal candidates '("v1.0")))
        (should (plist-get v1 :local))
        (should (plist-get v1 :synced))
        (should (equal (plist-get v1 :tracked-remotes) '("origin")))
        (should (plist-get v2 :conflict))
        (should (equal (plist-get v2 :untracked-remotes) '("fork")))))))

(ert-deftest majutsu-tag-read-exact-names/uses-name-history ()
  (let* ((payload (list :candidates '("v1.0" "v1.1")
                        :entries (make-hash-table :test #'equal)))
         seen-history
         seen-category)
    (cl-letf (((symbol-function 'majutsu-tag-candidate-data)
               (lambda (&optional _directory) payload))
              ((symbol-function 'completing-read-multiple)
               (lambda (_prompt collection _predicate _require-match
                                _initial history _default)
                 (setq seen-history history
                       seen-category (plist-get completion-extra-properties :category))
                 (should (equal collection '("v1.0" "v1.1")))
                 '("v1.0"))))
      (should (equal (majutsu-tag--read-exact-names "Set tag(s)") '("v1.0")))
      (should (eq seen-history 'majutsu-tag-name-history))
      (should (eq seen-category 'majutsu-tag)))))

(ert-deftest majutsu-tag-at-point/uses-structured-tag-field ()
  (with-temp-buffer
    (insert (propertize "v1.0" 'majutsu-row-field 'tags))
    (goto-char 2)
    (cl-letf (((symbol-function 'magit-section-value-if) #'ignore)
              ((symbol-function 'majutsu-jj-revision-p)
               (lambda (revision) (equal revision "v1.0"))))
      (should (equal (majutsu-tag-at-point) "v1.0")))))

(ert-deftest majutsu-tag-read-exact-names/defaults-to-tag-at-point ()
  (let* ((payload (list :candidates '("v1.0")
                        :entries (make-hash-table :test #'equal)))
         seen-default)
    (cl-letf (((symbol-function 'majutsu-tag-at-point)
               (lambda () "v1.0"))
              ((symbol-function 'majutsu-tag-candidate-data)
               (lambda (&optional _directory) payload))
              ((symbol-function 'completing-read-multiple)
               (lambda (_prompt _collection _predicate _require-match _initial _history default)
                 (setq seen-default default)
                 '("v1.0"))))
      (should (equal (majutsu-tag--read-exact-names "Set tag(s)") '("v1.0")))
      (should (equal seen-default "v1.0")))))

(ert-deftest majutsu-tag-read-patterns/uses-pattern-history ()
  (let* ((payload (list :candidates '("v1.0" "v1.1")
                        :entries (make-hash-table :test #'equal)))
         seen-history)
    (cl-letf (((symbol-function 'majutsu-tag-candidate-data)
               (lambda (&optional _directory) payload))
              ((symbol-function 'completing-read-multiple)
               (lambda (&rest args)
                 (setq seen-history (nth 5 args))
                 '("v1.*"))))
      (should (equal (majutsu-tag--read-patterns "Delete tag(s)") '("v1.*")))
      (should (eq seen-history 'majutsu-tag-pattern-history)))))

(ert-deftest majutsu-tag-wash-list/deleted-and-remote-rows ()
  "Deleted headers remain distinct and remote rows select their own revision."
  (with-temp-buffer
    (majutsu-tag-list-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (tag-list)
        (insert
         (majutsu-tag-test--row "before" "" "before: local target")
         (majutsu-tag-test--row "deleted" "" "deleted (deleted)")
         (majutsu-tag-test--row "deleted" "origin" "  @origin: remote target")
         (majutsu-tag-test--row "moved" "" "moved: local target")
         (majutsu-tag-test--row "moved" "origin"
                               "  @origin (ahead by 1 commits): remote target"))
        (majutsu-tag--wash-list nil)))
    (should (equal (mapcar (lambda (section) (oref section value))
                          (oref magit-root-section children))
                   '("before" "deleted" "moved")))
    (goto-char (point-min))
    (dolist (expected '(("before" "before")
                        ("deleted" "deleted")
                        ("deleted@origin" "deleted")
                        ("moved" "moved")
                        ("moved@origin" "moved")))
      (should (equal (majutsu-revision-at-point) (car expected)))
      (should (equal (majutsu-tag-at-point) (cadr expected)))
      (forward-line 1))))

(ert-deftest majutsu-tag-wash-list/preserves-conflict-display ()
  "Conflict display lines stay with their own structured tag entry."
  (let ((display "conflicted_tag (conflicted):\n  - old target\n  + new target"))
    (with-temp-buffer
      (majutsu-tag-list-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (tag-list)
          (insert (majutsu-tag-test--row "conflicted_tag" "" display)
                  (majutsu-tag-test--row "v1.0" "" "v1.0: abcdef"))
          (majutsu-tag--wash-list nil)))
      (should (equal (buffer-string) (concat display "\nv1.0: abcdef\n\n")))
      (goto-char (point-min))
      (forward-line 2)
      (should (equal (majutsu-tag-at-point) "conflicted_tag"))
      (forward-line 1)
      (should (equal (majutsu-revision-at-point) "v1.0")))))

(ert-deftest majutsu-tag-wash-list/native-tracked-remotes ()
  "Native jj output preserves deleted tags and divergent remote targets."
  (majutsu-jj-integration-with-sandbox sandbox
    (let* ((remote (majutsu-jj-integration-init sandbox "remote"))
           (repo (expand-file-name "local" sandbox))
           (majutsu-jj-executable majutsu-jj-integration--jj))
      (majutsu-jj-integration-commit remote "remote target")
      (majutsu-jj-integration-call remote "tag" "set" "deleted" "moved" "-r" "@-")
      (majutsu-jj-integration-call sandbox "git" "clone" remote repo)
      (majutsu-jj-integration-call repo "tag" "delete" "deleted")
      (majutsu-jj-integration-call repo "describe" "-m" "local target")
      (majutsu-jj-integration-call repo "tag" "set" "--allow-move" "moved" "-r" "@")
      (let* ((default-directory (file-name-as-directory repo))
             (native (majutsu-jj-integration-output
                      repo "--color=never" "tag" "list" "--quiet" "--all-remotes"))
             (local-id (majutsu-jj-integration-output
                        repo "log" "--no-graph" "-r" "moved" "-T" "commit_id"))
             (remote-id (majutsu-jj-integration-output
                         repo "log" "--no-graph" "-r" "moved@origin" "-T" "commit_id")))
        (should-not (equal local-id remote-id))
        (with-temp-buffer
          (majutsu-tag-list-mode)
          (setq-local majutsu-tag--list-all-remotes t)
          (let ((inhibit-read-only t))
            (majutsu-tag-list-refresh-buffer))
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         (concat native "\n")))
          (goto-char (point-min))
          (should (looking-at "deleted (deleted)"))
          (should (equal (majutsu-tag-at-point) "deleted"))
          (should (equal (majutsu-revision-at-point) "deleted"))
          (forward-line 1)
          (should (equal (majutsu-revision-at-point) "deleted@origin"))
          (re-search-forward "^moved:")
          (should (equal (majutsu-revision-at-point) "moved"))
          (re-search-forward "^  @origin")
          (should (equal (majutsu-tag-at-point) "moved"))
          (should (equal (majutsu-revision-at-point) "moved@origin"))
          (should (equal (majutsu-jj-integration-output
                          repo "log" "--no-graph" "-r"
                          (majutsu-revision-at-point) "-T" "commit_id")
                         remote-id)))))))

(ert-deftest majutsu-tag-set/allow-move-dispatches-correctly ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-tag-set '("v1.0" "release") "@-" t)
      (should (equal (seq-remove #'null (flatten-tree called))
                     '("tag" "set" "--allow-move" "-r" "@-" "v1.0" "release"))))))

(ert-deftest majutsu-tag-delete/dispatches-correctly ()
  (let (called)
    (cl-letf (((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (majutsu-tag-delete '("v1.*" "release"))
      (should (equal (seq-remove #'null (flatten-tree called))
                     '("tag" "delete" "v1.*" "release"))))))

(provide 'majutsu-tag-test)
;;; majutsu-tag-test.el ends here
