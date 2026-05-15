;;; majutsu-gerrit-test.el --- Tests for majutsu-gerrit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for `majutsu-gerrit.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-gerrit)
(require 'majutsu-git)

(defun majutsu-gerrit-test--suffix-prototype (suffix)
  "Return the prototype object configured for transient SUFFIX."
  (when-let* ((command (plist-get (cdr suffix) :command)))
    (get command 'transient--suffix)))

(defun majutsu-gerrit-test--suffix-reader (suffix)
  "Return the reader configured for transient SUFFIX."
  (or (plist-get (cdr suffix) :reader)
      (when-let* ((prototype (majutsu-gerrit-test--suffix-prototype suffix)))
        (oref prototype reader))))

(ert-deftest majutsu-gerrit-upload/starts-jj-gerrit-upload-async ()
  "Upload should dispatch to `jj gerrit upload' asynchronously."
  (let (seen-args seen-success seen-message)
    (cl-letf (((symbol-function 'majutsu--message-with-log)
               (lambda (format-string &rest args)
                 (setq seen-message (apply #'format format-string args))))
              ((symbol-function 'majutsu-gerrit--start)
               (lambda (args &optional success-msg _finish-callback)
                 (setq seen-args args)
                 (setq seen-success success-msg)
                 'process)))
      (should (eq (majutsu-gerrit-upload '("--revision=@-" "--dry-run"))
                  'process))
      (should (equal seen-message "Uploading to Gerrit..."))
      (should (equal seen-args '("upload" "--revision=@-" "--dry-run")))
      (should (equal seen-success "Gerrit upload dry run completed")))))

(ert-deftest majutsu-gerrit-upload/uses-success-message-for-real-upload ()
  "Non-dry-run upload should use a normal success message."
  (let (seen-success)
    (cl-letf (((symbol-function 'majutsu--message-with-log)
               (lambda (&rest _args) nil))
              ((symbol-function 'majutsu-gerrit--start)
               (lambda (_args &optional success-msg _finish-callback)
                 (setq seen-success success-msg))))
      (majutsu-gerrit-upload '("--revision=@-"))
      (should (equal seen-success "Uploaded to Gerrit")))))

(ert-deftest majutsu-gerrit-upload-arguments/direct-uses-jj-default ()
  "Outside the transient, leave revisions unset so jj can default @/@-."
  (let ((transient-current-command nil))
    (should-not (majutsu-gerrit-upload-arguments))))

(ert-deftest majutsu-gerrit-upload-arguments/uses-transient-args ()
  "Inside the transient, use the transient's explicit selections only."
  (let ((transient-current-command 'majutsu-gerrit-upload-transient))
    (cl-letf (((symbol-function 'transient-args)
               (lambda (_prefix) '("--dry-run" "--revision=abc"))))
      (should (equal (majutsu-gerrit-upload-arguments)
                     '("--dry-run" "--revision=abc"))))))

(ert-deftest majutsu-gerrit-upload-read-revset/uses-native-completion-context ()
  "The upload revision reader should complete in jj gerrit upload context."
  (let (seen-prompt seen-default seen-context)
    (cl-letf (((symbol-function 'majutsu-read-revset)
               (lambda (prompt default completion-args)
                 (setq seen-prompt prompt)
                 (setq seen-default default)
                 (setq seen-context completion-args)
                 "@-")))
      (should (equal (majutsu-gerrit-upload--read-revset
                      "Revision: " "@" nil)
                     "@-"))
      (should (equal seen-prompt "Revision: "))
      (should (equal seen-default "@"))
      (should (equal seen-context '("gerrit" "upload" "-r"))))))

(ert-deftest majutsu-gerrit-upload-transient/uses-toggle-at-point-revision-selection ()
  "Upload revisions should use Majutsu's selection/toggle UI."
  (let* ((option (transient-get-suffix 'majutsu-gerrit-upload-transient "-r"))
         (toggle (transient-get-suffix 'majutsu-gerrit-upload-transient "r"))
         (option-prototype (majutsu-gerrit-test--suffix-prototype option))
         (toggle-prototype (majutsu-gerrit-test--suffix-prototype toggle)))
    (should (cl-typep option-prototype 'majutsu-gerrit-upload-option))
    (should (equal (oref option-prototype argument) "--revision="))
    (should (eq (oref option-prototype multi-value) 'repeat))
    (should (equal (oref option-prototype selection-label) "[REV]"))
    (should (cl-typep toggle-prototype 'majutsu-selection-toggle-option))
    (should (equal (oref toggle-prototype argument) "--revision="))
    (should (eq (oref toggle-prototype multi-value) 'repeat))))

(ert-deftest majutsu-gerrit-upload-transient/uses-shared-remote-reader ()
  "Upload remote option should use the exact remote reader."
  (let* ((remote (transient-get-suffix 'majutsu-gerrit-upload-transient "-R"))
         (reader (majutsu-gerrit-test--suffix-reader remote)))
    (should (or (eq reader 'majutsu-transient-read-remote-name)
                (equal reader '(function majutsu-transient-read-remote-name))))))

(ert-deftest majutsu-gerrit-upload-transient/uses-borrowed-review-readers ()
  "Review metadata readers should use Majutsu's borrowed-provider adapters."
  (let* ((reviewer (transient-get-suffix 'majutsu-gerrit-upload-transient "-v"))
         (cc (transient-get-suffix 'majutsu-gerrit-upload-transient "-C"))
         (label (transient-get-suffix 'majutsu-gerrit-upload-transient "-l"))
         (topic (transient-get-suffix 'majutsu-gerrit-upload-transient "-T"))
         (hashtag (transient-get-suffix 'majutsu-gerrit-upload-transient "-H")))
    (should (eq (majutsu-gerrit-test--suffix-reader reviewer)
                'majutsu-gerrit-upload--read-reviewer))
    (should (eq (majutsu-gerrit-test--suffix-reader cc)
                'majutsu-gerrit-upload--read-cc))
    (should (eq (majutsu-gerrit-test--suffix-reader label)
                'majutsu-gerrit-upload--read-label))
    (should (eq (majutsu-gerrit-test--suffix-reader topic)
                'majutsu-gerrit-upload--read-topic))
    (should (eq (majutsu-gerrit-test--suffix-reader hashtag)
                'majutsu-gerrit-upload--read-hashtag))))

(ert-deftest majutsu-gerrit-upload-repo-args/keeps-stable-policy-only ()
  "Repository defaults should omit change-specific and dangerous options."
  (should (equal (majutsu-gerrit-upload--repo-args
                  '("--remote=gerrit"
                    "--remote-branch=main"
                    "--notify=owner"
                    "--ignore-attention-set"
                    "--revision=@-"
                    "--dry-run"
                    "--message=patch set"
                    "--submit"
                    "--skip-validation"
                    "--wip"
                    "--ready"
                    "--private"
                    "--remove-private"
                    "--reviewer=a@example.invalid"
                    "--cc=b@example.invalid"
                    "--topic=topic"
                    "--hashtag=hash"
                    "--label=Commit-Queue+1"))
                 '("--remote=gerrit"
                   "--remote-branch=main"
                   "--notify=owner"
                   "--ignore-attention-set"))))

(ert-deftest majutsu-gerrit-upload-transient/exposes-repo-default-action ()
  "Gerrit upload transient should expose repository-local defaults."
  (let ((suffix (transient-get-suffix 'majutsu-gerrit-upload-transient "W")))
    (should suffix)
    (should (eq (plist-get (cdr suffix) :command)
                'majutsu-transient-save-repository-defaults))))

(ert-deftest majutsu-gerrit--remote-branch-names/parses-single-remote ()
  "Should extract remote bookmark names from `jj bookmark list --remote'."
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest args)
               (should (member "--remote" args))
               (should (member "gerrit" args))
               '("feat/tramp" "" "main" "" "")))
            ((symbol-function 'majutsu--toplevel-safe)
             (lambda () "/repo")))
    (should (equal (majutsu-gerrit--remote-branch-names "gerrit")
                   '("feat/tramp" "main")))))

(ert-deftest majutsu-gerrit--remote-branch-names/dedupes-across-remotes ()
  "When no remote is given, should dedupe names from all remotes."
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _args)
               '("main" "dev" "main" "feat/tramp" "")))
            ((symbol-function 'majutsu--toplevel-safe)
             (lambda () "/repo")))
    (should (equal (majutsu-gerrit--remote-branch-names)
                   '("dev" "feat/tramp" "main")))))

(ert-deftest majutsu-gerrit--remote-branch-names/returns-nil-on-error ()
  "Should return nil when jj command fails."
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _args)
               (error "jj error")))
            ((symbol-function 'majutsu--toplevel-safe)
             (lambda () "/repo")))
    (should-not (majutsu-gerrit--remote-branch-names "origin"))))

(ert-deftest majutsu-gerrit-upload-read-remote-branch/extracts-remote-from-transient ()
  "Reader should extract --remote from current transient args."
  (let ((transient-args-remote nil))
    (cl-letf (((symbol-function 'transient-get-value)
               (lambda () '("--remote=gerrit" "--dry-run")))
              ((symbol-function 'majutsu-gerrit--remote-branch-names)
               (lambda (remote)
                 (setq transient-args-remote remote)
                 '("main" "dev")))
              ((symbol-function 'completing-read)
               (lambda (_prompt _collection &rest _rest) "main")))
      (should (equal (majutsu-gerrit-upload--read-remote-branch
                      "Remote branch: " nil nil)
                     "main"))
      (should (equal transient-args-remote "gerrit")))))

(ert-deftest majutsu-gerrit-upload-read-remote-branch/passes-branches-to-completing-read ()
  "Reader should present remote branch names as completion candidates."
  (let (seen-collection)
    (cl-letf (((symbol-function 'transient-get-value)
               (lambda () nil))
              ((symbol-function 'majutsu-gerrit--remote-branch-names)
               (lambda (_remote) '("main" "dev")))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _rest)
                 (setq seen-collection collection)
                 "dev")))
      (majutsu-gerrit-upload--read-remote-branch "Remote branch: " nil nil)
      (should (equal seen-collection '("main" "dev"))))))

(ert-deftest majutsu-gerrit-upload-read-remote-branch/allows-free-input ()
  "Reader should allow arbitrary input when no branch matches."
  (cl-letf (((symbol-function 'transient-get-value)
             (lambda () nil))
            ((symbol-function 'majutsu-gerrit--remote-branch-names)
             (lambda (_remote) '("main" "dev")))
            ((symbol-function 'completing-read)
             (lambda (_prompt _collection &rest _rest) "feature/new")))
    (should (equal (majutsu-gerrit-upload--read-remote-branch
                    "Remote branch: " nil nil)
                   "feature/new"))))

(ert-deftest majutsu-gerrit--project-from-remote-url/parses-common-forms ()
  (should (equal (majutsu-gerrit--project-from-remote-url
                  "https://review.gerrithub.io/team/project")
                 "team/project"))
  (should (equal (majutsu-gerrit--project-from-remote-url
                  "ssh://user@review.example.com:29418/team/project")
                 "team/project"))
  (should (equal (majutsu-gerrit--project-from-remote-url
                  "user@review.example.com:team/project.git")
                 "team/project")))

(ert-deftest majutsu-gerrit--base-url-from-remote-url/parses-common-forms ()
  (should (equal (majutsu-gerrit--base-url-from-remote-url
                  "https://review.gerrithub.io/team/project")
                 "https://review.gerrithub.io"))
  (should (equal (majutsu-gerrit--base-url-from-remote-url
                  "ssh://user@review.example.com:29418/team/project")
                 "https://review.example.com"))
  (should (equal (majutsu-gerrit--base-url-from-remote-url
                  "user@review.example.com:team/project.git")
                 "https://review.example.com")))

(ert-deftest majutsu-gerrit--shared-spec/ssh-remote-requires-explicit-web-origin ()
  (cl-letf (((symbol-function 'majutsu-gerrit--remote-url)
             (lambda (&rest _args)
               "ssh://admin@192.168.110.139:29418/majutsu"))
            ((symbol-function 'majutsu-gerrit--config-get)
             (lambda (_key) nil)))
    (should-not (majutsu-gerrit--shared-spec))))

(ert-deftest majutsu-gerrit--base-url-from-review-url/preserves-subpath ()
  (should (equal (majutsu-gerrit--base-url-from-review-url
                  "https://review.example.com/gerrit/")
                 "https://review.example.com/gerrit")))

(ert-deftest majutsu-gerrit--make-account-payload/prefers-email-and-annotates ()
  (let* ((payload (majutsu-gerrit--make-account-payload
                   '(((name . "Alice Example")
                      (username . "alice")
                      (email . "alice@example.com"))
                     ((name . "Bob Builder")
                      (username . "bob")))))
         (annotations (plist-get payload :annotations)))
    (should (equal (plist-get payload :candidates)
                   '("alice@example.com" "bob")))
    (should (equal (gethash "alice@example.com" annotations)
                   "Alice Example @alice"))
    (should (equal (gethash "bob" annotations)
                   "Bob Builder"))))

(ert-deftest majutsu-gerrit-account-candidate-data/prefers-gerrit-el-first ()
  (cl-letf (((symbol-function 'majutsu-gerrit--gerrit-account-candidate-data)
             (lambda (&rest _args) '(:candidates ("alice"))))
            ((symbol-function 'majutsu-gerrit--egerrit-account-candidate-data)
             (lambda (&rest _args) '(:candidates ("bob")))))
    (should (equal (majutsu-gerrit-account-candidate-data)
                   '(:candidates ("alice"))))))

(ert-deftest majutsu-gerrit-account-candidate-data/falls-back-to-egerrit ()
  (cl-letf (((symbol-function 'majutsu-gerrit--gerrit-account-candidate-data)
             (lambda (&rest _args) nil))
            ((symbol-function 'majutsu-gerrit--egerrit-account-candidate-data)
             (lambda (&rest _args) '(:candidates ("bob@example.com")))))
    (should (equal (majutsu-gerrit-account-candidate-data)
                   '(:candidates ("bob@example.com"))))))

(ert-deftest majutsu-gerrit--gerrit-account-candidate-data/borrows-gerrit-el-accounts ()
  (cl-letf (((symbol-function 'majutsu-gerrit--with-gerrit-context)
             (lambda (fn &optional _remote _directory)
               (funcall fn nil)))
            ((symbol-function 'gerrit-get-accounts-alist)
             (lambda ()
               '((101 . ((name . "Alice Example")
                         (username . "alice")
                         (email . "alice@example.com")))))))
    (let ((payload (majutsu-gerrit--gerrit-account-candidate-data)))
      (should (equal (plist-get payload :candidates)
                     '("alice@example.com"))))))

(ert-deftest majutsu-gerrit--egerrit-account-candidate-data/borrows-egerrit-users ()
  (cl-letf (((symbol-function 'majutsu-gerrit--with-egerrit-context)
             (lambda (fn &optional _remote _directory)
               (funcall fn nil)))
            ((symbol-function 'egerrit-users)
             (lambda (&optional _seed)
               '(("Alice Example  alice@example.com" . "alice@example.com")))))
    (let* ((payload (majutsu-gerrit--egerrit-account-candidate-data))
           (annotations (plist-get payload :annotations)))
      (should (equal (plist-get payload :candidates)
                     '("alice@example.com")))
      (should (equal (gethash "alice@example.com" annotations)
                     "Alice Example  alice@example.com")))))

(ert-deftest majutsu-gerrit-topic-candidate-data/prefers-gerrit-el-first ()
  (cl-letf (((symbol-function 'majutsu-gerrit--gerrit-topic-candidate-data)
             (lambda (&rest _args) '(:candidates ("alpha"))))
            ((symbol-function 'majutsu-gerrit--egerrit-topic-candidate-data)
             (lambda (&rest _args) '(:candidates ("beta")))))
    (should (equal (majutsu-gerrit-topic-candidate-data)
                   '(:candidates ("alpha"))))))

(ert-deftest majutsu-gerrit--gerrit-topic-candidate-data/borrows-open-review-topics ()
  (cl-letf (((symbol-function 'majutsu-gerrit--remote-url)
             (lambda (&rest _args)
               "ssh://admin@review.example.com:29418/team/project"))
            ((symbol-function 'majutsu-gerrit--with-gerrit-context)
             (lambda (fn &optional _remote _directory)
               (funcall fn nil)))
            ((symbol-function 'gerrit-rest-open-reviews-for-project)
             (lambda (_project)
               '(((topic . "alpha"))
                 ((topic . "alpha"))
                 ((topic . "beta"))))))
    (let* ((payload (majutsu-gerrit--gerrit-topic-candidate-data))
           (annotations (plist-get payload :annotations)))
      (should (equal (plist-get payload :candidates)
                     '("alpha" "beta")))
      (should (equal (gethash "alpha" annotations)
                     "topic (2 open)")))))

(ert-deftest majutsu-gerrit-label-candidate-data/uses-egerrit-borrowed-candidates ()
  (cl-letf (((symbol-function 'majutsu-gerrit--egerrit-label-candidate-data)
             (lambda (&rest _args) '(:candidates ("Code-Review")))))
    (should (equal (majutsu-gerrit-label-candidate-data)
                   '(:candidates ("Code-Review"))))))

(ert-deftest majutsu-gerrit--egerrit-label-candidate-data/converts-review-candidates ()
  (cl-letf (((symbol-function 'majutsu-gerrit--remote-url)
             (lambda (&rest _args)
               "ssh://admin@review.example.com:29418/team/project"))
            ((symbol-function 'majutsu-gerrit--with-egerrit-context)
             (lambda (fn &optional _remote _directory)
               (funcall fn nil)))
            ((symbol-function 'egerrit--read-changes)
             (lambda (&rest _args) '(dummy-json-change)))
            ((symbol-function 'egerrit--create-change)
             (lambda (_json) 'change))
            ((symbol-function 'egerrit-get-detailed-change)
             (lambda (_change) 'detailed-change))
            ((symbol-function 'egerrit--review-label-candidates)
             (lambda (_change)
               '(("CR: +1 Looks good" . ("Code-Review" . "+1"))
                 ("CR: +2 Approved" . ("Code-Review" . "+2"))))))
    (let* ((payload (majutsu-gerrit--egerrit-label-candidate-data))
           (annotations (plist-get payload :annotations)))
      (should (equal (plist-get payload :candidates)
                     '("Code-Review" "Code-Review+2")))
      (should (equal (gethash "Code-Review" annotations)
                     "CR: +1 Looks good")))))

(ert-deftest majutsu-gerrit-upload-read-reviewer/uses-borrowed-payload-when-available ()
  (let (seen)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _dir) "/repo"))
              ((symbol-function 'majutsu-gerrit-account-candidate-data)
               (lambda (&rest args)
                 (setq seen (plist-put seen :payload-args args))
                 '(:category majutsu-gerrit-account
                   :candidates ("alice@example.com"))))
              ((symbol-function 'majutsu-completing-read-multiple-payload)
               (lambda (&rest args)
                 (setq seen (plist-put seen :reader-args args))
                 '("alice@example.com"))))
      (should (equal (majutsu-gerrit-upload--read-reviewer
                      "Reviewer" nil nil)
                     '("alice@example.com")))
      (should (equal (plist-get seen :payload-args)
                     '(nil "/repo" nil)))
      (should (equal (nth 7 (plist-get seen :reader-args))
                     'majutsu-gerrit-account)))))

(ert-deftest majutsu-gerrit-upload-read-reviewer/falls-back-to-free-multiple-input ()
  (let (seen)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _dir) "/repo"))
              ((symbol-function 'majutsu-gerrit-account-candidate-data)
               (lambda (&rest _args) nil))
              ((symbol-function 'majutsu-completing-read-multiple)
               (lambda (&rest args)
                 (setq seen args)
                 '("alice@example.com"))))
      (should (equal (majutsu-gerrit-upload--read-reviewer
                      "Reviewer" nil nil)
                     '("alice@example.com")))
      (should (equal (nth 1 seen) '())))))

(ert-deftest majutsu-gerrit-upload-read-topic/uses-borrowed-payload-when-available ()
  (let (seen)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _dir) "/repo"))
              ((symbol-function 'majutsu-gerrit-topic-candidate-data)
               (lambda (&rest args)
                 (setq seen (plist-put seen :payload-args args))
                 '(:category majutsu-gerrit-topic
                   :candidates ("alpha"))))
              ((symbol-function 'majutsu-completing-read-payload)
               (lambda (&rest args)
                 (setq seen (plist-put seen :reader-args args))
                 "alpha")))
      (should (equal (majutsu-gerrit-upload--read-topic
                      "Topic" nil nil)
                     "alpha"))
      (should (equal (nth 7 (plist-get seen :reader-args))
                     'majutsu-gerrit-topic)))))

(ert-deftest majutsu-gerrit-upload-read-topic/falls-back-to-borrowed-gerrit-topic-reader ()
  (cl-letf (((symbol-function 'majutsu--toplevel-safe)
             (lambda (&optional _dir) "/repo"))
            ((symbol-function 'majutsu-gerrit-topic-candidate-data)
             (lambda (&rest _args) nil))
            ((symbol-function 'majutsu-gerrit--gerrit-available-p)
             (lambda () t))
            ((symbol-function 'majutsu-gerrit--with-gerrit-context)
             (lambda (fn &optional _remote _directory)
               (funcall fn nil)))
            ((symbol-function 'gerrit-upload:--read-topic)
             (lambda (&rest _args) "topic-from-gerrit.el")))
    (should (equal (majutsu-gerrit-upload--read-topic
                    "Topic" nil nil)
                   "topic-from-gerrit.el"))))

(ert-deftest majutsu-gerrit-upload-read-label/uses-borrowed-payload-when-available ()
  (let (seen)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _dir) "/repo"))
              ((symbol-function 'majutsu-gerrit-label-candidate-data)
               (lambda (&rest _args)
                 '(:category majutsu-gerrit-label
                   :candidates ("Code-Review"))))
              ((symbol-function 'majutsu-completing-read-multiple-payload)
               (lambda (&rest args)
                 (setq seen args)
                 '("Code-Review"))))
      (should (equal (majutsu-gerrit-upload--read-label
                      "Label" nil nil)
                     '("Code-Review")))
      (should (eq (nth 7 seen) 'majutsu-gerrit-label)))))

(ert-deftest majutsu-git-transient/exposes-gerrit-upload ()
  "Git transient should expose Gerrit upload from the sync group."
  (let ((suffix (transient-get-suffix 'majutsu-git-transient "u")))
    (should suffix)
    (should (eq (plist-get (cdr suffix) :command)
                'majutsu-gerrit-upload-transient))))

(provide 'majutsu-gerrit-test)
;;; majutsu-gerrit-test.el ends here
