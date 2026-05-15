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
         (option-prototype (majutsu-gerrit-test--suffix-prototype option)))
    (should (cl-typep option-prototype 'majutsu-gerrit-upload-option))
    (should (equal (oref option-prototype argument) "--revision="))
    (should (eq (oref option-prototype multi-value) 'repeat))
    (should (equal (oref option-prototype selection-label) "[REV]"))
    (should (equal (oref option-prototype selection-toggle-key) "r"))))

(ert-deftest majutsu-gerrit-upload-transient/uses-shared-remote-reader ()
  "Upload remote option should use the exact remote reader."
  (let* ((remote (transient-get-suffix 'majutsu-gerrit-upload-transient "-R"))
         (reader (majutsu-gerrit-test--suffix-reader remote)))
    (should (or (eq reader 'majutsu-transient-read-remote-name)
                (equal reader '(function majutsu-transient-read-remote-name))))))

(ert-deftest majutsu-gerrit-upload-transient/uses-rest-backed-review-readers ()
  "Review metadata readers should use the Gerrit completion helpers."
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
               (lambda (prompt _collection &rest _rest) "main")))
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

(ert-deftest majutsu-gerrit--base-url-from-review-url/preserves-subpath ()
  (should (equal (majutsu-gerrit--base-url-from-review-url
                  "https://review.example.com/gerrit/")
                 "https://review.example.com/gerrit")))

(ert-deftest majutsu-gerrit-account-candidate-data/prefers-email-and-annotates ()
  (let ((majutsu-gerrit--completion-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'majutsu-gerrit--rest-base-url)
               (lambda (&rest _args) "https://review.example.com"))
              ((symbol-function 'majutsu-gerrit--rest-get)
               (lambda (&rest _args)
                 '(((name . "Alice Example")
                    (username . "alice")
                    (email . "alice@example.com"))
                   ((name . "Bob Builder")
                    (username . "bob"))))))
      (let* ((payload (majutsu-gerrit-account-candidate-data "gerrit" "/repo"))
             (annotations (plist-get payload :annotations))
             (entries (plist-get payload :entries)))
        (should (equal (plist-get payload :candidates)
                       '("alice@example.com" "bob")))
        (should (equal (gethash "alice@example.com" annotations)
                       "Alice Example @alice"))
        (should (equal (gethash "bob" annotations)
                       "Bob Builder"))
        (should (equal (plist-get (gethash "alice@example.com" entries) :username)
                       "alice"))))))

(ert-deftest majutsu-gerrit-topic-candidate-data/dedupes-and-counts ()
  (cl-letf (((symbol-function 'majutsu-gerrit--project-open-changes)
             (lambda (&rest _args)
               '(((topic . "alpha"))
                 ((topic . "alpha"))
                 ((topic . "beta"))))))
    (let* ((payload (majutsu-gerrit-topic-candidate-data "gerrit" "/repo"))
           (annotations (plist-get payload :annotations)))
      (should (equal (plist-get payload :candidates)
                     '("alpha" "beta")))
      (should (equal (gethash "alpha" annotations)
                     "topic (2 open)"))
      (should (equal (gethash "beta" annotations)
                     "topic (1 open)")))))

(ert-deftest majutsu-gerrit-hashtag-candidate-data/dedupes-and-counts ()
  (cl-letf (((symbol-function 'majutsu-gerrit--project-open-changes)
             (lambda (&rest _args)
               '(((hashtags . ("one" "two")))
                 ((hashtags . ("two" "three")))
                 ((hashtags . ("two")))))))
    (let* ((payload (majutsu-gerrit-hashtag-candidate-data "gerrit" "/repo"))
           (annotations (plist-get payload :annotations)))
      (should (equal (plist-get payload :candidates)
                     '("one" "three" "two")))
      (should (equal (gethash "two" annotations)
                     "hashtag (3 open)")))))

(ert-deftest majutsu-gerrit-label-candidate-data/filters-permitted-votes ()
  (let ((changes
         '(((labels
             . ((Code-Review
                 . ((values
                     . ((-2 . "Do not submit")
                        (-1 . "Needs work")
                        (0 . "No score")
                        (1 . "Looks good")
                        (2 . "Approved")))
                    (all
                     . (((username . "alice")
                         (permitted_voting_range
                          . ((min . -1) (max . 1))))))))))))))
    (cl-letf (((symbol-function 'majutsu-gerrit--auth-user)
               (lambda (&rest _args) "alice"))
              ((symbol-function 'majutsu-gerrit--project-open-changes)
               (lambda (&rest _args) changes)))
      (let* ((payload (majutsu-gerrit-label-candidate-data "gerrit" "/repo"))
             (annotations (plist-get payload :annotations)))
        (should (equal (plist-get payload :candidates)
                       '("Code-Review" "Code-Review+0" "Code-Review-1")))
        (should (equal (gethash "Code-Review" annotations)
                       "Code-Review +1 Looks good"))
        (should-not (member "Code-Review+2" (plist-get payload :candidates)))))))

(ert-deftest majutsu-gerrit-upload-read-reviewer/forwards-rest-payload ()
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
      (should (equal (nth 0 (plist-get seen :reader-args))
                     "Reviewer"))
      (should (equal (nth 7 (plist-get seen :reader-args))
                     'majutsu-gerrit-account))
      (should (equal (nth 9 (plist-get seen :reader-args))
                     "/repo")))))

(ert-deftest majutsu-gerrit-upload-read-topic/forwards-project-payload ()
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
      (should (equal (plist-get seen :payload-args)
                     '(nil "/repo")))
      (should (equal (nth 7 (plist-get seen :reader-args))
                     'majutsu-gerrit-topic)))))

(provide 'majutsu-gerrit-test)
;;; majutsu-gerrit-test.el ends here
