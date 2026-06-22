;;; majutsu-gerrit-test.el --- Tests for majutsu-gerrit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused tests for `majutsu-gerrit.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-gerrit)
(require 'majutsu-gerrit-upload)
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

(ert-deftest majutsu-gerrit-upload-transient/uses-native-review-readers ()
  "Review metadata readers should use Majutsu's native readers."
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
               '("feat/tramp\tgerrit" "" "main\tgerrit" "" "")))
            ((symbol-function 'majutsu--toplevel-safe)
             (lambda () "/repo")))
    (should (equal (majutsu-gerrit--remote-branch-names "gerrit")
                   '("feat/tramp" "main")))))

(ert-deftest majutsu-gerrit--remote-branch-names/dedupes-across-remotes ()
  "When no remote is given, should dedupe names from all remotes."
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _args)
               '("main\tgerrit" "dev\tgerrit" "main\torigin" "feat/tramp\torigin" "")))
            ((symbol-function 'majutsu--toplevel-safe)
             (lambda () "/repo")))
    (should (equal (majutsu-gerrit--remote-branch-names)
                   '("dev" "feat/tramp" "main")))))

(ert-deftest majutsu-gerrit--remote-branch-candidate-data/provides-source-metadata ()
  "Remote branch payload should preserve source remotes as metadata."
  (cl-letf (((symbol-function 'majutsu-jj-lines)
             (lambda (&rest _args)
               '("main\tgerrit" "dev\tgerrit" "main\torigin")))
            ((symbol-function 'majutsu--toplevel-safe)
             (lambda () "/repo")))
    (let* ((payload (majutsu-gerrit-remote-branch-candidate-data))
           (annotations (plist-get payload :annotations))
           (suffix-function (plist-get payload :annotation-suffix-function)))
      (should (eq (plist-get payload :category) 'majutsu-gerrit-remote-branch))
      (should (equal (plist-get payload :candidates) '("dev" "main")))
      (should (equal (gethash "main" annotations) "remotes: gerrit,origin"))
      (should (equal (gethash "dev" annotations) "remote: gerrit"))
      (should (functionp suffix-function))
      (should (string-match-p "remote branch" (funcall suffix-function "main")))
      (should (string-match-p "gerrit,origin" (funcall suffix-function "main"))))))

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
  (let (seen)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda () "/repo"))
              ((symbol-function 'transient-get-value)
               (lambda () '("--remote=gerrit" "--dry-run")))
              ((symbol-function 'majutsu-gerrit-remote-branch-candidate-data)
               (lambda (remote)
                 (setq seen remote)
                 '(:category majutsu-gerrit-remote-branch
                   :candidates ("main" "dev"))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection _predicate require-match _initial _hist _default)
                 (should-not require-match)
                 (should (equal collection '("main" "dev")))
                 "main")))
      (should (equal (majutsu-gerrit-upload--read-remote-branch
                      "Remote branch: " nil nil)
                     "main"))
      (should (equal seen "gerrit")))))

(ert-deftest majutsu-gerrit-upload-read-remote-branch/passes-branches-to-completing-read ()
  "Reader should present remote branch names with metadata."
  (let (seen-annotation seen-affixation seen-category seen-history)
    (let ((annotations (make-hash-table :test #'equal)))
      (puthash "main" "remotes: gerrit,origin" annotations)
      (cl-letf (((symbol-function 'majutsu--toplevel-safe)
                 (lambda () "/repo"))
                ((symbol-function 'transient-get-value)
                 (lambda () nil))
                ((symbol-function 'majutsu-gerrit-remote-branch-candidate-data)
                 (lambda (_remote)
                   (list :category 'majutsu-gerrit-remote-branch
                         :candidates '("main" "dev")
                         :annotations annotations)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection _predicate require-match _initial hist _default)
                   (should-not require-match)
                   (setq seen-history hist
                         seen-category (plist-get completion-extra-properties :category)
                         seen-annotation (funcall (plist-get completion-extra-properties
                                                             :annotation-function)
                                                  "main")
                         seen-affixation (plist-get completion-extra-properties
                                                    :affixation-function))
                   (should (equal collection '("main" "dev")))
                   "dev")))
        (majutsu-gerrit-upload--read-remote-branch "Remote branch: " nil nil)
        (should (eq seen-category 'majutsu-gerrit-remote-branch))
        (should (equal seen-annotation " remotes: gerrit,origin"))
        (should (functionp seen-affixation))
        (should (eq seen-history 'majutsu-gerrit-remote-branch-history))))))

(ert-deftest majutsu-gerrit-upload-read-remote-branch/allows-free-input ()
  "Reader should allow arbitrary input when no branch matches."
  (cl-letf (((symbol-function 'majutsu--toplevel-safe)
             (lambda () "/repo"))
            ((symbol-function 'transient-get-value)
             (lambda () nil))
            ((symbol-function 'majutsu-gerrit-remote-branch-candidate-data)
             (lambda (_remote)
               '(:category majutsu-gerrit-remote-branch
                 :candidates ("main" "dev"))))
            ((symbol-function 'completing-read)
             (lambda (_prompt collection _predicate require-match _initial _hist _default)
               (should-not require-match)
               (should (equal collection '("main" "dev")))
               "feature/new")))
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

(ert-deftest majutsu-gerrit--account-candidate/prefers-email-and-annotates ()
  (let* ((alice '((name . "Alice Example") (username . "alice") (email . "alice@example.com")))
         (bob '((name . "Bob Builder") (username . "bob")))
         (alice-cand (majutsu-gerrit--account-candidate alice))
         (bob-cand (majutsu-gerrit--account-candidate bob)))
    (should (equal alice-cand "alice@example.com"))
    (should (equal bob-cand "bob"))
    (should (equal (majutsu-gerrit--account-annotation alice alice-cand)
                   "Alice Example @alice"))
    (should (equal (majutsu-gerrit--account-annotation bob bob-cand)
                   "Bob Builder"))))

(ert-deftest majutsu-gerrit--account-transform/parses-json-candidates ()
  "Transform should parse Gerrit JSON body into candidate strings."
  (let* ((json "[{\"_account_id\": 1001, \"name\": \"Alice Example\", \"username\": \"alice\", \"email\": \"alice@example.com\"}]")
         (candidates (majutsu-gerrit--account-transform (list json))))
    (should (equal candidates '("alice@example.com")))
    (should (equal (get-text-property 0 'majutsu-gerrit-account (car candidates))
                   '((_account_id . 1001)
                     (name . "Alice Example")
                     (username . "alice")
                     (email . "alice@example.com"))))))

(ert-deftest majutsu-gerrit--account-transform/ignores-marker-lines ()
  "Transform should ignore the curl status marker line."
  (should (null (majutsu-gerrit--account-transform
                 '("MAJUTSU_GERRIT_STATUS:200\n")))))

(ert-deftest majutsu-gerrit--account-builder/uses-suggest-by-default ()
  "The async builder should request the suggest endpoint by default."
  (let (seen-url)
    (cl-letf (((symbol-function 'majutsu-gerrit-rest-current-spec)
               (lambda (&rest _args) '(:host "review" :scheme "https")))
              ((symbol-function 'majutsu-gerrit-rest--build-url)
               (lambda (_spec _endpoint params)
                 (setq seen-url params)
                 "https://review/accounts/"))
              ((symbol-function 'majutsu-gerrit-rest--curl-args)
               (lambda (&rest _args) '("https://review/accounts/"))))
      (let ((builder (majutsu-gerrit--account-builder "gerrit"))
            (default-directory "/repo"))
        (should (equal (funcall builder "ali") '("curl" "https://review/accounts/")))
        (should (assoc "suggest" seen-url))
        (should (equal (cdr (assoc "q" seen-url)) "ali"))
        (should (equal (cdr (assoc "n" seen-url)) 15))))))

(ert-deftest majutsu-gerrit--account-builder/uses-query-strategy ()
  "The async builder should request the query endpoint when configured."
  (let (seen-url)
    (cl-letf (((symbol-function 'majutsu-gerrit-rest-current-spec)
               (lambda (&rest _args) '(:host "review" :scheme "https")))
              ((symbol-function 'majutsu-gerrit-rest--build-url)
               (lambda (_spec _endpoint params)
                 (setq seen-url params)
                 "https://review/accounts/"))
              ((symbol-function 'majutsu-gerrit-rest--curl-args)
               (lambda (&rest _args) '("https://review/accounts/"))))
      (let* ((majutsu-gerrit-account-completion-strategy 'query)
             (builder (majutsu-gerrit--account-builder "gerrit"))
             (default-directory "/repo"))
        (funcall builder "ali")
        (should-not (assoc "suggest" seen-url))
        (should (equal (cdr (assoc "q" seen-url)) "ali"))
        (should (equal (cdr (assoc "o" seen-url)) "DETAILS"))))))

(ert-deftest majutsu-gerrit-topic-candidate-data/uses-rest-change-query ()
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-gerrit--remote-url)
               (lambda (&rest _args) "https://review.example.com/team/project"))
              ((symbol-function 'majutsu-gerrit-rest-current-spec)
               (lambda (&rest args)
                 (setq seen (plist-put seen :spec-args args))
                 '(:host "review" :scheme "https")))
              ((symbol-function 'majutsu-gerrit-rest-change-query)
               (lambda (&rest args)
                 (setq seen (plist-put seen :query-args args))
                 '(((topic . "alpha"))
                   ((topic . "alpha"))
                   ((topic . "beta"))))))
      (let* ((default-directory "/repo")
             (payload (majutsu-gerrit-topic-candidate-data "gerrit"))
             (annotations (plist-get payload :annotations)))
        (should (equal (plist-get seen :spec-args) '("gerrit")))
        (should (equal (plist-get seen :query-args)
                       '("is:open project:team/project" 100 nil nil
                         (:host "review" :scheme "https"))))
        (should (equal (plist-get payload :candidates)
                       '("alpha" "beta")))
        (should (equal (gethash "alpha" annotations)
                       "topic (2 open)"))))))

(ert-deftest majutsu-gerrit-upload-read-reviewer/uses-dynamic-account-reader ()
  (let (seen)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _dir) "/repo"))
              ((symbol-function 'transient-get-value)
               (lambda () '("--remote=gerrit")))
              ((symbol-function 'majutsu-gerrit-read-accounts)
               (lambda (&rest args)
                 (setq seen args)
                 '("alice@example.com"))))
      (should (equal (majutsu-gerrit-upload--read-reviewer
                      "Reviewer" nil nil)
                     '("alice@example.com")))
      (should (equal seen
                     '("Reviewer" nil majutsu-gerrit-reviewer-history
                       "gerrit"))))))

(ert-deftest majutsu-gerrit-upload-read-cc/uses-dynamic-account-reader ()
  (let (seen)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _dir) "/repo"))
              ((symbol-function 'transient-get-value)
               (lambda () '("--remote=gerrit")))
              ((symbol-function 'majutsu-gerrit-read-accounts)
               (lambda (&rest args)
                 (setq seen args)
                 '("bob@example.com"))))
      (should (equal (majutsu-gerrit-upload--read-cc "CC" nil nil)
                     '("bob@example.com")))
      (should (equal seen
                     '("CC" nil majutsu-gerrit-cc-history "gerrit"))))))

(ert-deftest majutsu-gerrit-upload-read-topic/uses-rest-payload-when-available ()
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

(ert-deftest majutsu-gerrit-upload-read-topic/falls-back-to-free-input ()
  (cl-letf (((symbol-function 'majutsu--toplevel-safe)
             (lambda (&optional _dir) "/repo"))
            ((symbol-function 'majutsu-gerrit-topic-candidate-data)
             (lambda (&rest _args) nil))
            ((symbol-function 'majutsu-read-string)
             (lambda (&rest _args) "topic")))
    (should (equal (majutsu-gerrit-upload--read-topic "Topic" nil nil)
                   "topic"))))

(ert-deftest majutsu-gerrit-upload-read-label/uses-free-multiple-input ()
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-completing-read-multiple)
               (lambda (&rest args)
                 (setq seen args)
                 '("Code-Review"))))
      (should (equal (majutsu-gerrit-upload--read-label
                      "Label" nil nil)
                     '("Code-Review")))
      (should (eq (nth 7 seen) 'majutsu-gerrit-label)))))

(provide 'majutsu-gerrit-test)
;;; majutsu-gerrit-test.el ends here
