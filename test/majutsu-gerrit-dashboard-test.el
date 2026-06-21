;;; majutsu-gerrit-dashboard-test.el --- Tests for Gerrit dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Unit tests for `majutsu-gerrit-dashboard'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'majutsu-gerrit-dashboard)

(defun majutsu-gerrit-dashboard-test--raw-change (&optional overrides)
  "Return a small raw ChangeInfo alist merged with OVERRIDES."
  (append overrides
          '((id . "majutsu~72")
            (_number . 72)
            (project . "team/project")
            (branch . "main")
            (change_id . "Iabc")
            (subject . "Prefer section-aware command defaults.")
            (status . "NEW")
            (topic . "gerrit")
            (owner . ((_account_id . 1000000)
                      (name . "Admin")))
            (total_comment_count . 3)
            (unresolved_comment_count . 1))))

(ert-deftest majutsu-gerrit-dashboard-fetch/preserves-query-groups ()
  "Repeated-q responses should stay grouped by dashboard query."
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-gerrit-rest-current-spec)
               (lambda (&rest args)
                 (setq seen (plist-put seen :spec-args args))
                 '(:host "review.example.com" :scheme "https" :path "" :prefix "/a")))
              ((symbol-function 'majutsu-gerrit-rest-change-query)
               (lambda (&rest args)
                 (setq seen (plist-put seen :query-args args))
                 (list (list (majutsu-gerrit-dashboard-test--raw-change))
                       (list (majutsu-gerrit-dashboard-test--raw-change
                              '((_number . 73) (subject . "Incoming"))))))))
      (let* ((groups (majutsu-gerrit-dashboard--fetch
                      '(("Mine" . "is:open owner:self")
                        ("Incoming" . "is:open reviewer:self"))
                      "gerrit" "/repo"))
             (first-change (cadar groups)))
        (should (equal (plist-get seen :spec-args) '("gerrit" "/repo")))
        (should (equal (car (plist-get seen :query-args))
                       '("is:open owner:self" "is:open reviewer:self")))
        (should (equal (majutsu-gerrit-change-number first-change) 72))
        (should (equal (majutsu-gerrit-change-subject first-change)
                       "Prefer section-aware command defaults."))))))

(ert-deftest majutsu-gerrit-dashboard-fetch/wraps-single-query-response ()
  "A single q response is a flat list and should be wrapped once."
  (cl-letf (((symbol-function 'majutsu-gerrit-rest-current-spec)
             (lambda (&rest _args) '(:host "review" :scheme "https")))
            ((symbol-function 'majutsu-gerrit-rest-change-query)
             (lambda (&rest _args)
               (list (majutsu-gerrit-dashboard-test--raw-change)))))
    (let ((groups (majutsu-gerrit-dashboard--fetch
                   '(("Mine" . "is:open owner:self")))))
      (should (= (length groups) 1))
      (should (= (length (cdar groups)) 1)))))

(ert-deftest majutsu-gerrit-dashboard-fetch/passes-fetch-parameters ()
  "Dashboard fetch should pass limit/start/options to the REST layer."
  (let (seen)
    (cl-letf (((symbol-function 'majutsu-gerrit-rest-current-spec)
               (lambda (&rest _args) '(:host "review" :scheme "https")))
              ((symbol-function 'majutsu-gerrit-rest-change-query)
               (lambda (&rest args)
                 (setq seen args)
                 (list (majutsu-gerrit-dashboard-test--raw-change)))))
      (majutsu-gerrit-dashboard--fetch
       '(("Mine" . "is:open owner:self")) nil nil 10 20
       '("LABELS" "SUBMIT_REQUIREMENTS"))
      (should (equal (nth 1 seen) 10))
      (should (equal (nth 2 seen) 20))
      (should (equal (nth 3 seen) '("LABELS" "SUBMIT_REQUIREMENTS"))))))

(ert-deftest majutsu-gerrit-dashboard-queries-from-args/builds-preset-foreach ()
  "Transient args should select presets and append foreach filters."
  (should (equal (majutsu-gerrit-dashboard--queries-from-args
                  '("--preset=outgoing" "--foreach=project:team/project"))
                 '(("Outgoing reviews"
                    . "is:open owner:self -is:wip project:team/project")))))

(ert-deftest majutsu-gerrit-dashboard-queries-from-args/custom-query-wins ()
  "Custom query should define a single dashboard section."
  (should (equal (majutsu-gerrit-dashboard--queries-from-args
                  '("--custom-query=is:open owner:self"
                    "--foreach=project:team/project"))
                 '(("Query" . "is:open owner:self project:team/project")))))

(ert-deftest majutsu-gerrit-dashboard-state/assigns-section-ids ()
  "Dashboard state should assign stable query section ids."
  (let* ((state (majutsu-gerrit-dashboard--state-create
                 "/repo" "gerrit" nil
                 '(("Mine" . "is:open owner:self")
                   ("Incoming" . "is:open reviewer:self"))
                 50 nil '("LABELS") nil))
         (sections (majutsu-gerrit-dashboard-state-sections state)))
    (should (equal (mapcar #'majutsu-gerrit-dashboard-query-id sections)
                   '(1 2)))
    (should (= (majutsu-gerrit-dashboard-state-next-id state) 3))))

(ert-deftest majutsu-gerrit-dashboard-repo-args/keeps-open-state-args ()
  "Repository defaults should keep args needed to recreate a dashboard."
  (should (equal (majutsu-gerrit-dashboard--repo-args
                  '("--remote=gerrit" "--custom-query=is:open" "--foreach=project:x"
                    "--start=25" "--limit=10" "--option=LABELS" "--title=Mine"
                    "--hide-empty" "--ignored=bad"))
                 '("--remote=gerrit" "--custom-query=is:open" "--foreach=project:x"
                   "--start=25" "--limit=10" "--option=LABELS" "--title=Mine"
                   "--hide-empty"))))

(ert-deftest majutsu-gerrit-query-completion/completes-operators ()
  "Gerrit query completion should complete operators at token start."
  (with-temp-buffer
    (insert "ow")
    (pcase-let ((`(,start ,end ,table . ,_) (majutsu-gerrit-query-completion-at-point)))
      (should (= start (point-min)))
      (should (= end (point)))
      (should (member "owner:" table)))))

(ert-deftest majutsu-gerrit-query-completion/completes-static-values ()
  "Gerrit query completion should complete operator values."
  (with-temp-buffer
    (insert "is:o")
    (pcase-let ((`(,start ,end ,table . ,_) (majutsu-gerrit-query-completion-at-point)))
      (should (= start (+ (point-min) 3)))
      (should (= end (point)))
      (should (member "open" table)))))

(ert-deftest majutsu-gerrit-query-completion/uses-gerrit-account-settings ()
  "Account completion should reuse Gerrit account completion settings."
  (let ((majutsu-gerrit-query--remote "gerrit")
        (majutsu-gerrit-account-completion-strategy 'suggest)
        (majutsu-gerrit-account-suggestion-limit 7)
        seen)
    (cl-letf (((symbol-function 'majutsu-gerrit-rest-current-spec)
               (lambda (&rest args)
                 (setq seen (plist-put seen :spec-args args))
                 '(:host "review.example.com" :scheme "https")))
              ((symbol-function 'majutsu-gerrit-rest-account-suggest)
               (lambda (&rest args)
                 (setq seen (plist-put seen :suggest-args args))
                 '(((email . "user@example.com"))))))
      (should (equal (majutsu-gerrit-query--account-candidates "us")
                     '("user@example.com")))
      (should (equal (plist-get seen :spec-args) '("gerrit")))
      (should (equal (plist-get seen :suggest-args)
                     '("us" 7 (:host "review.example.com" :scheme "https")))))))

(ert-deftest majutsu-gerrit-dashboard-render/inserts-change-sections ()
  "Dashboard refresh should render Gerrit changes as visitable sections."
  (let ((change (majutsu-gerrit-change-from-alist
                 (majutsu-gerrit-dashboard-test--raw-change))))
    (with-temp-buffer
      (majutsu-gerrit-dashboard-mode)
      (setq majutsu--default-directory "/repo"
            majutsu-gerrit-dashboard--state
            (majutsu-gerrit-dashboard--state-create
             "/repo" "gerrit" nil
             '(("Query" . "is:open owner:self"))
             50 nil '("LABELS") nil))
      (cl-letf (((symbol-function 'majutsu-gerrit-dashboard--fetch)
                 (lambda (sections &rest _args)
                   (list (cons (car sections) (list change))))))
        (let ((inhibit-read-only t))
          (majutsu-gerrit-dashboard-refresh-buffer))
        (should (string-match-p "Gerrit Dashboard" (buffer-string)))
        (should (string-match-p "Query (1)" (buffer-string)))
        (should (text-property-any (point-min) (point-max)
                                   'font-lock-face 'magit-hash))
        (should (text-property-any (point-min) (point-max)
                                   'font-lock-face 'success))
        (goto-char (point-min))
        (search-forward "Prefer section-aware command defaults.")
        (should (eq (magit-section-value-if 'majutsu-gerrit-change)
                    change))))))

(ert-deftest majutsu-gerrit-dashboard-render/marks-truncated-query ()
  "Dashboard headings should indicate Gerrit _more_changes."
  (let ((change (majutsu-gerrit-change-from-alist
                 (majutsu-gerrit-dashboard-test--raw-change
                  '((_more_changes . t))))))
    (with-temp-buffer
      (majutsu-gerrit-dashboard-mode)
      (setq majutsu--default-directory "/repo"
            majutsu-gerrit-dashboard--state
            (majutsu-gerrit-dashboard--state-create
             "/repo" "gerrit" nil
             '(("Query" . "is:open owner:self"))
             50 nil '("LABELS") nil))
      (cl-letf (((symbol-function 'majutsu-gerrit-dashboard--fetch)
                 (lambda (sections &rest _args)
                   (list (cons (car sections) (list change))))))
        (let ((inhibit-read-only t))
          (majutsu-gerrit-dashboard-refresh-buffer))
        (should (string-match-p (regexp-quote "Query (1+)")
                                (buffer-string)))))))

(ert-deftest majutsu-gerrit-dashboard-section/remove-from-change-section ()
  "Removing a section from a change row should remove its parent query."
  (let ((change (majutsu-gerrit-change-from-alist
                 (majutsu-gerrit-dashboard-test--raw-change))))
    (with-temp-buffer
      (majutsu-gerrit-dashboard-mode)
      (setq majutsu--default-directory "/repo"
            majutsu-gerrit-dashboard--state
            (majutsu-gerrit-dashboard--state-create
             "/repo" "gerrit" nil
             '(("Query" . "is:open owner:self"))
             50 nil '("LABELS") nil))
      (cl-letf (((symbol-function 'majutsu-gerrit-dashboard--fetch)
                 (lambda (sections &rest _args)
                   (list (cons (car sections) (list change))))))
        (let ((inhibit-read-only t))
          (majutsu-gerrit-dashboard-refresh-buffer)))
      (goto-char (point-min))
      (search-forward "Prefer section-aware command defaults.")
      (cl-letf (((symbol-function 'majutsu-refresh-buffer) #'ignore))
        (majutsu-gerrit-dashboard-remove-section))
      (should-not (majutsu-gerrit-dashboard-state-sections
                   majutsu-gerrit-dashboard--state))
      (should (majutsu-gerrit-dashboard-state-dirty-p
               majutsu-gerrit-dashboard--state)))))

(ert-deftest majutsu-gerrit-dashboard-change-web-url/uses-web-path ()
  "Change web URLs should point to Polygerrit, not the REST /a prefix."
  (let ((change (majutsu-gerrit-change-from-alist
                 (majutsu-gerrit-dashboard-test--raw-change))))
    (should (equal (majutsu-gerrit-dashboard--change-web-url
                    change '(:host "review.example.com"
                             :scheme "https"
                             :path "/gerrit"
                             :prefix "/a"))
                   "https://review.example.com/gerrit/c/team%2Fproject/+/72"))))

(ert-deftest majutsu-gerrit-dashboard-change-web-url/uses-shared-spec-path ()
  "Shared specs should keep their Gerrit web context path."
  (let ((change (majutsu-gerrit-change-from-alist
                 (majutsu-gerrit-dashboard-test--raw-change))))
    (should (equal (majutsu-gerrit-dashboard--change-web-url
                    change '(:gerrit-host "review.example.com"
                             :gerrit-prefix "/gerrit/a"
                             :ssl t))
                   "https://review.example.com/gerrit/c/team%2Fproject/+/72"))))

(provide 'majutsu-gerrit-dashboard-test)

;;; majutsu-gerrit-dashboard-test.el ends here
