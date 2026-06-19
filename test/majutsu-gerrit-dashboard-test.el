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

(ert-deftest majutsu-gerrit-dashboard-render/inserts-change-sections ()
  "Dashboard refresh should render Gerrit changes as visitable sections."
  (let ((change (majutsu-gerrit-change-from-alist
                 (majutsu-gerrit-dashboard-test--raw-change))))
    (with-temp-buffer
      (majutsu-gerrit-dashboard-mode)
      (setq majutsu-gerrit-dashboard--remote "gerrit"
            majutsu-gerrit-dashboard--queries '(("Mine" . "is:open owner:self"))
            majutsu--default-directory "/repo")
      (cl-letf (((symbol-function 'majutsu-gerrit-dashboard--fetch)
                 (lambda (&rest _args)
                   (list (cons '("Mine" . "is:open owner:self")
                               (list change))))))
        (let ((inhibit-read-only t))
          (majutsu-gerrit-dashboard-refresh-buffer))
        (should (string-match-p "Gerrit Dashboard" (buffer-string)))
        (should (string-match-p "Mine (1)" (buffer-string)))
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
      (setq majutsu-gerrit-dashboard--remote "gerrit"
            majutsu-gerrit-dashboard--queries '(("Mine" . "is:open owner:self"))
            majutsu--default-directory "/repo")
      (cl-letf (((symbol-function 'majutsu-gerrit-dashboard--fetch)
                 (lambda (&rest _args)
                   (list (cons '("Mine" . "is:open owner:self")
                               (list change))))))
        (let ((inhibit-read-only t))
          (majutsu-gerrit-dashboard-refresh-buffer))
        (should (string-match-p (regexp-quote "Mine (1+)")
                                (buffer-string)))))))

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
