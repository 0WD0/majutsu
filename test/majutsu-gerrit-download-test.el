;;; majutsu-gerrit-download-test.el --- Tests for Gerrit download -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Unit tests for `majutsu-gerrit-download'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-gerrit-download)

(defun majutsu-gerrit-download-test--change (&optional with-revision)
  "Return a Gerrit change test object.
When WITH-REVISION is non-nil, include the current revision ref."
  (majutsu-gerrit-change-from-alist
   (append
    '((id . "team%2Fproject~72")
      (_number . 72)
      (project . "team/project")
      (change_id . "Iabc")
      (subject . "Download me"))
    (when with-revision
      '((current_revision . "deadbeef")
        (revisions . ((deadbeef . ((_number . 3)
                                   (ref . "refs/changes/72/72/3"))))))))))

(ert-deftest majutsu-gerrit-download-refspec/targets-remote-bookmark ()
  "Patchset refs should be fetched into jj-visible remote bookmark refs."
  (should (equal (majutsu-gerrit-download--fetch-refspec
                  "refs/changes/72/72/3" "gerrit" "majutsu/gerrit/72/3")
                 "+refs/changes/72/72/3:refs/remotes/gerrit/majutsu/gerrit/72/3"))
  (should (equal (majutsu-gerrit-download--revset
                  "gerrit" "majutsu/gerrit/72/3")
                 "majutsu/gerrit/72/3@gerrit")))

(ert-deftest majutsu-gerrit-download-start-git-fetch/uses-underlying-git-dir ()
  "The Git fetch should target the underlying Git store, not the cwd."
  (let (seen callback-ran)
    (cl-letf (((symbol-function 'majutsu-gerrit-download--git-dir)
               (lambda () "/repo/.git"))
              ((symbol-function 'majutsu-start-process)
               (lambda (program input &rest args)
                 (setq seen (list program input args))
                 (make-symbol "process")))
              ((symbol-function 'process-put)
               (lambda (process key value)
                 (put process key value))))
      (let ((process (majutsu-gerrit-download--start-git-fetch
                      "gerrit" "refs/changes/72/72/3" "majutsu/gerrit/72/3"
                      (lambda () (setq callback-ran t)))))
        (should (equal seen
                       '("git" nil ("--git-dir" "/repo/.git" "fetch" "gerrit"
                                    "+refs/changes/72/72/3:refs/remotes/gerrit/majutsu/gerrit/72/3"))))
        (funcall (get process 'finish-callback) process 0)
        (should callback-ran)))))

(ert-deftest majutsu-gerrit-download-current-revision/uses-existing-revision ()
  "A ChangeInfo with CURRENT_REVISION should not need a detail fetch."
  (let ((change (majutsu-gerrit-download-test--change t)))
    (cl-letf (((symbol-function 'majutsu-gerrit-rest-change-get)
               (lambda (&rest _args)
                 (ert-fail "unexpected detail fetch"))))
      (let ((revision (majutsu-gerrit-download--ensure-current-revision change)))
        (should (equal (majutsu-gerrit-revision-ref revision)
                       "refs/changes/72/72/3"))))))

(ert-deftest majutsu-gerrit-download-current-revision/fetches-detail-when-needed ()
  "A lightweight dashboard ChangeInfo should be detailed before download."
  (let ((change (majutsu-gerrit-download-test--change nil))
        seen-id seen-options)
    (cl-letf (((symbol-function 'majutsu-gerrit-rest-change-get)
               (lambda (change-id options &optional _spec)
                 (setq seen-id change-id
                       seen-options options)
                 '((id . "team%2Fproject~72")
                   (_number . 72)
                   (current_revision . "deadbeef")
                   (revisions . ((deadbeef . ((_number . 3)
                                              (ref . "refs/changes/72/72/3")))))))))
      (let ((revision (majutsu-gerrit-download--ensure-current-revision change)))
        (should (equal seen-id 72))
        (should (equal seen-options '("CURRENT_REVISION")))
        (should (equal (majutsu-gerrit-revision-number revision) 3))))))

(ert-deftest majutsu-gerrit-download-change/chains-fetch-import-new ()
  "Download should fetch the patchset, import it, then create a jj child."
  (let ((change (majutsu-gerrit-download-test--change nil))
        calls)
    (cl-letf (((symbol-function 'majutsu-gerrit-rest-current-spec)
               (lambda (&rest _args) '(:host "review" :scheme "https")))
              ((symbol-function 'majutsu-gerrit-rest-change-get)
               (lambda (&rest _args)
                 '((id . "team%2Fproject~72")
                   (_number . 72)
                   (current_revision . "deadbeef")
                   (revisions . ((deadbeef . ((_number . 3)
                                              (ref . "refs/changes/72/72/3"))))))))
              ((symbol-function 'majutsu-gerrit-download--start-git-fetch)
               (lambda (remote source-ref bookmark callback)
                 (push (list :fetch remote source-ref bookmark) calls)
                 (funcall callback)
                 'git-process))
              ((symbol-function 'majutsu-gerrit-download--start-import)
               (lambda (callback)
                 (push '(:import) calls)
                 (funcall callback)
                 'jj-import-process))
              ((symbol-function 'majutsu-gerrit-download--start-new)
               (lambda (remote bookmark)
                 (push (list :new remote bookmark) calls)
                 'jj-new-process)))
      (should (eq (majutsu-gerrit-download-change change "gerrit" "/repo")
                  'git-process))
      (should (equal (nreverse calls)
                     '((:fetch "gerrit" "refs/changes/72/72/3" "majutsu/gerrit/72/3")
                       (:import)
                       (:new "gerrit" "majutsu/gerrit/72/3")))))))

(provide 'majutsu-gerrit-download-test)

;;; majutsu-gerrit-download-test.el ends here
