;;; majutsu-gerrit-download-test.el --- Tests for Gerrit download -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Unit tests for `majutsu-gerrit-download'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-gerrit-download)

(defun majutsu-gerrit-download-test--change (&optional change-id)
  "Return a Gerrit change with optional CHANGE-ID footer."
  (majutsu-gerrit-change-from-alist
   `((id . "team%2Fproject~72")
     (_number . 72)
     (project . "team/project")
     (change_id . ,(or change-id "Iabc"))
     (subject . "Download me")
     (current_revision . "deadbeef")
     (revisions . ((deadbeef . ((_number . 3)
                                (ref . "refs/changes/72/72/3"))))))))

(ert-deftest majutsu-gerrit-download/edits-local-change-when-uploaded-here ()
  "A change uploaded from this repo should resolve to its local jj change."
  (let* ((jj-id "onwqukrrrnmxskuptxpyzyxzonksuoql")
         (gerrit-id "Ibc395f888cd27f5a62a10120bcf75b9e6a6a6964")
         (change (majutsu-gerrit-download-test--change gerrit-id))
         run)
    (cl-letf (((symbol-function 'majutsu-jj-string)
               (lambda (&rest args)
                 (should (member jj-id args))
                 jj-id))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args) (setq run args) 0))
              ((symbol-function 'majutsu-refresh) #'ignore)
              ((symbol-function 'majutsu-gerrit-rest-current-spec)
               (lambda (&rest _) (ert-fail "should not hit REST"))))
      (majutsu-gerrit-download-change change "/repo")
      (should (equal run (list "edit" jj-id))))))

(ert-deftest majutsu-gerrit-download/fetches-foreign-change ()
  "A change not uploaded from this repo should be fetched and checked out."
  (let ((change (majutsu-gerrit-download-test--change "Iabc"))
        calls)
    (cl-letf (((symbol-function 'majutsu-jj-string)
               (lambda (&rest args)
                 (cond ((member "root" args) "/repo/.git")
                       (t (error "Revision doesn't exist")))))
              ((symbol-function 'majutsu-gerrit-download--remote)
               (lambda (&rest _) "gerrit"))
              ((symbol-function 'majutsu-gerrit-rest-current-spec)
               (lambda (&rest _) '(:host "review" :scheme "https")))
              ((symbol-function 'majutsu-refresh) #'ignore)
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args) (push args calls) 0)))
      (majutsu-gerrit-download-change change "/repo")
      (should (equal (nreverse calls)
                     '(("git" "--git-dir" "/repo/.git" "fetch" "gerrit"
                        "+refs/changes/72/72/3:refs/remotes/gerrit/majutsu/gerrit/72/3")
                       ("git" "import")
                       ("new" "majutsu/gerrit/72/3@gerrit")))))))

(provide 'majutsu-gerrit-download-test)

;;; majutsu-gerrit-download-test.el ends here
