;;; majutsu-org-test.el --- Tests for Majutsu Org links -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for the `majutsu' and `majutsu-rev' link types.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'eieio)
(require 'majutsu-org)

(defmacro majutsu-org-test--with-buffer (root revision &rest body)
  "Run BODY in a Majutsu buffer with ROOT and REVISION at point.
`org-link-store-props' records the stored plist in
`org-store-link-plist' and returns it, like the real implementation."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (delay-mode-hooks (majutsu-mode))
     (cl-letf (((symbol-function 'majutsu--buffer-root)
                (lambda (&optional _buffer) ,root))
               ((symbol-function 'majutsu-revision-at-point)
                (lambda () ,revision))
               ((symbol-function 'org-link-store-props)
                (lambda (&rest plist)
                  (setq org-store-link-plist plist)
                  plist)))
       ,@body)))

(ert-deftest majutsu-org-setup/registers-link-types-and-remap ()
  (should (org-link-get-parameter "majutsu" :follow))
  (should (org-link-get-parameter "majutsu" :complete))
  (should (org-link-get-parameter "majutsu-rev" :store))
  (should (eq (lookup-key majutsu-mode-map [remap org-store-link])
              #'majutsu-org-store-link)))

(ert-deftest majutsu-org-store-repository/without-revision ()
  (majutsu-org-test--with-buffer "/repo/" nil
    (should (majutsu-org-store-repository))
    (should (equal (plist-get org-store-link-plist :type) "majutsu"))
    (should (equal (plist-get org-store-link-plist :link) "majutsu:/repo/"))))

(ert-deftest majutsu-org-store-repository/available-beside-revision ()
  (majutsu-org-test--with-buffer "/repo/" "abc123"
    (should (majutsu-org-store-repository))
    (should (equal (plist-get org-store-link-plist :link) "majutsu:/repo/"))))

(ert-deftest majutsu-org-store-revision/stores-revision-at-point ()
  (majutsu-org-test--with-buffer "/repo/" "abc123"
    (should (majutsu-org-store-revision))
    (should (equal (plist-get org-store-link-plist :type) "majutsu-rev"))
    (should (equal (plist-get org-store-link-plist :link)
                   "majutsu-rev:/repo/::abc123"))))

(ert-deftest majutsu-org-store-link/prefers-revision-over-repository ()
  "When both links are available, Org offers the revision first."
  (majutsu-org-test--with-buffer "/repo/" "abc123"
    (let (choices default)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest args)
                   (setq choices collection
                         default (nth 4 args))
                   (symbol-name (car collection)))))
        (org-link--try-link-store-functions t))
      (should (equal choices '(majutsu-org-store-revision
                               majutsu-org-store-repository)))
      (should (equal default "majutsu-org-store-revision")))))

(ert-deftest majutsu-org-store-link/stores-one-link-per-selected-section ()
  (majutsu-org-test--with-buffer "/repo/" nil
    (let ((inhibit-read-only t)
          (org-stored-links nil)
          (sections (list (magit-section) (magit-section))))
      (insert "a\nb\nc\n")
      (oset (car sections) start 1)
      (oset (cadr sections) start 2)
      (cl-letf (((symbol-function 'magit-region-sections)
                 (lambda (&rest _args) sections))
                ((symbol-function 'majutsu-revision-at-point)
                 (lambda () (format "rev%d" (point))))
                ((symbol-function 'deactivate-mark) (lambda (&optional _) nil)))
        (majutsu-org-store-link))
      (should (equal (mapcar #'car org-stored-links)
                     '("majutsu-rev:/repo/::rev2"
                       "majutsu-rev:/repo/::rev1"))))))

(ert-deftest majutsu-org-open-repository/opens-log-for-directory ()
  (let (directory)
    (cl-letf (((symbol-function 'majutsu-log)
               (lambda (&optional dir) (setq directory dir))))
      (majutsu-org-open-repository "/repo/"))
    (should (equal directory "/repo/"))))

(ert-deftest majutsu-org-open-revision/diffs-revision-in-repository ()
  (let (revision directory)
    (cl-letf (((symbol-function 'majutsu-diff-revset)
               (lambda (rev &rest _args)
                 (setq revision rev
                       directory default-directory))))
      (majutsu-org-open-revision "/repo/::abc123"))
    (should (equal revision "abc123"))
    (should (equal directory "/repo/"))))

(provide 'majutsu-org-test)
;;; majutsu-org-test.el ends here
