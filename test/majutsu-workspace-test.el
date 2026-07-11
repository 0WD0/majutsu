;;; majutsu-workspace-test.el --- Tests for majutsu-workspace helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for parsing and small helpers in majutsu-workspace.el.

;;; Code:

(require 'ert)
(require 'majutsu-workspace)

(defun majutsu-workspace-test--row (&rest fields)
  "Return one structured workspace row for FIELDS."
  (concat (mapconcat (lambda (entry)
                       (pcase-let ((`(,field ,value) entry))
                         (if (plist-get (majutsu-workspace--field-spec field)
                                       :json)
                             (json-serialize value)
                           value)))
                     (cl-mapcar #'list
                                majutsu-workspace--default-fields
                                fields)
                     majutsu-workspace--field-separator)
          majutsu-workspace--field-separator))

(ert-deftest majutsu-workspace-template-plan/default-fields ()
  "The default workspace template plan should transport the default fields."
  (let ((plan (majutsu-workspace--ensure-template-plan)))
    (should (equal (plist-get plan :fields)
                   majutsu-workspace--default-fields))))

(ert-deftest majutsu-workspace-template-plan/uses-native-field-types ()
  "Identifiers stay native while user-controlled strings are encoded."
  (should
   (equal
    (plist-get (majutsu-workspace--ensure-template-plan) :template)
    (concat
     "concat(join(\"\\0\", "
     "if(self.target().current_working_copy(), \"@\"), "
     "label(\"name\", json(self.name())), "
     "self.target().change_id().shortest(8), "
     "self.target().commit_id().shortest(8), "
     "label(\"description first_line\", json(self.target().description().first_line())), "
     "label(\"root\", json(self.root()))), \"\\0\")"))))

(ert-deftest majutsu-workspace-parse-list-output/preserves-native-faces ()
  "Native template labels and the encoded description should keep faces."
  (let* ((sep majutsu-workspace--field-separator)
         (change-id (propertize "wnurqwps" 'font-lock-face 'change-id-face))
         (commit-id (propertize "6acd46b7" 'font-lock-face 'commit-id-face))
         (description (propertize "\"Main wc\""
                                  'font-lock-face 'description-face))
         (output (concat "@" sep "\"default\"" sep change-id sep commit-id sep
                         description sep "\"/tmp/main\"" sep))
         (entry (car (majutsu-workspace-parse-list-output output))))
    (should (eq (get-text-property 0 'font-lock-face
                                   (plist-get entry :change-id))
                'change-id-face))
    (should (eq (get-text-property 0 'font-lock-face
                                   (plist-get entry :commit-id))
                'commit-id-face))
    (should (eq (get-text-property 0 'font-lock-face
                                   (plist-get entry :desc))
                'description-face))))

(ert-deftest majutsu-workspace-format-entry/keeps-native-id-faces ()
  "Workspace rendering should not replace jj's native id highlighting."
  (let* ((change-id (propertize "wnurqwps" 'font-lock-face 'change-id-face))
         (commit-id (propertize "6acd46b7" 'font-lock-face 'commit-id-face))
         (rendered (majutsu-workspace--format-entry
                    (list :name "default" :current t
                          :change-id change-id :commit-id commit-id
                          :desc "Main" :root "/tmp/main/")
                    7)))
    (should (eq (get-text-property (string-match "wnurqwps" rendered)
                                   'font-lock-face rendered)
                'change-id-face))
    (should (eq (get-text-property (string-match "6acd46b7" rendered)
                                   'font-lock-face rendered)
                'commit-id-face))))

(ert-deftest majutsu-workspace-parse-list-output/basic ()
  "Parse structured `jj workspace list -T ...` output."
  (let* ((default-directory "/tmp/main/")
         (output (concat
                  (majutsu-workspace-test--row
                   "@" "default" "wnurqwps" "6acd46b7" "Main wc" "/tmp/main")
                  (majutsu-workspace-test--row
                   "" "w2" "lvolzxkz" "32e07e11" "" "/tmp/w2")))
         (entries (majutsu-workspace-parse-list-output output)))
    (should (equal (length entries) 2))
    (should (equal (plist-get (nth 0 entries) :name) "default"))
    (should (equal (plist-get (nth 0 entries) :current) t))
    (should (equal (plist-get (nth 0 entries) :change-id) "wnurqwps"))
    (should (equal (plist-get (nth 0 entries) :commit-id) "6acd46b7"))
    (should (equal (plist-get (nth 0 entries) :desc) "Main wc"))
    (should (equal (plist-get (nth 0 entries) :root) "/tmp/main/"))
    (should (equal (plist-get (nth 1 entries) :name) "w2"))
    (should-not (plist-get (nth 1 entries) :current))
    (should (equal (plist-get (nth 1 entries) :desc) ""))
    (should (equal (plist-get (nth 1 entries) :root) "/tmp/w2/"))))

(ert-deftest majutsu-workspace-parse-list-output/root-error-normalizes-to-nil ()
  "Workspace root template errors should normalize to nil."
  (let* ((sep majutsu-workspace--field-separator)
         (default-directory "/tmp/main/")
         (output (concat
                  "@" sep "\"default\"" sep "wnurqwps" sep
                  "6acd46b7" sep "\"Main wc\""
                  sep "<Error: Failed to resolve workspace root>" sep))
         (entries (majutsu-workspace-parse-list-output output)))
    (should (equal (length entries) 1))
    (should (null (plist-get (car entries) :root)))))

(ert-deftest majutsu-workspace-parse-list-output/root-preserves-remote-prefix ()
  "Structured workspace roots should keep the current TRAMP prefix."
  (let ((default-directory "/ssh:demo:/tmp/main/"))
    (let* ((output (majutsu-workspace-test--row
                    "@" "default" "wnurqwps" "6acd46b7" "Main wc"
                    "/home/demo/repo-main"))
           (entries (majutsu-workspace-parse-list-output output)))
      (should (equal (plist-get (car entries) :root)
                     "/ssh:demo:/home/demo/repo-main/")))))

(ert-deftest majutsu-workspace-parse-list-output/strips-ansi-escapes ()
  "Structured workspace parsing should tolerate ANSI-colored jj output."
  (let* ((sep majutsu-workspace--field-separator)
         (default-directory "/tmp/main/")
         (output (concat
                  "\x1b[1m@\x1b[0m" sep
                  "\x1b[35m\"default\"\x1b[0m" sep
                  "\x1b[36mwnurqwps\x1b[0m" sep
                  "\x1b[32m6acd46b7\x1b[0m" sep
                  "\x1b[33m\"Main wc\"\x1b[0m" sep
                  "\x1b[34m\"/tmp/main\"\x1b[0m" sep))
         (entries (majutsu-workspace-parse-list-output output)))
    (should (equal (length entries) 1))
    (should (equal (plist-get (car entries) :name) "default"))
    (should (plist-get (car entries) :current))
    (should (equal (plist-get (car entries) :change-id) "wnurqwps"))
    (should (equal (plist-get (car entries) :commit-id) "6acd46b7"))
    (should (equal (plist-get (car entries) :desc) "Main wc"))
    (should (equal (plist-get (car entries) :root) "/tmp/main/"))))

(ert-deftest majutsu-workspace-parse-list-output/preserves-control-characters ()
  "NUL framing must preserve legal path controls and terminal escapes."
  (let* ((default-directory "/tmp/main/")
         (root (concat "/tmp/line\nbreak" (string 30)
                       "tail" (string 27) "[31mRED" (string 27) "[0m"))
         (output (majutsu-workspace-test--row
                  "@" "odd" "wnurqwps" "6acd46b7" "description" root))
         (entries (majutsu-workspace-parse-list-output output)))
    (should (= (length entries) 1))
    (should (equal (plist-get (car entries) :root)
                   (file-name-as-directory root)))
    (should (equal (plist-get (car entries) :desc) "description"))))

(ert-deftest majutsu-workspace-parse-list-output/preserves-nul-description ()
  "Description encoding must keep NUL from becoming a record delimiter."
  (let* ((description (concat "before" (string 0) "after"))
         (output (majutsu-workspace-test--row
                  "@" "default" "wnurqwps" "6acd46b7"
                  description "/tmp/main"))
         (entry (car (majutsu-workspace-parse-list-output output))))
    (should (equal (plist-get entry :desc) description))))

(ert-deftest majutsu-workspace-parse-list-output/root-error-controls-do-not-split ()
  "An inline root error may contain path controls but not NUL framing."
  (let* ((sep majutsu-workspace--field-separator)
         (output (concat
                  "@" sep "\"default\"" sep "wnurqwps" sep "6acd46b7" sep
                  "\"Main\"" sep
                  "<Error: root /tmp/line\nbreak" (string 30) "tail>" sep))
         (entries (majutsu-workspace-parse-list-output output)))
    (should (= (length entries) 1))
    (should (equal (plist-get (car entries) :name) "default"))
    (should-not (plist-get (car entries) :root))))

(ert-deftest majutsu-workspace-parse-list-output/preserves-error-like-root ()
  "A valid root beginning with an error-like prefix must remain a path."
  (let* ((default-directory "/tmp/main/")
         (root "/tmp/<Error: literal>/")
         (entries
          (majutsu-workspace-parse-list-output
           (majutsu-workspace-test--row
            "@" "default" "wnurqwps" "6acd46b7" "Main" root))))
    (should (equal (plist-get (car entries) :root) root))))

(ert-deftest majutsu-workspace--names/uses-structured-list-entries ()
  "Workspace names should be derived from structured entries."
  (cl-letf (((symbol-function 'majutsu-workspace-list-entries)
             (lambda (&optional _directory)
               '((:name "ws-a")
                 (:name "ws-b")
                 (:name "ws-a")))))
    (should (equal (majutsu-workspace--names)
                   '("ws-a" "ws-b")))))

(ert-deftest majutsu-workspace-current-name/uses-structured-list-entries ()
  "Current workspace name should be derived from structured entries."
  (cl-letf (((symbol-function 'majutsu-workspace-list-entries)
             (lambda (&optional _directory)
               '((:name "ws-a" :current nil)
                 (:name "ws-b" :current t)))))
    (should (equal (majutsu-workspace-current-name) "ws-b"))))

(ert-deftest majutsu-workspace-candidate-data/builds-entry-map ()
  "Workspace candidate payload should preserve names and structured entries."
  (cl-letf (((symbol-function 'majutsu-workspace-list-entries)
             (lambda (&optional _directory)
               '((:name "ws-a" :current t :root "/tmp/ws-a/" :change-id "abc12345" :desc "Main")
                 (:name "ws-b" :current nil :root "/tmp/ws-b/")))))
    (let* ((payload (majutsu-workspace-candidate-data))
           (entries (plist-get payload :entries))
           (suffix-function (plist-get payload :annotation-suffix-function)))
      (should (equal (plist-get payload :candidates) '("ws-a" "ws-b")))
      (should (equal (plist-get (gethash "ws-a" entries) :root) "/tmp/ws-a/"))
      (should-not (plist-get (gethash "ws-b" entries) :current))
      (should (functionp suffix-function))
      (should (string-match-p "current" (funcall suffix-function "ws-a")))
      (should (string-match-p "abc12345" (funcall suffix-function "ws-a")))
      (should (string-match-p "Main" (funcall suffix-function "ws-a"))))))

(ert-deftest majutsu-workspace-candidate-data/completion-suffix-captures-root-context ()
  "Workspace completion suffixes should keep sibling-relative root labels."
  (let ((payload
         (let ((default-directory "/tmp/parent/main/"))
           (cl-letf (((symbol-function 'majutsu-workspace-list-entries)
                      (lambda (&optional _directory)
                        '((:name "main" :current t :root "/tmp/parent/main/")
                          (:name "feature" :current nil :root "/tmp/parent/feature/")))))
             (majutsu-workspace-candidate-data)))))
    (let* ((default-directory "/tmp/unrelated/")
           (suffix-function (plist-get payload :annotation-suffix-function))
           (suffix (funcall suffix-function "feature")))
      (should (string-match-p "workspace" suffix))
      (should (string-match-p "feature" suffix))
      (should-not (string-match-p "/tmp/parent/feature" suffix)))))

(ert-deftest majutsu-workspace-read-name/uses-history-and-category ()
  "Workspace name reader should expose dedicated history and category."
  (let (seen-history seen-category)
    (cl-letf (((symbol-function 'magit-section-value-if)
               (lambda (&rest _args) nil))
              ((symbol-function 'majutsu-workspace-candidate-data)
               (lambda (&optional _root)
                 (let ((entries (make-hash-table :test #'equal))
                       (entry-list '((:name "ws-a" :current t)
                                     (:name "ws-b" :current nil))))
                   (puthash "ws-a" '(:name "ws-a" :current t) entries)
                   (puthash "ws-b" '(:name "ws-b" :current nil) entries)
                   (list :candidates '("ws-a" "ws-b")
                         :entry-list entry-list
                         :entries entries))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection _predicate _require-match _initial history _default)
                 (setq seen-history history
                       seen-category (plist-get completion-extra-properties :category))
                 (should (equal collection '("ws-a" "ws-b")))
                 "ws-b")))
      (should (equal (majutsu-workspace--read-name "Workspace") "ws-b"))
      (should (eq seen-history 'majutsu-workspace-name-history))
      (should (eq seen-category 'majutsu-workspace)))))

(ert-deftest majutsu-workspace-visit/binds-default-directory ()
  "Ensure visiting another workspace updates buffer context."
  (let ((new-dir (make-temp-file "majutsu-test-" t))
        seen-default
        seen-root)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'majutsu-refresh)
                     (lambda ()
                       (setq seen-default default-directory)
                       (setq seen-root majutsu--default-directory))))
            (let ((default-directory "/tmp/"))
              (majutsu-workspace-visit new-dir)))
          (should (equal seen-default (file-name-as-directory (expand-file-name new-dir))))
          (should (equal seen-root (file-name-as-directory (expand-file-name new-dir)))))
      (delete-directory new-dir t))))

;;; Wash tests

(ert-deftest majutsu-workspace-wash-list-transforms-buffer ()
  "Structured wash should transform the buffer into workspace sections."
  (let* ((sep majutsu-workspace--field-separator)
         (lines (concat
                 (majutsu-workspace-test--row
                  "@" "default" "wqps1234" "6acd46b7" "Main"
                  "/workspaces/default")
                 (majutsu-workspace-test--row
                  "" "feature" "lvolzxkz" "32e07e11" "Feature work"
                  "/workspaces/feature")))
         (default-directory "/tmp/test-repo/")
         (majutsu--default-directory "/tmp/test-repo/"))
    (with-temp-buffer
      (require 'magit-section)
      (magit-section-mode)
      (setq buffer-read-only nil)
      (magit-insert-section (workspaces)
        (insert lines)
        (goto-char (point-min))
        (majutsu-workspace--wash-list t nil)
        (should-not (string-match-p sep (buffer-string)))
        (should (string-match-p "Workspaces" (buffer-string)))
        (should (string-match-p "default" (buffer-string)))
        (should (string-match-p "feature" (buffer-string)))
        (should (string-match-p "/workspaces/default" (buffer-string)))
        (should (string-match-p "/workspaces/feature" (buffer-string)))))))

(ert-deftest majutsu-workspace-wash-list-renders-missing-root-placeholder ()
  "Missing structured roots should render as a fixed placeholder."
  (let* ((sep majutsu-workspace--field-separator)
         (lines (concat "@" sep "\"default\"" sep "wqps1234" sep
                        "6acd46b7" sep "\"Main\""
                        sep "<Error: Failed to resolve workspace root>" sep))
         (default-directory "/tmp/test-repo/")
         (majutsu--default-directory "/tmp/test-repo/"))
    (with-temp-buffer
      (require 'magit-section)
      (magit-section-mode)
      (setq buffer-read-only nil)
      (magit-insert-section (workspaces)
        (insert lines)
        (goto-char (point-min))
        (majutsu-workspace--wash-list t nil)
        (should (string-match-p "default" (buffer-string)))
        (should (string-match-p "-" (buffer-string)))))))

(ert-deftest majutsu-workspace-wash-list-hides-single-workspace ()
  "Wash-list should hide a single workspace when show-single is nil."
  (let ((line (majutsu-workspace-test--row
               "@" "default" "wqps1234" "6acd46b7" "Main"
               "/workspaces/default")))
    (with-temp-buffer
      (require 'magit-section)
      (magit-section-mode)
      (setq buffer-read-only nil)
      (magit-insert-section (workspaces)
        (insert line)
        (goto-char (point-min))
        (majutsu-workspace--wash-list nil nil)
        (should (member (buffer-string) '("" "(empty)\n")))))))

;;; Root-for-name tests

(ert-deftest majutsu-workspace--root-for-name/returns-directory ()
  "Test that root-for-name returns a directory path from jj output."
  (cl-letf (((symbol-function 'majutsu-jj-buffer-string)
             (lambda (&rest _args) "/home/user/repo-secondary\n")))
    (should (equal (majutsu-workspace--root-for-name "secondary")
                   "/home/user/repo-secondary/"))))

(ert-deftest majutsu-workspace--root-for-name/preserves-remote-prefix ()
  "Workspace roots discovered on TRAMP should keep remote host prefix."
  (let ((default-directory "/ssh:demo:/tmp/"))
    (cl-letf (((symbol-function 'file-remote-p)
               (lambda (path &optional identification _connected)
                 (when (and (equal path default-directory)
                            (null identification))
                   "/ssh:demo:")))
              ((symbol-function 'majutsu-jj-buffer-string)
               (lambda (&rest _args) "/home/demo/repo-secondary\n")))
      (should (equal (majutsu-workspace--root-for-name "secondary")
                     "/ssh:demo:/home/demo/repo-secondary/")))))

(ert-deftest majutsu-workspace--root-for-name/returns-nil-on-error ()
  "Test that root-for-name returns nil when jj fails (no output)."
  (cl-letf (((symbol-function 'majutsu-jj-buffer-string)
             (lambda (&rest _args) "")))
    (should (null (majutsu-workspace--root-for-name "nonexistent")))))

(ert-deftest majutsu-workspace--root-for-name/preserves-embedded-newline ()
  "Root lookup removes only jj's record terminator, not path newlines."
  (cl-letf (((symbol-function 'majutsu-jj-buffer-string)
             (lambda (&rest _args) "/tmp/line\nbreak\n")))
    (should (equal (majutsu-workspace--root-for-name "odd")
                   "/tmp/line\nbreak/"))))

(ert-deftest majutsu-workspace--read-root/prefers-section-root ()
  "Read-root should reuse a structured root from the current section first."
  (cl-letf (((symbol-function 'majutsu-workspace--section-entry)
             (lambda () '(:name "secondary" :root "/tmp/secondary/")))
            ((symbol-function 'majutsu-workspace--root-for-name)
             (lambda (&rest _args)
               (ert-fail "should not consult jj workspace root when section root exists"))))
    (should (equal (majutsu-workspace--read-root "secondary")
                   "/tmp/secondary/"))))

(ert-deftest majutsu-workspace--read-root/uses-jj-workspace-root ()
  "Test that read-root delegates to jj workspace root --name."
  (cl-letf (((symbol-function 'majutsu-workspace--section-entry)
             (lambda () nil))
            ((symbol-function 'majutsu-workspace--root-for-name)
             (lambda (name)
               (when (equal name "secondary") "/tmp/secondary/"))))
    (should (equal (majutsu-workspace--read-root "secondary")
                   "/tmp/secondary/"))))

(ert-deftest majutsu-workspace--read-root/preserves-remote-prefix ()
  "Read-root should keep remote host prefix when using absolute localname."
  (let ((root "/ssh:demo:/tmp/main/"))
    (cl-letf (((symbol-function 'majutsu-workspace--section-entry)
               (lambda () nil))
              ((symbol-function 'file-remote-p)
               (lambda (path &optional identification _connected)
                 (when (and (equal path root)
                            (null identification))
                   "/ssh:demo:")))
              ((symbol-function 'majutsu-workspace--root-for-name)
               (lambda (_name) "/ssh:demo:/home/demo/repo-secondary/")))
      (should (equal (majutsu-workspace--read-root "secondary" root)
                     "/ssh:demo:/home/demo/repo-secondary/")))))

(ert-deftest majutsu-workspace--sibling-root/finds-matching-sibling ()
  "Test that sibling-root finds a sibling dir that is the named workspace."
  (let* ((parent (make-temp-file "majutsu-test-" t))
         (root (file-name-as-directory (expand-file-name "main" parent)))
         (sibling (file-name-as-directory (expand-file-name "feature" parent))))
    (unwind-protect
        (progn
          (make-directory root t)
          (make-directory sibling t)
          (cl-letf (((symbol-function 'majutsu-toplevel)
                     (lambda (&optional dir) dir))
                    ((symbol-function 'majutsu-workspace-current-name)
                     (lambda (&optional dir)
                       (when (string= (file-name-as-directory dir) sibling)
                         "feature"))))
            (should (equal (majutsu-workspace--sibling-root "feature" root)
                           sibling))))
      (delete-directory parent t))))

(ert-deftest majutsu-workspace--sibling-root/returns-nil-when-no-match ()
  "Test that sibling-root returns nil when no matching sibling exists."
  (let* ((parent (make-temp-file "majutsu-test-" t))
         (root (file-name-as-directory (expand-file-name "main" parent))))
    (unwind-protect
        (progn
          (make-directory root t)
          (cl-letf (((symbol-function 'majutsu-toplevel)
                     (lambda (&optional _dir) nil))
                    ((symbol-function 'majutsu-workspace-current-name)
                     (lambda (&optional _dir) nil)))
            (should (null (majutsu-workspace--sibling-root "nonexistent" root)))))
      (delete-directory parent t))))

(ert-deftest majutsu-workspace--read-root/falls-back-to-sibling ()
  "Test that read-root uses sibling-root when root-for-name fails."
  (cl-letf (((symbol-function 'majutsu-workspace--section-entry)
             (lambda () nil))
            ((symbol-function 'majutsu-workspace--root-for-name)
             (lambda (_name) nil))
            ((symbol-function 'majutsu-workspace--sibling-root)
             (lambda (name _root)
               (when (equal name "feature") "/tmp/feature/"))))
    (should (equal (majutsu-workspace--read-root "feature")
                   "/tmp/feature/"))))

(ert-deftest majutsu-workspace-add/uses-local-destination-for-jj ()
  "Workspace add should pass a local destination path to remote jj.
The Emacs-facing path remains unchanged for visiting the new workspace."
  (let (seen-args seen-visit)
    (cl-letf (((symbol-function 'majutsu-convert-filename-for-jj)
               (lambda (_path) "/tmp/feature"))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq seen-args args)
                 0))
              ((symbol-function 'majutsu-workspace-visit)
               (lambda (dir)
                 (setq seen-visit dir))))
      (majutsu-workspace-add "/ssh:demo:/tmp/feature")
      (should (equal seen-args '("workspace" "add" "/tmp/feature")))
      (should (equal seen-visit (expand-file-name "/ssh:demo:/tmp/feature"))))))

(ert-deftest majutsu-workspace-forget/forgets-without-trash-by-default ()
  "Normal workspace forget should not resolve or delete workspace roots."
  (let (seen-action seen-args)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory)
                 (ert-fail "non-trash forget should not resolve repo root")))
              ((symbol-function 'majutsu-workspace-list-entries)
               (lambda (&optional _directory)
                 (ert-fail "non-trash forget should not list workspace roots")))
              ((symbol-function 'majutsu-confirm)
               (lambda (action _prompt)
                 (setq seen-action action)
                 t))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq seen-args args)
                 0))
              ((symbol-function 'delete-directory)
               (lambda (&rest _args)
                 (ert-fail "non-trash forget should not delete directories"))))
      (majutsu-workspace-forget '("feature"))
      (should (eq seen-action 'workspace-forget))
      (should (equal seen-args '("workspace" "forget" "feature"))))))

(ert-deftest majutsu-workspace-forget/interactive-ignores-trash-args-outside-transient ()
  "Direct interactive forget should ignore stale saved transient arguments."
  (let ((transient-current-command nil)
        seen-action seen-args)
    (cl-letf (((symbol-function 'majutsu-workspace--names)
               (lambda (&optional _directory) '("feature")))
              ((symbol-function 'majutsu-completing-read-multiple)
               (lambda (&rest _args) '("feature")))
              ((symbol-function 'transient-args)
               (lambda (&rest _args) '("--trash")))
              ((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory)
                 (ert-fail "inactive transient --trash should not resolve roots")))
              ((symbol-function 'majutsu-confirm)
               (lambda (action _prompt)
                 (setq seen-action action)
                 t))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq seen-args args)
                 0))
              ((symbol-function 'delete-directory)
               (lambda (&rest _args)
                 (ert-fail "inactive transient --trash should not delete"))))
      (call-interactively #'majutsu-workspace-forget)
      (should (eq seen-action 'workspace-forget))
      (should (equal seen-args '("workspace" "forget" "feature"))))))

(ert-deftest majutsu-workspace-forget/trashes-directories-when-requested ()
  "Workspace forget should optionally trash workspace directories after jj succeeds."
  (let ((feature (make-temp-file "majutsu-feature-" t))
        seen-args deleted events)
    (unwind-protect
        (cl-letf (((symbol-function 'majutsu--toplevel-safe)
                   (lambda (&optional _directory) "/tmp/main/"))
                  ((symbol-function 'majutsu-workspace-list-entries)
                   (lambda (&optional _directory)
                     `((:name "feature" :root ,(file-name-as-directory feature)))))
                  ((symbol-function 'majutsu-confirm)
                   (lambda (action _prompt)
                     (should (eq action 'workspace-trash))
                     t))
                  ((symbol-function 'majutsu-run-jj)
                   (lambda (&rest args)
                     (push 'run-jj events)
                     (setq seen-args args)
                     0))
                  ((symbol-function 'delete-directory)
                   (lambda (directory recursive trash)
                     (should (memq 'run-jj events))
                     (push 'delete-directory events)
                     (push (list directory recursive trash delete-by-moving-to-trash)
                           deleted))))
          (majutsu-workspace-forget '("feature") t)
          (should (equal seen-args '("workspace" "forget" "feature")))
          (should (equal (nreverse events) '(run-jj delete-directory)))
          (should (equal deleted
                         `((,(file-name-as-directory feature) t t t)))))
      (when (file-directory-p feature)
        (delete-directory feature t)))))

(ert-deftest majutsu-workspace-forget/trashes-exact-control-character-root ()
  "Trash must receive the complete root, not a line-truncated prefix."
  (let* ((parent (make-temp-file "majutsu-control-root-" t))
         (odd (file-name-as-directory
               (expand-file-name "line\nbreak" parent)))
         prompt deleted)
    (make-directory odd)
    (unwind-protect
        (cl-letf (((symbol-function 'majutsu--toplevel-safe)
                   (lambda (&optional _directory) "/tmp/main/"))
                  ((symbol-function 'majutsu-workspace-list-entries)
                   (lambda (&optional _directory)
                     `((:name "odd" :root ,odd))))
                  ((symbol-function 'majutsu-confirm)
                   (lambda (_action text)
                     (setq prompt text)
                     t))
                  ((symbol-function 'majutsu-run-jj)
                   (lambda (&rest _args) 0))
                  ((symbol-function 'delete-directory)
                   (lambda (directory &rest _args)
                     (setq deleted directory))))
          (majutsu-workspace-forget '("odd") t)
          (should (equal deleted odd))
          (should (string-match-p "\\\\n" prompt)))
      (delete-directory parent t))))

(ert-deftest majutsu-workspace-forget/refuses-to-trash-repo-store-root ()
  "Trash flow should not delete a root that owns the shared .jj/repo store."
  (let* ((primary (make-temp-file "majutsu-primary-" t))
         (repo-store (expand-file-name ".jj/repo" primary))
         ran-jj deleted)
    (unwind-protect
        (progn
          (make-directory repo-store t)
          (cl-letf (((symbol-function 'majutsu--toplevel-safe)
                     (lambda (&optional _directory) "/tmp/main/"))
                    ((symbol-function 'majutsu-workspace-list-entries)
                     (lambda (&optional _directory)
                       `((:name "default" :root ,(file-name-as-directory primary)))))
                    ((symbol-function 'majutsu-confirm)
                     (lambda (action _prompt)
                       (should (eq action 'workspace-trash))
                       t))
                    ((symbol-function 'majutsu-run-jj)
                     (lambda (&rest _args)
                       (setq ran-jj t)
                       0))
                    ((symbol-function 'delete-directory)
                     (lambda (&rest _args)
                       (setq deleted t))))
            (should-error (majutsu-workspace-forget '("default") t)
                          :type 'user-error)
            (should-not ran-jj)
            (should-not deleted)))
      (when (file-directory-p primary)
        (delete-directory primary t)))))

(ert-deftest majutsu-workspace-forget/refuses-to-trash-remote-root-before-io ()
  "Trash flow must never turn a remote root into recursive deletion."
  (let (confirmed ran-jj deleted touched-remote)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/tmp/main/"))
              ((symbol-function 'majutsu-workspace-list-entries)
               (lambda (&optional _directory)
                 '((:name "remote" :root "/ssh:demo:/tmp/remote/"))))
              ((symbol-function 'file-exists-p)
               (lambda (path)
                 (when (file-remote-p path)
                   (setq touched-remote t))
                 nil))
              ((symbol-function 'majutsu-confirm)
               (lambda (&rest _args) (setq confirmed t) t))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest _args) (setq ran-jj t) 0))
              ((symbol-function 'delete-directory)
               (lambda (&rest _args) (setq deleted t))))
      (should-error (majutsu-workspace-forget '("remote") t)
                    :type 'user-error)
      (should-not touched-remote)
      (should-not confirmed)
      (should-not ran-jj)
      (should-not deleted))))

(ert-deftest majutsu-workspace-forget/reports-trash-failure-after-forget ()
  "A trash failure must say that jj metadata already changed."
  (let ((feature (make-temp-file "majutsu-feature-" t))
        ran-jj condition)
    (unwind-protect
        (cl-letf (((symbol-function 'majutsu--toplevel-safe)
                   (lambda (&optional _directory) "/tmp/main/"))
                  ((symbol-function 'majutsu-workspace-list-entries)
                   (lambda (&optional _directory)
                     `((:name "feature" :root ,(file-name-as-directory feature)))))
                  ((symbol-function 'majutsu-confirm)
                   (lambda (&rest _args) t))
                  ((symbol-function 'majutsu-run-jj)
                   (lambda (&rest _args) (setq ran-jj t) 0))
                  ((symbol-function 'delete-directory)
                   (lambda (&rest _args) (error "trash unavailable"))))
          (setq condition
                (should-error (majutsu-workspace-forget '("feature") t)
                              :type 'user-error))
          (should ran-jj)
          (let ((message (error-message-string condition)))
            (should (string-match-p "forget succeeded" message))
            (should (string-match-p "not moved to trash" message))
            (should (string-match-p "Move them manually" message))))
      (when (file-directory-p feature)
        (delete-directory feature t)))))

(ert-deftest majutsu-workspace-forget/binds-directory-identity-across-confirmation ()
  "A replacement at the confirmed path must not be trashed after forget."
  (let* ((parent (make-temp-file "majutsu-identity-" t))
         (feature (expand-file-name "feature" parent))
         (displaced (expand-file-name "confirmed-feature" parent))
         condition)
    (make-directory feature)
    (unwind-protect
        (cl-letf (((symbol-function 'majutsu--toplevel-safe)
                   (lambda (&optional _directory) "/tmp/main/"))
                  ((symbol-function 'majutsu-workspace-list-entries)
                   (lambda (&optional _directory)
                     `((:name "feature"
                        :root ,(file-name-as-directory feature)))))
                  ((symbol-function 'majutsu-confirm)
                   (lambda (&rest _args) t))
                  ((symbol-function 'majutsu-run-jj)
                   (lambda (&rest _args)
                     (rename-file feature displaced)
                     (make-directory feature)
                     0)))
          (setq condition
                (should-error (majutsu-workspace-forget '("feature") t)
                              :type 'user-error))
          (should (file-directory-p feature))
          (should (file-directory-p displaced))
          (should (string-match-p "identity changed after confirmation"
                                  (error-message-string condition))))
      (delete-directory parent t))))

(ert-deftest majutsu-workspace-forget/continues-and-reports-all-trash-failures ()
  "Every eligible root is attempted and all trash errors are reported together."
  (let ((one (make-temp-file "majutsu-one-" t))
        (two (make-temp-file "majutsu-two-" t))
        attempts condition)
    (unwind-protect
        (cl-letf (((symbol-function 'majutsu--toplevel-safe)
                   (lambda (&optional _directory) "/tmp/main/"))
                  ((symbol-function 'majutsu-workspace-list-entries)
                   (lambda (&optional _directory)
                     `((:name "one" :root ,(file-name-as-directory one))
                       (:name "two" :root ,(file-name-as-directory two)))))
                  ((symbol-function 'majutsu-confirm)
                   (lambda (&rest _args) t))
                  ((symbol-function 'majutsu-run-jj)
                   (lambda (&rest _args) 0))
                  ((symbol-function 'delete-directory)
                   (lambda (directory &rest _args)
                     (push directory attempts)
                     (error "trash failed for %s" directory))))
          (setq condition
                (should-error (majutsu-workspace-forget '("one" "two") t)
                              :type 'user-error))
          (should (equal (nreverse attempts)
                         (list (file-name-as-directory one)
                               (file-name-as-directory two))))
          (should (string-match-p "one" (error-message-string condition)))
          (should (string-match-p "two" (error-message-string condition))))
      (delete-directory one t)
      (delete-directory two t))))

(ert-deftest majutsu-workspace-forget/trash-with-missing-root-does-not-prompt ()
  "Trash flow should forget but not prompt or delete when root is unavailable."
  (let (seen-args deleted)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/tmp/main/"))
              ((symbol-function 'majutsu-workspace-list-entries)
               (lambda (&optional _directory)
                 '((:name "gone" :root nil))))
              ((symbol-function 'majutsu-workspace--root-for-name)
               (lambda (&rest _args) nil))
              ((symbol-function 'majutsu-workspace--sibling-root)
               (lambda (&rest _args) nil))
              ((symbol-function 'majutsu-workspace--read-root)
               (lambda (&rest _args)
                 (ert-fail "trash flow should not prompt through read-root")))
              ((symbol-function 'read-directory-name)
               (lambda (&rest _args)
                 (ert-fail "trash flow should not prompt for a directory")))
              ((symbol-function 'majutsu-confirm)
               (lambda (action _prompt)
                 (should (eq action 'workspace-trash))
                 t))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq seen-args args)
                 0))
              ((symbol-function 'delete-directory)
               (lambda (&rest _args)
                 (setq deleted t))))
      (majutsu-workspace-forget '("gone") t)
      (should (equal seen-args '("workspace" "forget" "gone")))
      (should-not deleted))))

(ert-deftest majutsu-workspace-forget/does-not-trash-when-jj-fails ()
  "Workspace directories should not be removed when forget fails."
  (let ((feature (make-temp-file "majutsu-feature-" t))
        deleted)
    (unwind-protect
        (cl-letf (((symbol-function 'majutsu--toplevel-safe)
                   (lambda (&optional _directory) "/tmp/main/"))
                  ((symbol-function 'majutsu-workspace-list-entries)
                   (lambda (&optional _directory)
                     `((:name "feature" :root ,(file-name-as-directory feature)))))
                  ((symbol-function 'majutsu-confirm)
                   (lambda (&rest _args) t))
                  ((symbol-function 'majutsu-run-jj)
                   (lambda (&rest _args) 1))
                  ((symbol-function 'delete-directory)
                   (lambda (&rest _args)
                     (setq deleted t))))
          (majutsu-workspace-forget '("feature") t)
          (should-not deleted))
      (when (file-directory-p feature)
        (delete-directory feature t)))))

(ert-deftest majutsu-workspace/integration-control-root-with-minimum-jj ()
  "Exercise NUL-framed root transport against MAJUTSU_TEST_JJ."
  (let ((jj (getenv "MAJUTSU_TEST_JJ")))
    (skip-unless (and jj (file-executable-p jj)))
    (let* ((parent (make-temp-file "majutsu-workspace-integration-" t))
           (repo (expand-file-name "repo" parent))
           (odd-root (concat (expand-file-name "line\nbreak" parent)
                             (string 30)
                             "tail"
                             (string 27) "[31mRED" (string 27) "[0m")))
      (unwind-protect
          (progn
            (should (zerop (call-process jj nil nil nil "git" "init" repo)))
            (let ((default-directory (file-name-as-directory repo))
                  (majutsu-jj-executable jj))
              (should (zerop (call-process jj nil nil nil
                                           "workspace" "add" odd-root
                                           "--name" "odd")))
              (let ((entry (cl-find "odd" (majutsu-workspace-list-entries)
                                    :key (lambda (item)
                                           (plist-get item :name))
                                    :test #'equal)))
                (should entry)
                (should (equal (plist-get entry :root)
                               (file-name-as-directory odd-root))))
              (should (equal (majutsu-workspace--root-for-name "odd")
                             (file-name-as-directory odd-root)))
              ;; A root() error includes the unresolved path verbatim.  Its
              ;; newline and record-separator characters must not corrupt the
              ;; surrounding NUL-framed workspace records.
              (delete-directory odd-root t)
              (let ((entry (cl-find "odd" (majutsu-workspace-list-entries)
                                    :key (lambda (item)
                                           (plist-get item :name))
                                    :test #'equal)))
                (should entry)
                (should-not (plist-get entry :root)))))
        (delete-directory parent t)))))

(provide 'majutsu-workspace-test)
;;; majutsu-workspace-test.el ends here
