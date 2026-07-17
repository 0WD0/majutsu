;;; majutsu-jj-integration.el --- Shared helpers for real-jj ERT tests  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Helpers for Majutsu integration tests that exercise a real `jj' binary.
;;
;; Tests are gated on the environment variable `MAJUTSU_TEST_JJ', which must
;; name an executable.  Pinning a binary keeps CI and local runs on a known
;; minimum jj version instead of whatever happens to be on PATH.
;;
;; Typical shapes:
;;
;;   (majutsu-jj-integration-with-repo repo
;;     (majutsu-jj-integration-write-text repo "a.txt" "hi\n")
;;     (majutsu-jj-integration-commit repo "base")
;;     ...)
;;
;;   (majutsu-jj-integration-with-sandbox sandbox
;;     (let ((a (majutsu-jj-integration-init sandbox "a"))
;;           (b (majutsu-jj-integration-init sandbox "b")))
;;       ...))

;;; Code:

(require 'cl-lib)
(require 'ert)

(defvar majutsu-jj-integration--jj nil
  "jj executable bound while a sandbox/repo macro is active.")

(defun majutsu-jj-integration-executable ()
  "Return the pinned jj binary for integration tests, or nil."
  (let ((jj (getenv "MAJUTSU_TEST_JJ")))
    (and jj (file-executable-p jj) jj)))

(defun majutsu-jj-integration--require-jj ()
  "Return the active or pinned jj binary, or fail the test."
  (or majutsu-jj-integration--jj
      (majutsu-jj-integration-executable)
      (ert-fail "MAJUTSU_TEST_JJ is unset or not executable")))

(defmacro majutsu-jj-integration-with-sandbox (sandbox &rest body)
  "Skip unless a pinned jj is available, then run BODY with SANDBOX bound.
SANDBOX is a temporary directory that is deleted after BODY.  Inside BODY,
`majutsu-jj-integration--jj' is bound to the pinned executable."
  (declare (indent 1) (debug (symbol body)))
  (let ((jj (make-symbol "jj")))
    `(let ((,jj (majutsu-jj-integration-executable)))
       (skip-unless ,jj)
       (let ((,sandbox (make-temp-file "majutsu-jj-" t))
             (majutsu-jj-integration--jj ,jj))
         (unwind-protect
             (progn ,@body)
           (when (file-directory-p ,sandbox)
             (delete-directory ,sandbox t)))))))

(defmacro majutsu-jj-integration-with-repo (repo &rest body)
  "Like `majutsu-jj-integration-with-sandbox', but also initialize one repo.
REPO is bound to the repository path.  BODY runs with `default-directory'
and `majutsu-jj-executable' pointed at that repository."
  (declare (indent 1) (debug (symbol body)))
  (let ((sandbox (make-symbol "sandbox")))
    `(majutsu-jj-integration-with-sandbox ,sandbox
      (let* ((,repo (majutsu-jj-integration-init ,sandbox))
             (default-directory (file-name-as-directory ,repo))
             (majutsu-jj-executable majutsu-jj-integration--jj))
        ,@body))))

(defun majutsu-jj-integration-init (sandbox &optional name)
  "Create a jj git-colocated repository under SANDBOX.
NAME defaults to \"repo\".  Return the repository path."
  (let* ((jj (majutsu-jj-integration--require-jj))
         (repo (expand-file-name (or name "repo") sandbox)))
    (majutsu-jj-integration--check
     (call-process jj nil nil nil "git" "init" repo)
     "git init" repo)
    repo))

(defun majutsu-jj-integration-file (repo relative)
  "Return absolute path of RELATIVE inside REPO."
  (expand-file-name relative (file-name-as-directory repo)))

(defun majutsu-jj-integration--check (status command repo)
  "Fail the current test unless STATUS is zero for COMMAND in REPO."
  (unless (zerop status)
    (ert-fail (format "jj %s failed in %s (exit %s)" command repo status)))
  status)

(defun majutsu-jj-integration-call (repo &rest args)
  "Run jj ARGS in REPO and fail the test on non-zero exit.
Return the exit status (always 0 on success)."
  (let ((default-directory (file-name-as-directory repo))
        (jj (majutsu-jj-integration--require-jj)))
    (majutsu-jj-integration--check
     (apply #'call-process jj nil nil nil args)
     (mapconcat #'identity args " ")
     repo)))

(defun majutsu-jj-integration-output (repo &rest args)
  "Run jj ARGS in REPO, return stdout, and fail the test on non-zero exit."
  (let ((default-directory (file-name-as-directory repo))
        (jj (majutsu-jj-integration--require-jj)))
    (with-temp-buffer
      (majutsu-jj-integration--check
       (apply #'call-process jj nil (current-buffer) nil args)
       (mapconcat #'identity args " ")
       repo)
      (buffer-string))))

(defun majutsu-jj-integration-commit (repo message)
  "Create a commit in REPO with MESSAGE."
  (majutsu-jj-integration-call repo "commit" "-m" message))

(defun majutsu-jj-integration-write-bytes (path bytes)
  "Write BYTES literally to PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (let ((coding-system-for-write 'binary))
      (write-region (point-min) (point-max) path nil 'silent))))

(defun majutsu-jj-integration-read-bytes (path)
  "Return PATH contents as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun majutsu-jj-integration-write-text (path text)
  "Write TEXT to PATH."
  (with-temp-file path
    (insert text)))

(defun majutsu-jj-integration-run-replay (repo command args plan)
  "Run jj COMMAND with interactive applypatch PLAN in REPO.
ARGS are extra command arguments before the injected tool flags."
  (require 'majutsu-interactive)
  (let ((directory (make-temp-file "majutsu-tool-" t)))
    (unwind-protect
        (let* ((patch (majutsu-interactive--write-patch
                       (or (plist-get plan :patch) "") directory))
               (config (majutsu-interactive--build-tool-config
                        patch plan directory)))
          (apply #'majutsu-jj-integration-call repo
                 (append (list command "-i" "--tool" "majutsu-applypatch")
                         args
                         config)))
      (ignore-errors (delete-directory directory t)))))

(defun majutsu-jj-integration-with-washed-diff (repo fn)
  "Wash REPO's current Git-format diff and call FN in that buffer.
FN is called with no arguments after sections and file metadata are attached.
Returns FN's value.  Requires Majutsu diff helpers."
  (require 'majutsu-diff)
  (require 'majutsu-interactive)
  (let* ((default-directory (file-name-as-directory repo))
         (majutsu-jj-executable (majutsu-jj-integration--require-jj))
         (git-output (majutsu-jj-buffer-string "diff" "--git"))
         (metadata (majutsu-diff--query-file-metadata nil nil)))
    (with-temp-buffer
      (majutsu-diff-mode)
      (let ((inhibit-read-only t)
            (magit-section-inhibit-markers t)
            (majutsu-diff--active-file-metadata metadata)
            (majutsu-diff--file-metadata-queue (copy-sequence metadata)))
        (magit-insert-section (root)
          (insert git-output)
          (save-restriction
            (narrow-to-region (point-min) (point-max))
            (majutsu-diff-wash-diffs '("--git")))))
      (majutsu-diff--attach-file-metadata metadata)
      (funcall fn))))

(provide 'majutsu-jj-integration)
;;; majutsu-jj-integration.el ends here
