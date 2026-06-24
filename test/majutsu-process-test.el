;;; majutsu-process-test.el --- Tests for majutsu-process  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for process helpers and filters.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-process)

(ert-deftest test-majutsu-process-error-summary-from-string/error ()
  (should (equal (majutsu--process-error-summary-from-string "Error: something went wrong\n")
                 "something went wrong")))

(ert-deftest test-majutsu-process-error-summary-from-string/error-lowercase ()
  (should (equal (majutsu--process-error-summary-from-string "error: nope\n")
                 "nope")))

(ert-deftest test-majutsu-process-error-summary-from-string/fatal ()
  (should (equal (majutsu--process-error-summary-from-string "fatal: bad\n")
                 "bad")))

(ert-deftest test-majutsu-process-error-summary-from-string/multi-line ()
  (let ((out (string-join '("some output"
                            "warning: ignore this"
                            "Error: actual issue"
                            "")
                          "\n")))
    (should (equal (majutsu--process-error-summary-from-string out)
                   "actual issue"))))

(ert-deftest test-majutsu-process-error-summary-from-string/empty ()
  (should-not (majutsu--process-error-summary-from-string "")))

(ert-deftest test-majutsu-process-filter/calls-magit-prompt ()
  (let ((called nil))
    (cl-letf (((symbol-function 'magit-process-password-prompt)
               (lambda (_proc _string)
                 (setq called 'password))))
      (with-temp-buffer
        (let ((proc (make-process :name "majutsu-test"
                                  :buffer (current-buffer)
                                  :command (list "cat"))))
          (set-marker (process-mark proc) (point))
          (majutsu--process-filter proc "Password: ")
          (delete-process proc)))
      (should (eq called 'password)))))

(ert-deftest test-majutsu-process-filter/ignores-with-editor-control-packets ()
  (with-temp-buffer
    (let* ((packet (format "WITH-EDITOR: 123 OPEN +1%c/tmp/manifest%c IN /tmp\n"
                           ?\x1f ?\x1f))
           (proc (make-process :name "majutsu-test"
                               :buffer (current-buffer)
                               :command (list "cat"))))
      (set-marker (process-mark proc) (point))
      (majutsu--process-filter proc (concat "normal output\n" packet "tail\n"))
      (delete-process proc)
      (let ((out (buffer-string)))
        (should (string-match-p (regexp-quote "normal output\ntail\n") out))
        (should-not (string-match-p "WITH-EDITOR:" out))))))

(ert-deftest test-majutsu-process-filter/ignores-split-with-editor-packets ()
  (with-temp-buffer
    (let* ((part1 "WITH-EDITOR: 123 OPEN +1")
           (part2 (format "%c/tmp/manifest%c IN /tmp\n"
                          ?\x1f ?\x1f))
           (proc (make-process :name "majutsu-test"
                               :buffer (current-buffer)
                               :command (list "cat"))))
      (set-marker (process-mark proc) (point))
      (majutsu--process-filter proc "normal\n")
      (majutsu--process-filter proc part1)
      (majutsu--process-filter proc part2)
      (majutsu--process-filter proc "tail\n")
      (delete-process proc)
      (let ((out (buffer-string)))
        (should (string-match-p (regexp-quote "normal\ntail\n") out))
        (should-not (string-match-p "WITH-EDITOR:" out))))))

(ert-deftest test-majutsu-process-filter/remembers-with-editor-file-root ()
  (let ((majutsu-process--with-editor-file-roots (make-hash-table :test #'equal)))
    (with-temp-buffer
      (let* ((packet (format "WITH-EDITOR: 123 OPEN +1%c/tmp/manifest%c IN /repo\n"
                             ?\x1f ?\x1f))
             (proc (make-process :name "majutsu-test"
                                 :buffer (current-buffer)
                                 :command (list "cat"))))
        (process-put proc 'default-dir "/repo/")
        (set-marker (process-mark proc) (point))
        (majutsu--process-filter proc packet)
        (delete-process proc)
        (should (equal (majutsu-process-with-editor-file-root "/tmp/manifest")
                       "/repo/"))))))

(ert-deftest test-majutsu-process-filter/dispatches-majutsu-ediff-control-packets ()
  (with-temp-buffer
    (let* ((packet (format "MAJUTSU-EDIFF: 123 DIFF /tmp/left%c/tmp/right%cfoo.txt\n"
                           ?\x1f ?\x1f))
           (proc (make-process :name "majutsu-test"
                               :buffer (current-buffer)
                               :command (list "cat")))
           seen)
      (cl-letf (((symbol-function 'majutsu-ediff--handle-control-line)
                 (lambda (_proc line)
                   (setq seen line)
                   t)))
        (set-marker (process-mark proc) (point))
        (majutsu--process-filter proc (concat "normal\n" packet "tail\n"))
        (delete-process proc)
        (let ((out (buffer-string)))
          (should (equal seen (substring packet 0 -1)))
          (should (string-match-p (regexp-quote "normal\ntail\n") out))
          (should-not (string-match-p "MAJUTSU-EDIFF:" out)))))))

(ert-deftest test-majutsu-process-filter/dispatches-split-majutsu-ediff-packets ()
  (with-temp-buffer
    (let* ((part1 (format "MAJUTSU-EDIFF: 123 DIFF /tmp/left%c" ?\x1f))
           (part2 (format "/tmp/right%cfoo.txt\n" ?\x1f))
           (proc (make-process :name "majutsu-test"
                               :buffer (current-buffer)
                               :command (list "cat")))
           seen)
      (cl-letf (((symbol-function 'majutsu-ediff--handle-control-line)
                 (lambda (_proc line)
                   (setq seen line)
                   t)))
        (set-marker (process-mark proc) (point))
        (majutsu--process-filter proc "normal\n")
        (majutsu--process-filter proc part1)
        (majutsu--process-filter proc part2)
        (majutsu--process-filter proc "tail\n")
        (delete-process proc)
        (let ((out (buffer-string)))
          (should (equal seen (substring (concat part1 part2) 0 -1)))
          (should (string-match-p (regexp-quote "normal\ntail\n") out))
          (should-not (string-match-p "MAJUTSU-EDIFF:" out)))))))

(ert-deftest majutsu-process-test-start-jj-binds-root-cwd ()
  "`majutsu-start-jj' should run from repo root."
  (let ((default-directory "/repo/sub/")
        seen-root)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-start-process)
               (lambda (&rest _args)
                 (setq seen-root default-directory)
                 'dummy-process)))
      (should (eq (majutsu-start-jj '("status")) 'dummy-process))
      (should (equal seen-root "/repo/")))))

(ert-deftest majutsu-process-test-call-jj-runs-at-root ()
  "`majutsu-call-jj' should execute jj in repo root.
The process section should use root as command directory."
  (let ((default-directory "/repo/sub/")
        seen-process-cwd
        seen-finish-root
        seen-prefix-pwd
        seen-args)
    (with-temp-buffer
      (let ((process-buf (current-buffer)))
        (setq default-directory "/repo/")
        (cl-letf (((symbol-function 'majutsu--toplevel-safe)
                   (lambda (&optional _directory) "/repo/"))
                  ((symbol-function 'majutsu-process-jj-arguments)
                   (lambda (args) args))
                  ((symbol-function 'majutsu-process-buffer)
                   (lambda (&optional _nodisplay)
                     process-buf))
                  ((symbol-function 'majutsu--process-insert-section)
                   (lambda (pwd _program _args &optional _err _errlog _face)
                     (setq seen-prefix-pwd pwd)
                     (insert "\n")
                     'dummy-section))
                  ((symbol-function 'majutsu--call-process-responsive)
                   (lambda (_program _process-buf _section root &rest args)
                     (setq seen-process-cwd default-directory)
                     (setq seen-finish-root root)
                     (setq seen-args args)
                     0))
                  ((symbol-function 'majutsu-process-finish)
                   (lambda (exit _process-buf _command-buf default-dir _section)
                     (setq seen-finish-root default-dir)
                     exit)))
          (should (= 0 (majutsu-call-jj "status"))))))
    (should (equal seen-prefix-pwd "/repo/"))
    (should (equal seen-process-cwd "/repo/"))
    (should (equal seen-finish-root "/repo/"))
    (should (equal seen-args '("status")))))

(ert-deftest majutsu-process-test-start-jj-uses-process-environment-helper ()
  "`majutsu-start-jj' should apply helper environment via start-process."
  (let ((default-directory "/repo/sub/")
        (process-environment (cons "COLUMNS=80" process-environment))
        process
        seen-columns
        seen-env-args)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/tmp/"))
              ((symbol-function 'majutsu-process-jj-arguments)
               (lambda (args) args))
              ((symbol-function 'majutsu-process-environment)
               (lambda (args)
                 (setq seen-env-args args)
                 (cons "COLUMNS=123" process-environment)))
              ((symbol-function 'start-file-process)
               (lambda (name buffer _program &rest _args)
                 (setq seen-columns (getenv "COLUMNS"))
                 (make-process :name (format "%s-test" name)
                               :buffer buffer
                               :command (list "cat"))))
              ((symbol-function 'majutsu--process-display-buffer)
               (lambda (_process) nil)))
      (unwind-protect
          (progn
            (setq process (majutsu-start-jj '("diff" "--stat")))
            (should (processp process))
            (should (equal seen-env-args '("diff" "--stat")))
            (should (equal seen-columns "123")))
        (when (and process (process-live-p process))
          (delete-process process))
        (let ((buf (and process (process-buffer process))))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

(ert-deftest majutsu-process-test-responsive-call-uses-process-environment-helper ()
  "`majutsu--call-process-responsive' should bind helper environment."
  (let ((process-environment (cons "COLUMNS=80" process-environment))
        seen-env-args
        seen-columns
        process)
    (with-temp-buffer
      (let ((process-buf (current-buffer)))
        (cl-letf (((symbol-function 'majutsu-process-environment)
                   (lambda (args)
                     (setq seen-env-args args)
                     (cons "COLUMNS=234" process-environment)))
                  ((symbol-function 'start-file-process)
                   (lambda (name buffer _program &rest _args)
                     (setq seen-columns (getenv "COLUMNS"))
                     (setq process
                           (make-process :name (format "%s-test" name)
                                         :buffer buffer
                                         :command (list "true")))))
                  ((symbol-function 'majutsu--process-display-buffer)
                   (lambda (_process) nil)))
          (unwind-protect
              (should (= 0 (majutsu--call-process-responsive
                            "jj" process-buf 'dummy-section "/repo/"
                            "diff" "--stat")))
            (when (and process (process-live-p process))
              (delete-process process))))))
    (should (equal seen-env-args '("diff" "--stat")))
    (should (equal seen-columns "234"))))

(ert-deftest majutsu-process-test-responsive-call-suppresses-default-sentinel-output ()
  "`majutsu--call-process-responsive' should not pollute its output buffer."
  (with-temp-buffer
    (let ((process-buf (current-buffer)))
      (cl-letf (((symbol-function 'majutsu-process-environment)
                 (lambda (_args) process-environment))
                ((symbol-function 'majutsu--process-display-buffer)
                 (lambda (_process) nil)))
        (should (= 0 (majutsu--call-process-responsive
                      "true" process-buf 'dummy-section "/repo/")))
        ;; Give any pending sentinel notification one more chance to run.
        (accept-process-output nil 0.05)
        (should-not (string-match-p "Process .*\\(?:finished\\|exited\\)"
                                    (buffer-string)))))))

(ert-deftest majutsu-process-test-process-wait-drains-output ()
  "`majutsu--process-wait' should loop on `accept-process-output' until drained."
  (let ((results '(t t nil))
        calls)
    (cl-letf (((symbol-function 'accept-process-output)
               (lambda (&rest args)
                 (push args calls)
                 (pop results)))
              ((symbol-function 'process-exit-status)
               (lambda (_process) 0)))
      (should (= 0 (majutsu--process-wait 'fake-process))))
    (should (equal (nreverse calls)
                   '((fake-process) (fake-process) (fake-process))))))

(ert-deftest majutsu-process-test-process-wait-drains-stderr-process ()
  "`majutsu--process-wait' should also drain a standard error process."
  (let ((seen nil))
    (cl-letf (((symbol-function 'accept-process-output)
               (lambda (process &rest _args)
                 (push process seen)
                 nil))
              ((symbol-function 'process-exit-status)
               (lambda (_process) 0)))
      (should (= 0 (majutsu--process-wait 'main 'stderr))))
    (should (equal (nreverse seen) '(main stderr)))))

(ert-deftest majutsu-process-test-process-wait-enables-local-quit ()
  "`majutsu--process-wait' should not block with quitting inhibited."
  (let ((seen-inhibit-quit t))
    (cl-letf (((symbol-function 'accept-process-output)
               (lambda (&rest _args)
                 (setq seen-inhibit-quit inhibit-quit)
                 nil))
              ((symbol-function 'process-exit-status)
               (lambda (_process) 0)))
      (let ((inhibit-quit t))
        (should (= 0 (majutsu--process-wait 'fake-process)))))
    (should-not seen-inhibit-quit)))

(ert-deftest majutsu-process-test-process-wait-returns-255-on-quit ()
  "`majutsu--process-wait' should return 255 when interrupted."
  (cl-letf (((symbol-function 'accept-process-output)
             (lambda (&rest _args) (signal 'quit nil)))
            ((symbol-function 'process-exit-status)
             (lambda (_process) (ert-fail "Should not be reached"))))
    (should (= 255 (majutsu--process-wait 'fake-process)))))

(ert-deftest majutsu-process-test-responsive-call-kills-process-on-quit ()
  "`majutsu--call-process-responsive' should clean up when interrupted."
  (let (deleted)
    (with-temp-buffer
      (let ((process-buf (current-buffer)))
        (cl-letf (((symbol-function 'majutsu-process-environment)
                   (lambda (_args) process-environment))
                  ((symbol-function 'majutsu--process-display-buffer)
                   (lambda (_process) nil))
                  ((symbol-function 'process-live-p)
                   (lambda (_process) t))
                  ((symbol-function 'delete-process)
                   (lambda (process) (setq deleted process)))
                  ((symbol-function 'process-status)
                   (lambda (_process) 'run))
                  ((symbol-function 'accept-process-output)
                   (lambda (&rest _args) (signal 'quit nil)))
                  ((symbol-function 'process-exit-status)
                   (lambda (_process) (ert-fail "Should not be reached"))))
          (should (= 255 (majutsu--call-process-responsive
                          "jj" process-buf 'dummy-section "/repo/")))
          (should (processp deleted)))))))

(ert-deftest majutsu-process-test-call-jj-handles-quit ()
  "`majutsu-call-jj' should finalize the section when interrupted."
  (let (seen-exit seen-section)
    (with-temp-buffer
      (let ((process-buf (current-buffer)))
        (setq default-directory "/repo/")
        (cl-letf (((symbol-function 'majutsu--toplevel-safe)
                   (lambda (&optional _directory) "/repo/"))
                  ((symbol-function 'majutsu-process-jj-arguments)
                   (lambda (args) args))
                  ((symbol-function 'majutsu-process-buffer)
                   (lambda (&optional _nodisplay) process-buf))
                  ((symbol-function 'majutsu--process-insert-section)
                   (lambda (_pwd _program _args &optional _err _errlog _face)
                     (insert "\n")
                     'dummy-section))
                  ((symbol-function 'majutsu--call-process-responsive)
                   (lambda (_program _process-buf section _root &rest _args)
                     (setq seen-section section)
                     255))
                  ((symbol-function 'majutsu-process-finish)
                   (lambda (exit _process-buf _command-buf _default-dir section)
                     (setq seen-exit exit)
                     (should (eq section 'dummy-section))
                     exit)))
          (should (= 255 (majutsu-call-jj "status"))))))
    (should (= seen-exit 255))
    (should (eq seen-section 'dummy-section))))

(defmacro majutsu-process-test--with-sh (&rest body)
  "Run BODY only when a POSIX `sh' is available.
When `sh' is missing, mark the test as skipped via `skip-unless' so it
is not silently reported as passed."
  (declare (indent 0))
  `(progn
     (skip-unless (executable-find "sh"))
     (let ((default-process-coding-system '(utf-8-unix . utf-8-unix)))
       ,@body)))

(ert-deftest majutsu-process-test-file-responsive-captures-stdout ()
  "Responsive runner should insert stdout at point in the destination buffer."
  (majutsu-process-test--with-sh
    (with-temp-buffer
      (insert "prefix:")
      (let ((exit (majutsu--process-file-responsive
                   "sh" nil t "-c" "printf 'hello\\nworld\\n'")))
        (should (= 0 exit))
        (should (equal (buffer-string) "prefix:hello\nworld\n"))))))

(ert-deftest majutsu-process-test-file-responsive-returns-exit-code ()
  "Responsive runner should return the subprocess exit status."
  (majutsu-process-test--with-sh
    (with-temp-buffer
      (should (= 3 (majutsu--process-file-responsive
                    "sh" nil t "-c" "exit 3"))))))

(ert-deftest majutsu-process-test-file-responsive-stderr-file-has-no-sentinel-noise ()
  "Responsive runner must not write Emacs sentinel status lines to a stderr file.

With `:stderr' set to a buffer, `make-process' spawns a separate
standard error process whose default sentinel appends a line such as
`Process sh stderr finished' to the buffer once it exits.  Without an
explicit sentinel override that line ends up in the stderr file, leaking
Emacs internals into Majutsu error messages.  This is a regression test
for that leak; see the Emacs Lisp manual, node `Accepting Output', for
the separate standard error process."
  (majutsu-process-test--with-sh
    (let ((errfile (make-temp-file "majutsu-proc-err")))
      (unwind-protect
          (with-temp-buffer
            (let ((exit (majutsu--process-file-responsive
                         "sh" nil (list t errfile)
                         "-c" (concat "printf 'OUT\\n'; "
                                      "for i in $(seq 1 2000); do "
                                      "printf 'ERR-%d\\n' \"$i\" 1>&2; done; "
                                      "exit 1"))))
              (should (= 1 exit))
              (should (equal (buffer-string) "OUT\n"))
              (with-temp-buffer
                (insert-file-contents errfile)
                (should (= 2000 (count-matches "^ERR-[0-9]+$"
                                               (point-min) (point-max))))
                (should (string-match-p "\\`ERR-1\n" (buffer-string)))
                (should (string-match-p "ERR-2000\n\\'" (buffer-string)))
                (should-not (string-match-p "Process .* \\(?:finished\\|exited\\)"
                                            (buffer-string))))))
        (ignore-errors (delete-file errfile))))))

(ert-deftest majutsu-process-test-file-responsive-mixes-stderr-into-stdout ()
  "With stdout-only destination, stderr is mixed into stdout."
  (majutsu-process-test--with-sh
    (with-temp-buffer
      (let ((exit (majutsu--process-file-responsive
                   "sh" nil t "-c" "printf 'O\\n'; printf 'E\\n' 1>&2")))
        (should (= 0 exit))
        (should (string-match-p "O\n" (buffer-string)))
        (should (string-match-p "E\n" (buffer-string)))))))

(ert-deftest majutsu-process-test-file-supported-p-narrow-contract ()
  "Only the call shapes Majutsu uses are routed through the runner."
  ;; Supported: no infile; stdout as t/buffer/string/nil; stderr nil/t/string.
  (should (majutsu--process-file-supported-p nil t))
  (should (majutsu--process-file-supported-p nil (list t "/tmp/err")))
  (should (majutsu--process-file-supported-p nil (list t t)))
  (should (majutsu--process-file-supported-p nil nil))
  ;; Unsupported: input files and stderr-to-buffer require `process-file'.
  (should-not (majutsu--process-file-supported-p "/tmp/in" t))
  (should-not (majutsu--process-file-supported-p
               nil (list t (get-buffer-create "*majutsu-test-err*")))))

;;; majutsu-process-test.el ends here
