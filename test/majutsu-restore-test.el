;;; majutsu-restore-test.el --- Tests for restore transient  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for restore command assembly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'majutsu-restore)

(ert-deftest majutsu-restore-execute/places-structured-filesets-after-options ()
  "Execute restore with transient filesets after option arguments."
  (let (called)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _) nil))
              ((symbol-function 'majutsu-run-jj)
               (lambda (&rest args)
                 (setq called args)
                 0))
              ((symbol-function 'message)
               (lambda (&rest _) nil)))
      (majutsu-restore-execute '(("--" "src/a.el") "--from=@-" "--to=@"))
      (should (equal called
                     '("restore" "--from=@-" "--to=@"
                       "--" "src/a.el"))))))

(ert-deftest majutsu-restore-execute/replays-complement-plan ()
  "Restore replays the complement plan after checking the displayed selector."
  (let (called cleared)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _)
                 '(:base right :payload-root left
                   :patch "PATCH"
                   :file-ops ((:action add :path "keep.bin")))))
              ((symbol-function 'majutsu-restore--patch-selector)
               (lambda (&rest _)
                 (list :from "@-" :to "@")))
              ((symbol-function 'majutsu-interactive-run-replay-plan)
               (lambda (&rest args)
                 (setq called args)))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (majutsu-restore-execute '(("--" "src/a.el") "--from=@-" "--to=@"))
      (should (equal called
                     '("restore"
                       ("--from=@-" "--to=@")
                       ("src/a.el")
                       (:base right
                        :payload-root left
                        :patch "PATCH"
                        :file-ops ((:action add :path "keep.bin"))))))
      (should cleared))))

(ert-deftest majutsu-restore-execute/rejects-mismatched-selector ()
  "Patch restore refuses when transient selectors leave the displayed diff."
  (let (cleared)
    (cl-letf (((symbol-function 'majutsu-interactive-build-replay-plan-if-selected)
               (lambda (&rest _)
                 '(:base right :payload-root left :patch "PATCH" :file-ops nil)))
              ((symbol-function 'majutsu-restore--patch-selector)
               (lambda (&rest _)
                 (list :from "@-" :to "@")))
              ((symbol-function 'majutsu-interactive-clear)
               (lambda () (setq cleared t))))
      (should-error (majutsu-restore-execute '("--from=A" "--to=B"))
                    :type 'user-error)
      (should-not cleared))))

(ert-deftest majutsu-restore-default-args/maps-revisions-to-changes-in ()
  "Diff --revisions becomes restore --changes-in."
  (with-temp-buffer
    (majutsu-diff-mode)
    (setq-local majutsu-buffer-diff-range '("--revisions=abc"))
    (should (equal (majutsu-restore--default-args)
                   '("--changes-in=abc")))))

(ert-deftest majutsu-restore-selector/uses-transient-arg-value ()
  "Selector projection uses transient-arg-value, not a private argv scanner."
  (should (equal (majutsu-restore--selector '("--changes-in=abc"))
                 '(:changes-in "abc")))
  (should (equal (majutsu-restore--selector '("--from=A"))
                 '(:from "A" :to "@")))
  (should (equal (majutsu-restore--selector '("--to=B"))
                 '(:from "@" :to "B")))
  (should (equal (majutsu-restore--selector nil)
                 '(:changes-in "@"))))

(provide 'majutsu-restore-test)
;;; majutsu-restore-test.el ends here
