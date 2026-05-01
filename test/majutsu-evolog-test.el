;;; majutsu-evolog-test.el --- Tests for evolog buffers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for jj evolog integration.

;;; Code:

(require 'ert)
(require 'majutsu-evolog)

(defun majutsu-evolog-test--record (&optional display)
  "Return one graph-record wrapped evolog test entry with DISPLAY."
  (concat "○  "
          (majutsu-graph-record--start-token majutsu-evolog--record-name)
          (or display
              (concat "qsustnur wd@example.com 2026-05-02 06:23:59 f6af8071\n"
                      "│  feat(op): phase1\n"
                      "│  -- operation 86978b4db954 describe commit 8794efe3"))
          (majutsu-graph-record--metadata-token majutsu-evolog--record-name)
          "change-full" majutsu-graph-record-field-separator
          "commit-full" majutsu-graph-record-field-separator
          "op-full"
          (majutsu-graph-record--end-token majutsu-evolog--record-name)))

(ert-deftest majutsu-evolog-entry-template/rebuilds-default-compact-with-graph-record ()
  "Evolog template should rebuild builtin_evolog_compact using graph-record."
  (should (string-match-p (regexp-quote "\\x1DGR:evolog:S")
                          majutsu-evolog--entry-template))
  (should (string-match-p (regexp-quote "\\x1DGR:evolog:M")
                          majutsu-evolog--entry-template))
  (should (string-match-p "self.commit().change_id().shortest(8)"
                          majutsu-evolog--entry-template))
  (should (string-match-p "self.commit().author().email()"
                          majutsu-evolog--entry-template))
  (should (string-match-p "self.commit().committer().timestamp().local().format"
                          majutsu-evolog--entry-template))
  (should (string-match-p "self.operation().id().short()"
                          majutsu-evolog--entry-template))
  (should-not (string-match-p "builtin_evolog_compact"
                              majutsu-evolog--entry-template)))

(ert-deftest majutsu-evolog-command-args/use-read-only-top-level-args-and-template ()
  "Evolog list commands should avoid snapshots and use Majutsu's template."
  (let ((args (majutsu-evolog--command-args "@" '("--limit=2" "--reversed"))))
    (should (member "--at-op=@" args))
    (should (member "--ignore-working-copy" args))
    (should (< (cl-position "--at-op=@" args :test #'equal)
               (cl-position "evolog" args :test #'equal)))
    (should (member "--limit=2" args))
    (should (member "--reversed" args))
    (should-not (member "--no-graph" args))
    (should (equal (last args 4)
                   (list "-r" "@" "-T" majutsu-evolog--entry-template)))))

(ert-deftest majutsu-evolog-wash-output/restores-visible-compact-output ()
  "Evolog washer should strip record tokens and keep visible graph output."
  (with-temp-buffer
    (insert (majutsu-evolog-test--record) "\n")
    (majutsu-evolog--wash-output nil)
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "○  qsustnur wd@example.com" content))
      (should (string-match-p "│  feat(op): phase1" content))
      (should (string-match-p "│  -- operation 86978b4db954" content))
      (should-not (string-match-p "GR:evolog" content))
      (should-not (string-match-p (regexp-quote majutsu-graph-record-field-separator)
                                  content)))))

(ert-deftest majutsu-evolog-refresh-buffer/inserts-restored-compact-output ()
  "Evolog refresh should render restored compact graph output, not details." 
  (cl-letf (((symbol-function 'majutsu-jj-wash)
             (lambda (washer _keep-error &rest _args)
               (insert (majutsu-evolog-test--record))
               (insert "\n")
               (funcall washer nil)
               0)))
    (with-temp-buffer
      (majutsu-evolog-mode)
      (setq majutsu-evolog--revset "@")
      (let ((inhibit-read-only t))
        (majutsu-evolog-refresh-buffer))
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "qsustnur wd@example.com" content))
        (should (string-match-p "-- operation 86978b4db954" content))
        (should-not (string-match-p "Change: change-full" content))
        (should-not (string-match-p "Operation id: op-full" content))))))

(ert-deftest majutsu-evolog-mode-map/does-not-bind-entry-actions ()
  "Evolog mode should not install premature point-specific entry actions."
  (should (eq (lookup-key majutsu-evolog-mode-map (kbd "RET"))
              'majutsu-visit-thing))
  (should (eq (lookup-key majutsu-evolog-mode-map (kbd "d"))
              'majutsu-diff)))

(ert-deftest majutsu-evolog/passes-revset-and-args-to-buffer ()
  "majutsu-evolog should remember revset and args in the buffer."
  (let (captured)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-evolog--buffer-name)
               (lambda (_revset) "*evolog*"))
              ((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (mode locked bindings &rest kwargs)
                 (setq captured (list mode locked bindings kwargs))
                 'buffer)))
      (should (eq (majutsu-evolog "@" '("--limit=2")) 'buffer))
      (should (eq (nth 0 captured) #'majutsu-evolog-mode))
      (should-not (nth 1 captured))
      (should (equal (nth 2 captured)
                     '((majutsu-evolog--revset "@")
                       (majutsu-evolog--args ("--limit=2")))))
      (should (equal (nth 3 captured)
                     '(:buffer "*evolog*" :directory "/repo/"))))))

(provide 'majutsu-evolog-test)
;;; majutsu-evolog-test.el ends here
