;;; majutsu-selection-test.el --- Tests for selection  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for selection overlays and option helpers.

;;; Code:

(require 'ert)
(require 'majutsu-selection)
(require 'transient)
(require 'magit-section)

(defclass majutsu-test-option (majutsu-selection-option)
  ())

(defclass majutsu-test-toggle-option (majutsu-selection-toggle-option)
  ())

(defclass majutsu-test-reading-option (majutsu-selection-option)
  ())

(defvar-local majutsu-selection-test--source-marker nil)

(cl-defmethod transient-infix-read ((_obj majutsu-test-reading-option))
  majutsu-selection-test--source-marker)

(ert-deftest majutsu-selection-render-test ()
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (insert "1234567890")
        (let* ((session (majutsu-selection-session-begin))
               (obj (make-instance 'majutsu-test-option
                                   :command 'ignore
                                   :key "a"
                                   :argument "--a="
                                   :selection-label "A"
                                   :locate-fn (lambda (val)
                                                (if (equal val "1")
                                                    (cons 1 3)
                                                  (cons 4 6)))))
               (transient--suffixes (list obj)))
        (oset obj value "1")
        (majutsu-selection-render session)
        (let ((ovs (overlays-at 1)))
          (should ovs)
          (let ((ov (seq-find (lambda (o) (overlay-get o 'majutsu-selection)) ovs)))
            (should ov)
            (should (= (overlay-start ov) 1))
            (should (= (overlay-end ov) 3))
            (should (string-match-p "A" (overlay-get ov 'before-string)))))
        (oset obj value "2")
        (majutsu-selection-render session)
        (let ((ovs (overlays-at 4)))
          (should ovs)
          (let ((ov (seq-find (lambda (o) (overlay-get o 'majutsu-selection)) ovs)))
            (should ov)
            (should (= (overlay-start ov) 4))
            (should (= (overlay-end ov) 6))))
        (oset obj value nil)
        (majutsu-selection-render session)
        (should-not (seq-find (lambda (o) (overlay-get o 'majutsu-selection))
                              (overlays-in (point-min) (point-max))))))))

(ert-deftest majutsu-selection-shared-locate-fn-test ()
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (insert "1234567890")
      (let* ((session (majutsu-selection-session-begin))
             (locator (lambda (val)
                        (if (equal val "1")
                            (cons 1 3)
                          (cons 4 6))))
             (provider (make-instance 'majutsu-test-option
                                      :command 'ignore
                                      :key "a"
                                      :argument "--a="
                                      :selection-label "A"
                                      :locate-fn locator))
             (consumer (make-instance 'majutsu-test-option
                                      :command 'ignore
                                      :key "b"
                                      :argument "--a="
                                      :selection-label "B"))
             (transient--suffixes (list provider consumer)))
        (oset consumer value "1")
        (majutsu-selection-render session)
        (let ((ovs (overlays-at 1)))
          (should ovs)
          (let ((ov (seq-find (lambda (o) (overlay-get o 'majutsu-selection)) ovs)))
            (should ov)
            (should (= (overlay-start ov) 1))
            (should (= (overlay-end ov) 3))
            (should (string-match-p "B" (overlay-get ov 'before-string)))))))))

(ert-deftest majutsu-selection-shared-targets-fn-test ()
  (with-temp-buffer
    (magit-section-mode)
    (let* ((provider (make-instance 'majutsu-test-option
                                    :command 'ignore
                                    :key "a"
                                    :argument "--a="
                                    :multi-value 'repeat
                                    :targets-fn (lambda () '("1"))))
           (consumer (make-instance 'majutsu-test-option
                                    :command 'ignore
                                    :key "b"
                                    :argument "--a="))
           (transient--suffixes (list consumer provider)))
      (should (majutsu-selection--selection-multi-p consumer))
      (let* ((fn (majutsu-selection--resolve-targets-fn consumer))
             (values (funcall fn))
             (current (majutsu-selection--toggle-current
                       (oref consumer value)
                       values
                       (majutsu-selection--selection-multi-p consumer))))
        (oset consumer value current)
        (should (equal (oref consumer value) '("1")))
        (should (listp (oref consumer value)))))))

(ert-deftest majutsu-selection-infix-read/uses-session-buffer ()
  "Selection option readers should run in the session source buffer."
  (let ((source-buf (generate-new-buffer " *majutsu-selection-source*"))
        (caller-buf (generate-new-buffer " *majutsu-selection-caller*")))
    (unwind-protect
        (let ((session (majutsu-selection-session-create
                        :buffer source-buf
                        :overlays (make-hash-table :test 'equal)))
              (obj (make-instance 'majutsu-test-reading-option
                                  :command 'ignore
                                  :key "a"
                                  :argument "--a=")))
          (with-current-buffer source-buf
            (setq majutsu-selection-test--source-marker 'source))
          (with-current-buffer caller-buf
            (setq majutsu-selection-test--source-marker 'caller))
          (cl-letf (((symbol-function 'transient-scope)
                     (lambda (&rest _args) session))
                    ((symbol-function 'transient--show) #'ignore))
            (with-current-buffer caller-buf
              (should (eq (transient-infix-read obj) 'source)))))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p caller-buf)
        (kill-buffer caller-buf)))))

(ert-deftest majutsu-selection-toggle-option-read/uses-session-buffer ()
  "Toggle options should collect point targets in the session source buffer."
  (let ((source-buf (generate-new-buffer " *majutsu-selection-source*"))
        (caller-buf (generate-new-buffer " *majutsu-selection-caller*")))
    (unwind-protect
        (let* ((session (majutsu-selection-session-create
                         :buffer source-buf
                         :overlays (make-hash-table :test 'equal)))
               (obj (make-instance 'majutsu-test-toggle-option
                                   :command 'ignore
                                   :key "a"
                                   :argument "--a="
                                   :targets-fn (lambda ()
                                                 (list (symbol-name
                                                        majutsu-selection-test--source-marker))))))
          (with-current-buffer source-buf
            (setq majutsu-selection-test--source-marker 'source))
          (with-current-buffer caller-buf
            (setq majutsu-selection-test--source-marker 'caller))
          (cl-letf (((symbol-function 'transient-scope)
                     (lambda (&rest _args) session))
                    ((symbol-function 'transient--show) #'ignore))
            (with-current-buffer caller-buf
              (should (equal (transient-infix-read obj) "source")))))
      (when (buffer-live-p source-buf)
        (kill-buffer source-buf))
      (when (buffer-live-p caller-buf)
        (kill-buffer caller-buf)))))

(provide 'majutsu-selection-test)
;;; majutsu-selection-test.el ends here
