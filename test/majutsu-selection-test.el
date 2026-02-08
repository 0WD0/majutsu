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

(provide 'majutsu-selection-test)
;;; majutsu-selection-test.el ends here
