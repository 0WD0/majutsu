;;; majutsu-arrange-test.el --- Tests for majutsu-arrange  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <1105848296@qq.com>
;; Maintainer: 0WD0 <1105848296@qq.com>
;; Keywords: tools, vc
;; URL: https://github.com/0WD0/majutsu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for the arrange graph model and CLI replay compiler.

;;; Code:

(require 'ert)
(require 'majutsu-arrange)

(defun majutsu-arrange-test--node (id parents &optional role change-id)
  "Create a test arrange node ID with PARENTS and ROLE."
  (majutsu-arrange-node-create
   :id id
   :change-id (or change-id (concat "change-" id))
   :commit-id id
   :short-change-id id
   :short-commit-id id
   :description (concat "commit " id)
   :parents (copy-sequence parents)
   :original-parents (copy-sequence parents)
   :action 'keep
   :role (or role 'target)))

(defun majutsu-arrange-test--session (nodes target-ids &optional external-parent-ids external-child-ids selected-id)
  "Create a test arrange session from NODES and TARGET-IDS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (node nodes)
      (puthash (majutsu-arrange-node-id node) node table))
    (majutsu-arrange--refresh-derived-order
     (majutsu-arrange-session-create
      :root "/repo/"
      :revset "test"
      :base-operation-id "op"
      :nodes table
      :target-ids target-ids
      :external-parent-ids external-parent-ids
      :external-child-ids external-child-ids
      :head-order nil
      :current-order nil
      :selected-id (or selected-id (car target-ids))
      :operations nil
      :dirty-p nil
      :applying-p nil))))

(ert-deftest majutsu-arrange-parse-node-line ()
  "Machine template lines should parse into arrange nodes."
  (let* ((line (string-join
                (list "change-full" "commit-full" "change8" "commit8" "summary"
                      (string-join '("parent-1" "parent-2")
                                   majutsu-arrange--list-separator))
                majutsu-arrange--field-separator))
         (node (majutsu-arrange--parse-node-line line 'target)))
    (should (equal (majutsu-arrange-node-id node) "commit-full"))
    (should (equal (majutsu-arrange-node-change-id node) "change-full"))
    (should (equal (majutsu-arrange-node-short-change-id node) "change8"))
    (should (equal (majutsu-arrange-node-description node) "summary"))
    (should (equal (majutsu-arrange-node-parents node) '("parent-1" "parent-2")))
    (should (equal (majutsu-arrange-node-original-parents node) '("parent-1" "parent-2")))
    (should (eq (majutsu-arrange-node-action node) 'keep))
    (should (eq (majutsu-arrange-node-role node) 'target))))

(ert-deftest majutsu-arrange-compute-order/merge-stack ()
  "Current order should be stable children-before-parents order."
  ;; D
  ;; |\
  ;; B C
  ;; |/
  ;; A
  (let* ((a (majutsu-arrange-test--node "A" '("root")))
         (b (majutsu-arrange-test--node "B" '("A")))
         (c (majutsu-arrange-test--node "C" '("A")))
         (d (majutsu-arrange-test--node "D" '("B" "C")))
         (session (majutsu-arrange-test--session
                   (list d c b a)
                   '("D" "C" "B" "A")
                   '("root"))))
    (should (equal (majutsu-arrange-session-head-order session) '("D")))
    (should (equal (majutsu-arrange-session-current-order session)
                   '("D" "B" "C" "A")))))

(ert-deftest majutsu-arrange-swap-commits/linear-stack ()
  "Swapping adjacent commits should exchange parent relationships."
  ;; C       B
  ;; |  =>   |
  ;; B       C
  ;; |       |
  ;; A       A
  (let* ((a (majutsu-arrange-test--node "A" '("root")))
         (b (majutsu-arrange-test--node "B" '("A")))
         (c (majutsu-arrange-test--node "C" '("B")))
         (session (majutsu-arrange-test--session
                   (list c b a)
                   '("C" "B" "A")
                   '("root") nil "C")))
    (majutsu-arrange--swap-commits session "C" "B")
    (should (equal (majutsu-arrange-node-parents (majutsu-arrange--node session "B"))
                   '("C")))
    (should (equal (majutsu-arrange-node-parents (majutsu-arrange--node session "C"))
                   '("A")))
    (should (equal (majutsu-arrange-session-head-order session) '("B")))
    (should (equal (majutsu-arrange-session-current-order session) '("B" "C" "A")))
    (should (equal (majutsu-arrange-session-selected-id session) "C"))))

(ert-deftest majutsu-arrange-swap-commits/updates-external-child ()
  "Swapping should update context child parent references too."
  ;; E is an external child of C. After swapping C and B, E should point at B.
  (let* ((a (majutsu-arrange-test--node "A" '("root")))
         (b (majutsu-arrange-test--node "B" '("A")))
         (c (majutsu-arrange-test--node "C" '("B")))
         (e (majutsu-arrange-test--node "E" '("C") 'external-child))
         (session (majutsu-arrange-test--session
                   (list e c b a)
                   '("C" "B" "A")
                   '("root") '("E") "C")))
    (majutsu-arrange--swap-commits session "C" "B")
    (should (equal (majutsu-arrange-node-parents (majutsu-arrange--node session "E"))
                   '("B")))))

(ert-deftest majutsu-arrange-swap-down/records-normalized-operation ()
  "Swap-down should record (:swap parent child)."
  (let* ((a (majutsu-arrange-test--node "A" '("root")))
         (b (majutsu-arrange-test--node "B" '("A")))
         (c (majutsu-arrange-test--node "C" '("B")))
         (session (majutsu-arrange-test--session
                   (list c b a)
                   '("C" "B" "A")
                   '("root") nil "C")))
    (with-temp-buffer
      (majutsu-arrange-mode)
      (setq majutsu-arrange--session session)
      (cl-letf (((symbol-function 'majutsu-arrange--rerender) (lambda () nil)))
        (majutsu-arrange-swap-down))
      (should (equal (majutsu-arrange-session-operations session)
                     '((:swap "B" "C")))))))

(ert-deftest majutsu-arrange-subject/subdag-roots-heads-and-boundary-edges ()
  "Subdag subjects should expose roots, heads, and boundary edges."
  ;; E
  ;; |
  ;; D F
  ;; |\
  ;; B C
  ;; |/
  ;; A
  (let* ((a (majutsu-arrange-test--node "A" '("root")))
         (b (majutsu-arrange-test--node "B" '("A")))
         (c (majutsu-arrange-test--node "C" '("A")))
         (d (majutsu-arrange-test--node "D" '("B" "C")))
         (e (majutsu-arrange-test--node "E" '("D" "F")))
         (f (majutsu-arrange-test--node "F" '("root")))
         (session (majutsu-arrange-test--session
                   (list e f d c b a)
                   '("E" "F" "D" "C" "B" "A")
                   '("root") nil "B")))
    (let ((subject (majutsu-arrange--make-subject session 'subdag)))
      (should (equal (majutsu-arrange-subject-ids subject) '("E" "D" "B")))
      (should (equal (majutsu-arrange-subject-roots subject) '("B")))
      (should (equal (majutsu-arrange-subject-heads subject) '("E")))
      (should (equal (majutsu-arrange-subject-incoming-edges subject)
                     '(("E" . "F") ("D" . "C") ("B" . "A")))))))

(ert-deftest majutsu-arrange-insert-after/preserves-subject-dag ()
  "Insert-after should preserve internal DAG edges and rewrite boundaries."
  ;; E      Y
  ;; |      |
  ;; D      X
  ;; |\
  ;; B C
  ;; |/
  ;; A
  (let* ((a (majutsu-arrange-test--node "A" '("root")))
         (b (majutsu-arrange-test--node "B" '("A")))
         (c (majutsu-arrange-test--node "C" '("A")))
         (d (majutsu-arrange-test--node "D" '("B" "C")))
         (e (majutsu-arrange-test--node "E" '("D")))
         (x (majutsu-arrange-test--node "X" '("root")))
         (y (majutsu-arrange-test--node "Y" '("X")))
         (session (majutsu-arrange-test--session
                   (list y x e d c b a)
                   '("Y" "X" "E" "D" "C" "B" "A")
                   '("root") nil "B")))
    (let ((subject (majutsu-arrange--make-subject session 'marked '("D" "C" "B"))))
      (majutsu-arrange--insert-subject-after session subject '("X"))
      (should (equal (majutsu-arrange-node-parents (majutsu-arrange--node session "D"))
                     '("B" "C")))
      (should (equal (majutsu-arrange-node-parents (majutsu-arrange--node session "B"))
                     '("X")))
      (should (equal (majutsu-arrange-node-parents (majutsu-arrange--node session "C"))
                     '("X")))
      (should (equal (majutsu-arrange-node-parents (majutsu-arrange--node session "E"))
                     '("A")))
      (should (equal (majutsu-arrange-node-parents (majutsu-arrange--node session "Y"))
                     '("D"))))))

(ert-deftest majutsu-arrange-insert-before/own-child-does-not-create-cycle ()
  "Insert-before should preserve old root parents when anchor is a child."
  ;; C
  ;; |
  ;; B
  ;; |
  ;; A
  (let* ((a (majutsu-arrange-test--node "A" '("root")))
         (b (majutsu-arrange-test--node "B" '("A")))
         (c (majutsu-arrange-test--node "C" '("B")))
         (session (majutsu-arrange-test--session
                   (list c b a)
                   '("C" "B" "A")
                   '("root") nil "B")))
    (let ((subject (majutsu-arrange--make-subject session 'single '("B"))))
      (majutsu-arrange--insert-subject-before session subject '("C"))
      (should (equal (majutsu-arrange-node-parents (majutsu-arrange--node session "B"))
                     '("A")))
      (should (equal (majutsu-arrange-node-parents (majutsu-arrange--node session "C"))
                     '("B"))))))

(ert-deftest majutsu-arrange-marked-subject/rejects-disconnected-set ()
  "Marked subjects should be connected before they can move."
  (let* ((a (majutsu-arrange-test--node "A" '("root")))
         (b (majutsu-arrange-test--node "B" '("A")))
         (x (majutsu-arrange-test--node "X" '("root")))
         (session (majutsu-arrange-test--session
                   (list x b a)
                   '("X" "B" "A")
                   '("root") nil "B")))
    (should-error (majutsu-arrange--make-subject session 'marked '("B" "X"))
                  :type 'user-error)))

(ert-deftest majutsu-arrange-compile-dag-preserving-move-commands ()
  "DAG-preserving moves should compile through jj rebase -r."
  (let* ((a (majutsu-arrange-test--node "A" '("root") 'target "change-A"))
         (b (majutsu-arrange-test--node "B" '("A") 'target "change-B"))
         (c (majutsu-arrange-test--node "C" '("A") 'target "change-C"))
         (d (majutsu-arrange-test--node "D" '("B" "C") 'target "change-D"))
         (x (majutsu-arrange-test--node "X" '("root") 'target "change-X"))
         (session (majutsu-arrange-test--session
                   (list x d c b a)
                   '("X" "D" "C" "B" "A")
                   '("root") nil "D")))
    (setf (majutsu-arrange-session-operations session)
          '((:insert-after ("D" "B" "C") ("X"))
            (:move-onto ("B" "C") ("A" "X"))
            (:insert-before ("D") ("X"))))
    (should (equal (majutsu-arrange--compile-commands session)
                   '(("rebase" "-r" "change-D | change-B | change-C"
                      "-A" "change-X" "--keep-divergent")
                     ("rebase" "-r" "change-B | change-C"
                      "-o" "change-A" "-o" "change-X" "--keep-divergent")
                     ("rebase" "-r" "change-D"
                      "-B" "change-X" "--keep-divergent"))))))

(ert-deftest majutsu-arrange-swap-up/rejects-external-child ()
  "Swap-up should not cross the target/context boundary."
  (let* ((a (majutsu-arrange-test--node "A" '("root")))
         (b (majutsu-arrange-test--node "B" '("A")))
         (e (majutsu-arrange-test--node "E" '("B") 'external-child))
         (session (majutsu-arrange-test--session
                   (list e b a)
                   '("B" "A")
                   '("root") '("E") "B")))
    (with-temp-buffer
      (majutsu-arrange-mode)
      (setq majutsu-arrange--session session)
      (cl-letf (((symbol-function 'majutsu-arrange--rerender) (lambda () nil)))
        (majutsu-arrange-swap-up))
      (should (null (majutsu-arrange-session-operations session)))
      (should (equal (majutsu-arrange-node-parents (majutsu-arrange--node session "E"))
                     '("B"))))))

(ert-deftest majutsu-arrange-swap-up/rejects-multiple-children ()
  "Swap-up should require exactly one child."
  (let* ((a (majutsu-arrange-test--node "A" '("root")))
         (b (majutsu-arrange-test--node "B" '("A")))
         (c (majutsu-arrange-test--node "C" '("A")))
         (session (majutsu-arrange-test--session
                   (list c b a)
                   '("C" "B" "A")
                   '("root") nil "A")))
    (with-temp-buffer
      (majutsu-arrange-mode)
      (setq majutsu-arrange--session session)
      (cl-letf (((symbol-function 'majutsu-arrange--rerender) (lambda () nil)))
        (majutsu-arrange-swap-up))
      (should (null (majutsu-arrange-session-operations session)))
      (should (equal (majutsu-arrange-node-parents (majutsu-arrange--node session "B"))
                     '("A")))
      (should (equal (majutsu-arrange-node-parents (majutsu-arrange--node session "C"))
                     '("A"))))))

(ert-deftest majutsu-arrange-compile-commands ()
  "Arrange plans should compile to conservative CLI replay commands."
  (let* ((a (majutsu-arrange-test--node "A" '("root") 'target "change-A"))
         (b (majutsu-arrange-test--node "B" '("A") 'target "change-B"))
         (c (majutsu-arrange-test--node "C" '("B") 'target "change-C"))
         (session (majutsu-arrange-test--session
                   (list c b a)
                   '("C" "B" "A")
                   '("root") nil "C")))
    (setf (majutsu-arrange-session-operations session) '((:swap "B" "C")))
    (setf (majutsu-arrange-node-action a) 'abandon)
    (should (equal (majutsu-arrange--compile-commands session)
                   '(("rebase" "-r" "change-B" "-A" "change-C" "--keep-divergent")
                     ("abandon" "change-A"))))))

(ert-deftest majutsu-arrange-entrypoint/passes-session-binding ()
  "Entry command should pass arrange session as one setup-buffer binding."
  (let (seen-mode seen-locked seen-bindings seen-keys)
    (cl-letf (((symbol-function 'majutsu--toplevel-safe)
               (lambda (&optional _directory) "/repo/"))
              ((symbol-function 'majutsu-arrange--configured-default-revset)
               (lambda () "revset"))
              ((symbol-function 'majutsu-arrange-build-session)
               (lambda (revset root) (list :session revset root)))
              ((symbol-function 'majutsu-setup-buffer-internal)
               (lambda (mode locked bindings &rest keys)
                 (setq seen-mode mode)
                 (setq seen-locked locked)
                 (setq seen-bindings bindings)
                 (setq seen-keys keys)
                 'arrange-buffer)))
      (let ((current-prefix-arg nil))
        (should (eq (majutsu-arrange) 'arrange-buffer))))
    (should (eq seen-mode #'majutsu-arrange-mode))
    (should-not seen-locked)
    (should (equal seen-bindings
                   '((majutsu-arrange--session (:session "revset" "/repo/")))))
    (should (equal seen-keys
                   '(:buffer "*majutsu-arrange: repo*" :directory "/repo/")))))

(ert-deftest majutsu-arrange-make-plan/captures-final-state ()
  "Plan objects should carry final parents, actions, operations, and affected ids."
  (let* ((a (majutsu-arrange-test--node "A" '("root")))
         (b (majutsu-arrange-test--node "B" '("A")))
         (session (majutsu-arrange-test--session
                   (list b a)
                   '("B" "A")
                   '("root") nil "B")))
    (setf (majutsu-arrange-session-operations session) '((:swap "A" "B")))
    (setf (majutsu-arrange-node-action a) 'abandon)
    (let ((plan (majutsu-arrange--make-plan session)))
      (should (equal (gethash "A" (majutsu-arrange-plan-final-parents plan))
                     '("root")))
      (should (eq (gethash "A" (majutsu-arrange-plan-final-actions plan))
                  'abandon))
      (should (equal (majutsu-arrange-plan-operations plan)
                     '((:swap "A" "B"))))
      (should (equal (majutsu-arrange-plan-affected-ids plan)
                     '("A" "B"))))))

(provide 'majutsu-arrange-test)
;;; majutsu-arrange-test.el ends here
