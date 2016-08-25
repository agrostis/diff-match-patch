;;;; -*- lisp -*-

(defpackage #:dmp-system
  (:use #:cl #:asdf #:uiop/package))
(in-package #:dmp-system)

(defsystem #:diff-match-patch
  :name "Diff/Match/Patch"
  :depends-on (#:iterate #:cl-ppcre)
  :components
    ((:module #:src
        :serial t
        :pathname ""
        :components
          ((:file "package")
           (:file "lib")
           (:file "diff")
           (:file "cleanup")
           (:file "match")
           (:file "patch")))))

(defsystem #:diff-match-patch.test
  :name "Diff/Match/Patch tests"
  :depends-on (#:diff-match-patch #:fiveam #:cl-interpol)
  :components
    ((:module #:src
        :serial t
        :pathname ""
        :components ((:file "test")))))

(defmethod perform ((op test-op)
                    (system (eql (find-system '#:diff-match-patch))))
  (load-system '#:diff-match-patch.test)
  (funcall (find-symbol* '#:run-all '#:dmp-test)))
