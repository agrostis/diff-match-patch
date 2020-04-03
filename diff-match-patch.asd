;;;; -*- lisp -*-

(defpackage #:dmp-system
  (:use #:cl #:asdf #:uiop/package))
(in-package #:dmp-system)

(defsystem #:diff-match-patch
  :name "Diff/Match/Patch"
  :description "A Common Lisp port of Neil Fraser's library of the same name"
  :version "0.2.1"
  :author "Neil Fraser; ported by Boris Smilga"
  :maintainer "Boris Smilga <boris.smilga@gmail.com>"
  :license "Apache 2.0"
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
           (:file "patch"))))
  :in-order-to ((test-op (load-op #:diff-match-patch/test))))

(defsystem #:diff-match-patch/test
  :name "Diff/Match/Patch tests"
  :version "0.2.1"
  :author "Neil Fraser; ported by Boris Smilga"
  :maintainer "Boris Smilga <boris.smilga@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:diff-match-patch #:fiveam
               (:version #:cl-interpol "0.2.6"))
  :components
    ((:module #:src
        :serial t
        :pathname ""
        :components ((:file "test")))))

(defmethod perform ((op test-op)
                    (system (eql (find-system '#:diff-match-patch))))
  (funcall (find-symbol* '#:run-all '#:dmp-test)))
