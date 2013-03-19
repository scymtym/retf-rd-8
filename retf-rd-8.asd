;;;; retf-rd-8.asd --- System definitions for the retf-rd-8 system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:retf-rd-8-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:retf-rd-8-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 1
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))

;;; System definition

(defsystem :retf-rd-8
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING for details"
  :description "Implementation of the RETF RD-8 recommendation"
  :depends-on  (:alexandria
                :split-sequence
                :iterate
                (:version :let-plus "0.2")

                :puri
                :ironclad
                :cl-base32)
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "protocol")
                              (:file       "uri"))))

  :in-order-to ((test-op (test-op :retf-rd-8-test))))

(defsystem :retf-rd-8-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING for details"
  :description "Implementation of the RETF RD-8 recommendation"
  :depends-on  (:alexandria
                :eos

                :retf-rd-8)
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "uri")))))

(defmethod perform ((op     test-op)
                    (system (eql (find-system :retf-rd-8-test))))
  (funcall (find-symbol "RUN-TESTS" :retf-rd-8.test)))
