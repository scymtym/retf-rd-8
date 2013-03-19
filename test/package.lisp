;;;; package.lisp --- Package definition unit tests of the retf-rd-8 system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:retf-rd-8.test
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:eos

   #:retf-rd-8)

  (:documentation
   "This package contains unit tests for retf-rd-8 system."))

(cl:in-package #:retf-rd-8.test)

(def-suite retf-rd-8)

(defun run-tests ()
  (eos:run! 'retf-rd-8))
