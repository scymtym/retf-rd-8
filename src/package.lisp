;;;; package.lisp --- Package definition for the retf-rd-8 system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:retf-rd-8
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:iterate)

  (:export
   #:make-uri

   #:uri-hash
   #:uri-syntax
   #:uri-display-name
   #:uri-version
   #:uri-sources)

  (:documentation
   "This package contains an implementation of the RETF RD-8
recommendation for uniform referencing of data types."))
