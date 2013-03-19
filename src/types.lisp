;;;; types.lisp --- Types used by the retf-rd-8 system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:retf-rd-8)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %magnet-schema? (uri)
    "A `puri:uri' with \"magnet\" schema."
    (and (puri:uri-p uri)
         (eq (puri:uri-scheme uri) :magnet))))

(deftype rd-8-uri ()
  "A `puri:uri' which can be interpreted as RETF RD 8 URI."
  `(and puri:uri (satisfies %magnet-schema?)))
