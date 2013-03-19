;;;; protocol.lisp --- Protocol functions provided by the retf-rd-8 system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:retf-rd-8)

;;; Accessors

(defgeneric uri-hash (uri)
  (:documentation
   "Return two values: the hash value of URI and the name of the
digest that has been used to create the hash value."))

(defgeneric uri-syntax (uri)
  (:documentation
   "Return nil or a keyword designating the syntax according to which
the type designated by URI has been specified."))

(defgeneric uri-display-name (uri)
  (:documentation
   "Return nil or a name of the type designated by URI."))

(defgeneric uri-version (uri)
  (:documentation
   "Return nil or the version of the type designated by URI."))

(defgeneric uri-sources (uri)
  (:documentation
   "Return a list of URL from which the type designated by URI can be
retrieved."))

;;; Construction

(defgeneric make-uri (thing
                      &key
                      digest
                      syntax
                      display-name
                      version
                      source
                      sources)
  (:documentation
   "Construct a new RETF RD 8 URI for the data type definition given
by THING. THING can be an `nibbles:octet-vector', a string (without
newlines) or a sequences of lines.

DIGEST specifies the digest algorithm which should be used to computed
the hash of THING. The default, and the recommended digest is SHA1.

SYNTAX specifies the syntax in which THING is defined.

DISPLAY-NAME specifies a readable name for THING such as the name of
the file in which THING is defined or the qualified name of the data
type.

VERSION specifies the version of the data type corresponding to THING.

SOURCE and SOURCES can be used to specify URIs under which the data
type definition can be retrieved. If both are supplied, the union of
all sources is used."))
