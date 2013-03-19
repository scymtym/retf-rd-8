;;;; uri.lisp --- Tests for URI-related functions in the retf-rd-8 system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:retf-rd-8.test)

(in-suite retf-rd-8)

(macrolet
    ((define-test-case (name
                        &rest args
                        &key
                        message-definition
                        hash
                        digest
                        syntax
                        display-name
                        version
                        sources
                        uri)
       (declare (ignore message-definition hash digest syntax
                        display-name version sources uri))
       (let+ (((&flet+ variable ((aspect . value))
                 (let ((variable-name
                         (format-symbol *package* "+~A-~A+" name aspect)))
                   `(defvar ,variable-name ,value)))))
         `(progn
            ,@(mapcar #'variable (plist-alist args))))))

  (define-test-case 1
    :message-definition
    "float64 x
float64 y
float64 z
float64 x
float64 y
float64 z"
    :hash         #(241 96 181 129 248 175 145 229 244 169 253 16 218 88 198 57 25 215 132 224)
    :digest       :sha1
    :syntax       "rosmsg"
    :display-name "Twist.msg"
    :version      "g34242"
    :sources      '("https://code.cor-lab.org/Twist.msg")
    :uri          "magnet:/?xt=urn:sha1:6FQLLAPYV6I6L5FJ7UINUWGGHEM5PBHA&syntax=rosmsg&dn=Twist.msg&v=g34242&as=https://code.cor-lab.org/Twist.msg")

  #+TODO-case2? (let ((u (puri:uri "magnet:?xt=urn:sha1:23409882430&dn=Bla.msg&as=http://foo")))
    (values
     (multiple-value-list (uri-hash u))
     (uri-display-name u)
     (uri-sources u))))

(test encode
  (is (puri:uri= (make-uri (mapcar #'sb-ext:string-to-octets
                                   (split-sequence #\Newline +1-message-definition+))
                           :syntax       +1-syntax+
                           :display-name +1-display-name+
                           :version      +1-version+
                           :sources      +1-sources+)
                 (puri:uri +1-uri+))))

(test decode
  (let ((uri (puri:uri +1-uri+)))
    (let+ (((&values hash digest) (uri-hash uri)))
      (is (equalp hash +1-hash+))
      (is (eq digest +1-digest+)))
    (macrolet ((slot= (accessor value &optional (predicate '#'string=))
                 `(is (funcall ,predicate (,accessor uri) ,value))))
      (slot= uri-syntax       +1-syntax+)
      (slot= uri-display-name +1-display-name+)
      (slot= uri-version      +1-version+)
      (slot= uri-sources      (mapcar #'puri:uri +1-sources+)
             (rcurry #'set-equal :test #'puri:uri=)))))
