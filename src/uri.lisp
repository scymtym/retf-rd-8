;;;; uri.lisp --- URI-related functions in the retf-rd-8 system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:retf-rd-8)

;;; Accessors

(defgeneric uri-query/plist (uri))

(defmethod uri-query/plist ((uri puri:uri))
  (let ((query  (or (puri:uri-query uri)
                    (error "~@<~A does not have a query part.~@:>"
                           uri)))
        (result '()))
    (mapc (lambda (element)
            (let* ((index (position #\= element)))
              (push (subseq element (1+ index)) result)
              (push (make-keyword (subseq element 0 index)) result)))
          (split-sequence #\& query))
    result))

(defmethod uri-hash ((uri puri:uri))
  (check-type uri rd-8-uri)
  (let+ (((&plist-r/o (xt :|xt|)) (uri-query/plist uri))
         (hash-urn (puri:uri xt)))
    (values (base32:base32-to-bytes
             (string-downcase (puri:urn-nss hash-urn)))
            (make-keyword (string-upcase (puri:urn-nid hash-urn))))))

(defmethod uri-syntax ((uri puri:uri))
  (check-type uri rd-8-uri)
  (getf (uri-query/plist uri) :|syntax|))

(defmethod uri-display-name ((uri puri:uri))
  (check-type uri rd-8-uri)
  (getf (uri-query/plist uri) :|dn|))

(defmethod uri-version ((uri puri:uri))
  (check-type uri rd-8-uri)
  (getf (uri-query/plist uri) :|v|))

(defmethod uri-sources ((uri puri:uri))
  (check-type uri rd-8-uri)
  (iter (for (key value) on (uri-query/plist uri))
        (when (eq key :|as|)
          (collect (puri:uri value)))))

;;; Construction

(defmethod make-uri ((thing simple-array)
                     &key
                     (digest       :sha1)
                     syntax
                     display-name
                     version
                     source
                     sources)
  (let* ((hash/b32 (base32:bytes-to-base32 thing))
         (hash-urn (make-instance 'puri:urn
                                  :nid (string-downcase digest)
                                  :nss (string-upcase hash/b32)))
         (sources  (mapcar #'puri:uri
                           (append (when source (list source)) sources)))
         (query    (format nil "xt=~A~@[&syntax=~A~]~@[&dn=~A~]~@[&v=~A~]~{&as=~A~}"
                           hash-urn syntax display-name version sources)))
    (make-instance 'puri:uri
                   :scheme :magnet
                   :path   "/"
                   :query  query)))

(defmethod make-uri ((thing string)
                     &rest args
                     &key
                     (digest :sha1)
                     &allow-other-keys)
  (let ((hash (ironclad:digest-sequence
               digest (sb-ext:string-to-octets thing))))
    (apply #'make-uri hash args)))

(defmethod make-uri ((thing sequence)
                     &rest args
                     &key
                     (digest :sha1)
                     &allow-other-keys)
  (let ((digester (ironclad:make-digest digest)))
    (mapc (lambda (line)
            (ironclad:update-digest digester line)
            (ironclad:update-digest
             digester (load-time-value (nibbles:octet-vector 13 10) t)))
          thing)
    (apply #'make-uri (ironclad:produce-digest digester) args)))
