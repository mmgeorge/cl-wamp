(defpackage :wamp/session
  (:use :cl :wamp/transport)
  (:import-from :wamp/transport)
  (:import-from :wamp/message-type)
  (:export #:session
           #:make-session))

(in-package :wamp/session)

;; Avoid "to-complex" sbcl warning

(deftype session-t () '(values session &optional))

(defstruct registration
  (id 0 :type integer :read-only t))

(defstruct registration-opts)

;; Exports

(defclass session ()
  ((transport :reader session-transport :initarg :transport :type transport)
   (realm :reader session-realm :initarg :realm :type string)))


(defun make-session (realm)
  (declare (string realm))
  (the session-t
       (make-instance 'session :realm realm)))



;(deftype match-t () '(member 'prefix 'wildcard))

(defun session-register (self uri procedure &key match)
  (declare (string uri) (function procedure) ((member 'prefix 'wildcard) match))
  (the session-t
       (let ((option (_make-options (pairlis '(match) (list match)))))
         (_session-send-message self 'register (_make-message-id) option uri))))

;; Interal

(defvar *_id-counter* 0)

(defun _make-message-id () (incf *_id-counter*))

(defun _session-send-message (self type &rest args)
  (declare (session self) (wamp/message-type:message-t type) (list args))
  (the session-t
       (progn (transport-send (session-transport self) (cons type args))
              self)))


(defun _make-options (assoc)
  "Encodes an association list as a hash-table. Nil values are discarded. 
   Used for encoding."
  (declare (list assoc))
  (the hash-table
       (let ((table (make-hash-table)))
         (loop for (key . value) in assoc
               when value do (setf (gethash key table) value))
         table)))

