(defpackage :wamp/session
  (:use :cl :wamp/transport :wamp/message-type)
  (:import-from :wamp/transport)
  (:import-from :wamp/message-type)
  (:export #:session #:make-session))

(in-package :wamp/session)

;; Avoid "to-complex" sbcl warning
(deftype session-t () '(values session &optional))

(defstruct registration
  (id 0 :type integer :read-only t))

;; ++ Exports ++

(defclass session ()
  ((transport :reader session-transport :initarg :transport :type transport)
   (realm :reader session-realm :initarg :realm :type string)))


(defun make-session (transport realm)
  (declare (transport transport) (string realm))
  (the session-t
       (make-instance 'session :transport transport :realm realm)))


(defun session-register (self uri procedure &key match)
  (declare (string uri) (function procedure) ((member 'prefix 'wildcard) match))
  (the session-t
       (let ((option (_make-options (pairlis '(match) (list match)))))
         (_session-send-message self 'register (_make-message-id) option uri))))

;; ++ Internal ++


(defvar *_id-counter* 0)

(defun _make-message-id ()
  (the fixnum
       (incf *_id-counter*)))

(defun _session-handle-message (self type &rest args)
  (declare (session self) (message-t type) (list args))
  (case type
    (welcome (_session-handle-welcome self (car args) (cdr args))))
  )


(defun _session-handle-welcome (self realm details)
  (declare(string realm) (list details)
           (ignore self))
  (format t "~a ~a" realm details))


(defun _session-send-message (self type &rest args)
  (declare (session self) (wamp/message-type:message-t type) (list args))
  (the session-t
       (progn (transport-send (session-transport self) (cons type args))
              self)))

(defun _make-options (assoc)
  "Encodes an association list as a hash-table. Nil key-value tuples are discarded. 
   Used for encoding."
  (declare (list assoc))
  (the hash-table
       (let ((table (make-hash-table)))
         (loop for (key . value) in assoc
               when value do (setf (gethash key table) value))
         table)))

;; ++ Tests ++ 
