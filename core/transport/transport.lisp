(defpackage :wamp/transport/transport
  (:use :cl :wamp/message-type)
  (:nicknames :transport)
  (:import-from :wamp/message-type)
  (:import-from :websocket-driver)
  (:import-from :cl-json)
  (:import-from :parachute)
  (:export #:transport #:make-websocket
           #:start #:stop #:send #:mock
           #:on-open #:on-close #:on-message #:on-error
           #:serialize #:deserialize))

(in-package :wamp/transport/transport)

(defclass transport () 
  ((on-open :accessor on-open :initarg :on-open :type function)
   (on-close :accessor on-close :initarg :on-close :type function)
   (on-message :accessor on-message :initarg :on-message :type function)
   (on-error :accessor on-error :initarg :on-error :type function))
   (:default-initargs :on-open (lambda () ())
                      :on-close  (lambda () ())
                      :on-message (lambda () ())
                      :on-error (lambda () ())))

;; MG: Add verbose before/after logging ?

(defgeneric start (transport))
(defgeneric stop (transport))
(defgeneric send (transport message))
(defgeneric mock (transport message)
  (:documentation "Send a mock message that will be 'recieved' by the transport. Useful for testing"))

(defgeneric serialize (transport message)
  (:documentation "Serialize the given message. Currently only json is supported"))

(defgeneric deserialize (transport stream)
  (:documentation "Deserialize the given message which is either json or msgpack encoded. 
                   Currently only json is supported"))

(defclass websocket (transport)
  ((client :accessor websocket-client :initarg :client :type function)
   (handle :accessor websocket-handle :initform nil :type function)))
           
