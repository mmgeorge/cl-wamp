(defpackage :wamp/transport
  (:use :cl :wamp/message-type)
  (:import-from :wamp/message-type)
  (:import-from :websocket-driver)
  (:import-from :cl-json)
  (:import-from :parachute)
  (:export #:transport #:make-websocket
           #:transport-open #:transport-close #:transport-send #:transport-mock
           #:transport-on-open #:transport-on-close #:transport-on-message #:transport-on-error
           #:transport-serialize #:transport-deserialize))

(in-package :wamp/transport)

(defclass transport () 
  ((on-open :accessor transport-on-open :initarg :on-open :type function)
   (on-close :accessor transport-on-close :initarg :on-close :type function)
   (on-message :accessor transport-on-message :initarg :on-message :type function)
   (on-error :accessor transport-on-error :initarg :on-error :type function))
   (:default-initargs :on-open (lambda () ())
                      :on-close  (lambda () ())
                      :on-message (lambda () ())
                      :on-error (lambda () ())))

;; MG: Add verbose before/after logging ?

(defgeneric transport-open (transport))
(defgeneric transport-close (transport))
(defgeneric transport-send (transport message))
(defgeneric transport-mock (transport message)
  (:documentation "Send a mock message that will be 'recieved' by the transport. Useful for testing"))

(defgeneric transport-serialize (transport message)
  (:documentation "Serialize the given message. Currently only json is supported"))

(defgeneric transport-deserialize (transport stream)
  (:documentation "Deserialize the given message which is either json or msgpack encoded. 
                   Currently only json is supported"))

(defclass websocket (transport)
  ((client :accessor websocket-client :initarg :client :type function)
   (handle :accessor websocket-handle :initform nil :type function)))
           

(defun make-websocket (url &key
                         (on-open (lambda () ()))
                         (on-close (lambda (&key code reason) (declare (ignore code reason))))
                         (on-message (lambda (type args) (declare (ignore type args))))
                         (on-error (lambda (error) (declare (ignore error)))))
  "Create a new websocket transport instance"
  (declare (string url) (function on-open on-close on-message on-error))
  (the websocket
       (let* ((instance (make-instance 'websocket :on-open on-open
                                                  :on-close on-close
                                                  :on-message on-message
                                                  :on-error on-error))
              (client (wsd:make-client url :accept-protocols '("wamp.2.json"))))
         (wsd:on :open client (lambda () (funcall (transport-on-open instance))))
         (wsd:on :close client (lambda (error) (funcall (transport-on-error error))))
         (wsd:on :message client (lambda (message) (-transport-handle-message instance message)))
         (wsd:on :error client  (lambda (&key code reason)
                                  (funcall (transport-on-message instance) :code code :reason reason)))
         (setf (websocket-client instance) client)
         instance)))


(defmethod transport-open ((self websocket))
  (the websocket
       (progn (wsd:start-connection (websocket-client self))
              self)))


(defmethod transport-close ((self websocket))
  (the websocket
       (progn (wsd:close-connection (websocket-client self))
              self)))


(defmethod transport-send ((self websocket) (message list))
  (the websocket
       (progn (wsd:send-text (websocket-client self) (transport-serialize self message))
              self)))


(defmethod transport-mock ((self websocket) (message list))
  (funcall (transport-on-message self) (mtype:code-to-message-t (car message)) (cdr message)))


(defmethod transport-serialize ((self websocket) (message list))
  (the (values string &optional)
       (progn (rplaca message (wamp/message-type:message-t-to-code (car message)))
              (json:encode-json-to-string message))))


(defmethod transport-deserialize ((self websocket) (message string))
  (the list
       (let ((parsed (json:decode-json-from-string message)))
         (check-type parsed list "a well-formed wamp-message")
         (check-type (car parsed) fixnum "a message type")
         (rplaca parsed (wamp/message-type:code-to-message-t (car parsed)))
         parsed)))


(defmethod transport-deserialize ((self websocket) (message array))
  (the list
       (error "no impl!")))


;; ++ Internal ++

(defun -transport-handle-message (self message)
  (declare (transport self) ((or string (unsigned-byte 8)) message))
  (let ((message (handler-case (transport-deserialize self message)
                   ;; Ignore any malformed messages
                   (t () nil)) ))
    (when message
      (funcall (transport-on-message self) (car message) (cdr message)))))

