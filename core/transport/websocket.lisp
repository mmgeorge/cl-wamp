(defpackage :wamp/transport/websocket
  (:nicknames :websocket)
  (:use :cl :wamp/message-type)
  (:import-from :cl-json)
  (:import-from :websocket-driver)
  (:import-from :wamp/message-type)
  (:import-from :wamp/transport/transport #:transport)
  (:import-from :wamp/decorators #:dtype)
  (:export #:websocket #:make-websocket))

(in-package :wamp/transport/websocket)


(defclass websocket (transport)
  ((client :accessor websocket-client :initarg :client :type function)
   (handle :accessor websocket-handle :initform nil :type function)))
           

#[(dtype (string &key (:on-open function) (:on-close function) (:on-message function) (:on-error function)) websocket)]
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
         (wsd:on :open client (lambda () (funcall (transport:on-open instance))))
         (wsd:on :close client (lambda (error) (funcall (transport:on-error error))))
         (wsd:on :message client (lambda (message) (-transport-handle-message instance message)))
         (wsd:on :error client  (lambda (&key code reason)
                                  (funcall (transport:on-message instance) :code code :reason reason)))
         (setf (websocket-client instance) client)
         instance)))


(defmethod transport:start ((self websocket))
  (the websocket
       (progn (wsd:start-connection (websocket-client self))
              self)))


(defmethod transport:stop ((self websocket))
  (the websocket
       (progn (wsd:close-connection (websocket-client self))
              self)))


(defmethod transport:send ((self websocket) (message list))
  (the websocket
       (progn (wsd:send-text (websocket-client self) (transport:serialize self message))
              self)))


(defmethod transport:mock ((self websocket) (message list))
  (funcall (transport:on-message self) (mtype:code-to-message-t (car message)) (cdr message)))


(defmethod transport:serialize ((self websocket) (message list))
  (the (values string &optional)
       (progn (rplaca message (wamp/message-type:message-t-to-code (car message)))
              (json:encode-json-to-string message))))


(defmethod transport:deserialize ((self websocket) (message string))
  (the list
       (let ((parsed (json:decode-json-from-string message)))
         (check-type parsed list "a well-formed wamp-message")
         (check-type (car parsed) fixnum "a message type")
         (rplaca parsed (wamp/message-type:code-to-message-t (car parsed)))
         parsed)))


;; ++ Internal ++

(defun -transport-handle-message (self message)
  (let ((message (handler-case (transport:deserialize self message)
                   ;; Ignore any malformed messages
                   (t () nil)) ))
    (when message
      (funcall (transport:on-message self) (car message) (cdr message)))))

