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



(defconstant %DEBUG_PRINT% t)

(defmacro debug-print (control &rest args)
  (when %DEBUG_PRINT%
    `(progn
       (format t "wamp/transport/websocket:")
       (funcall #'format ,@(cons t (cons control args)))
       (format t "~%"))))


(defclass websocket (transport)
  ((url :reader websocket-url :initarg :url :type string)
   (client :accessor websocket-client :initarg :client :type function)
   (handle :accessor websocket-handle :initform nil :type function)))


#[(dtype (string &key (:on-open function) (:on-close function) (:on-message function) (:on-error function)) websocket)]
(defun make-websocket (url &key
                         (on-open (lambda () ()))
                         (on-close #'-handle-close)
                         (on-message (lambda (type args) (declare (ignore type args))))
                         (on-error (lambda (error) (declare (ignore error)))))
  "Create a new websocket transport instance"
  (declare (string url) (function on-open on-close on-message on-error))
  (the websocket
       (let* ((instance (make-instance 'websocket :url url
                                                  :on-open on-open
                                                  :on-close on-close
                                                  :on-message on-message
                                                  :on-error on-error))
              (client (wsd:make-client url :accept-protocols '("wamp.2.json"))))
         (wsd:on :open client (lambda () (funcall (transport:on-open instance))))
         (wsd:on :error client (lambda (error) (funcall (transport:on-error error))))
         (wsd:on :message client (lambda (message) (-transport-handle-message instance message)))
         (wsd:on :close client  (lambda (&key code reason)
                                  (-handle-close :code code :reason reason)
                                  (funcall (transport:on-close instance) :code code :reason reason)))
         (setf (websocket-client instance) client)
         instance)))


#[(dtype (&key (:code (or null fixnum)) (:reason t)) null)]
(defun -handle-close (&key code reason)
  (declare (ignore reason))
  (debug-print "close: Transport closing code: ~a" code))


(defmethod transport:start ((self websocket))
  (debug-print "start: Starting transport on ~a" (websocket-url self))
  (the websocket
       (progn (wsd:start-connection (websocket-client self))
              self)))


(defmethod transport:stop ((self websocket))
  (debug-print "stop: Stopping transport")
  (the websocket
       (progn (wsd:close-connection (websocket-client self))
              self)))


(defmethod transport:send ((self websocket) (message list))
  (debug-print "send: Sending message: ~a" message)
  (let ((serialized (transport:serialize self message)))
    (debug-print "send: Serialized message as: ~a" serialized)
    (wsd:send-text (websocket-client self) serialized)))


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
  (debug-print "send: Recieved message: ~a" message)
  (let ((message (handler-case (transport:deserialize self message)
                   ;; Ignore any malformed messages
                   (t () nil)) ))
    (when message
      (debug-print "send: Decoded message: ~a" (car message))
      (funcall (transport:on-message self) (car message) (cdr message)))))

