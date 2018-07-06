(defpackage :wamp/transport
  (:use :cl)
  (:import-from :websocket-driver)
  (:import-from :clack)
  (:export #:make-websocket #:transport-open #:transport-close #:transport-send))

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

(defgeneric transport-open (transport port))
(defgeneric transport-close (transport))
(defgeneric transport-send (transport message))

(defmethod transport-open :before (transport (port integer))
  (check-type port (integer 0 65535)))

(defclass websocket (transport)
  ((client :accessor websocket-client :initarg :client :type function)
   (handle :accessor websocket-handle :initform nil :type function)))


(defun make-websocket (url &key
                         (on-open (lambda () ()))
                         (on-close (lambda (&key code reason) (declare (ignore code reason))))
                         (on-message (lambda (message) (declare (ignore message))))
                         (on-error (lambda (error) (declare (ignore error)))))
  "Create a new websocket transport instance"
  (declare (string url) (function on-open on-close on-message on-error))
  (the websocket
       (let* ((instance (make-instance 'websocket :on-open on-open
                                                  :on-close on-close
                                                  :on-message on-message
                                                  :on-error on-error))
              (client (wsd:make-client url)))
         (wsd:on :open client (lambda () (funcall (transport-on-open instance))))
         (wsd:on :close client (lambda (error) (funcall (transport-on-error error))))
         (wsd:on :message client (lambda (message) (funcall (transport-on-message instance) message)))
         (wsd:on :error client  (lambda (&key code reason)
                                  (funcall (transport-on-message instance) :code code :reason reason)))
         (setf (websocket-client instance) client)
         instance)))

(defmethod transport-open ((self websocket) (port integer))
  (declare (integer port))
  (the websocket
       (with-slots (handle client) self
         (if handle (error "Cannot open a transport that is already open!")
             (progn
               (setf handle (clack:clackup client :client :wookie :port port))
                self)))))


(defmethod transport-close ((self websocket))
  (the websocket
       (with-slots (handle) self
         (if (not handle) (error "Cannot close a transport that is not open!")
             (progn (clack:stop handle)
                    (setf handle nil)
                    self)))))


(defmethod transport-send ((self websocket) (message string))
  (the websocket
       (progn (wsd:send-text (websocket-client self) message)
              self)))
