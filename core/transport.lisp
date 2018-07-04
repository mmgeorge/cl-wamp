(defpackage :wamp/core/transport
  (:use :cl)
  (:import-from :websocket-driver)
  (:import-from :clack))

(in-package :wamp/core/transport)

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
  ((server :accessor websocket-server :initarg :server :type function)
   (handle :accessor websocket-handle :initform nil :type function)))


(defun make-websocket (&key
                         (on-open (lambda () ()))
                         (on-close (lambda (&key code reason) (declare (ignore code reason))))
                         (on-message (lambda (message) (declare (ignore message))))
                         (on-error (lambda (error) (declare (ignore error)))))
  "Create a new websocket transport instance"
  (declare (function on-open on-close on-message on-error))
  (the websocket
       (let* ((instance (make-instance 'websocket :on-open on-open
                                                  :on-close on-close
                                                  :on-message on-message
                                                  :on-error on-error))
              (handle-open (lambda () (funcall (transport-on-open instance))))
              (handle-close (lambda (error) (funcall (transport-on-error error))))
              (handle-message (lambda (message) (funcall (transport-on-message instance) message)))
              (handle-error (lambda (&key code reason) (funcall (transport-on-message instance) :code code :reason reason)))
              (server (lambda (env) (let ((ws (wsd:make-server env)))
                                      (wsd:on :open ws handle-open)
                                      (wsd:on :close ws handle-close)
                                      (wsd:on :message ws handle-message)
                                      (wsd:on :error ws handle-error)
                                      (lambda (responder)
                                        (declare (ignore responder))
                                        (wsd:start-connection ws))))))
              (setf (websocket-server instance) server)
              instance)))

(defmethod transport-open ((self websocket) (port integer))
  (declare (integer port))
  (the websocket
       (with-slots (handle server) self
         (if handle (error "Cannot open a transport that is already open!")
             (progn
               (setf handle (clack:clackup server :server :wookie :port port))
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
       (progn (wsd:send-text (websocket-server self) message)
              self)))
