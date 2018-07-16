(defpackage :wamp/connection
  (:use :cl
   :wamp/transport
   :wamp/session)
  (:import-from :wamp/transport)
  (:import-from :wamp/session))


(in-package :wamp/connection)


(deftype connection-t () '(values connection &optional))


(defclass connection ()
  ((transport :accessor connection-transport :initarg :transport :type transport)
   (sessions :reader connection-sessions :initform nil :type (list session))))


(defun make-connection (url)
  (declare (string url))
  (the connection-t
       (let ((transport (transport-open (make-websocket url))))
         (make-instance 'connection :transport transport))))


(defun connection-create-session (self realm)
  (declare (connection self) (string realm))
  (the session-t
       (session-open (-connection-add-session self (make-session (connection-transport self) realm)))))


;; ++ Internal ++


(defun -connection-add-session (self session)
  (declare (connection self) (session session))
  (the session
       (with-slots (sessions) self
         (progn (setf sessions (cons session session))
              session))))

