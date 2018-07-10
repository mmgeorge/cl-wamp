(defpackage :wamp/connection
  (:use :cl
   :wamp/transport
   :wamp/session)
  (:import-from :wamp/transport)
  (:import-from :wamp/session))

(in-package :wamp/connection)

(defclass connection ()
  ((transport :accessor connection-transport :initarg :transport :type transport)
   (sessions :reader connection-sessions :initform nil :type (list session))))

(defun make-connection (url)
  (declare (string url))
  (the (values connection &optional)
       (make-instance 'connection :transport (make-websocket url))))

(defun connection-add-session (self session)
  (declare (connection self) (session session))
  (the session
       (with-slots (sessions) self
         (progn (setf sessions (cons session session))
              session))))

(defun connection-create-session (self realm)
  (declare (connection self) (string realm))
  (the (values session &optional)
       (connection-add-session self (make-session (connection-transport self) realm))))


