(defpackage :wamp/connection
  (:nicknames #:connection)
  (:use :cl)
  (:import-from :wamp/transport/websocket)
  (:import-from :wamp/transport/transport)
  (:import-from :wamp/session #:session)
  (:import-from :wamp/util #:promise-of #:local-nicknames)
  (:import-from :wamp/decorators #:dtype))

(in-package :wamp/connection)

(local-nicknames :wamp/session :session)

(defclass connection ()
  ((transport :accessor transport :initarg :transport :type transport)
   (sessions :reader sessions :initform nil :type (list session))))


#[(dtype (string) connection)]
(defun make-connection (url)
  "Make a new connection instance"
  (let ((transport (transport:start (websocket:make-websocket url))))
    (make-instance 'connection :transport transport)))


#[(dtype (connection string) (promise-of session))]
(defun create-session (self realm)
  (session:start (-add-session self (session:make-session (transport self) realm))))


; ++ Internal ++
#[(dtype (connection session) session)]
(defun -add-session (self session)
  (with-slots (sessions) self
    (setf sessions (cons session session))
    session))
