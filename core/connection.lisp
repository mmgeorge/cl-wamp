(defpackage :wamp/connection
  (:use :cl :wamp/transport )
  (:import-from :wamp/transport))

(in-package :wamp/connection)

(defclass connection ()
  ((transport :accessor connection-transport :initarg :transport :type transport)
   (realm :accessor connection-realm :initarg :realm :type string)))

(defun make-connection (url realm)
  (declare (string url realm))
  (the (values connection &optional)
       (make-instance 'connection :transport (make-websocket url) :realm realm)))

