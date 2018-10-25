(defpackage :wamp/ws/client
  (:use :cl)
  (:import-from :usocket)
  (:import-from :fast-http)
  (:import-from :flexi-streams)
  (:import-from :wamp/ws/protocol/base)
  (:local-nicknames (:protocol :wamp/ws/protocol/base))
  (:export #:client #:make-client #:recieve #:protocol #:socket-stream))

(in-package :wamp/ws/client)


(defclass client (usocket:stream-usocket)
  ((protocol :accessor protocol :initarg :protocol :type 'protocol:protocol )
   (socket-stream :accessor socket-stream :initarg :socket-stream)
   ))


(defun make-client (socket protocol)
  (let ((stream (flexi-streams:make-flexi-stream (usocket:socket-stream socket) :external-format :utf-8)))
    (change-class socket 'client :protocol protocol :socket-stream stream )))
  

;(defun socket-stream (self)
  ;(usocket:socket-stream self))


;; or can we pass down stateless information of some sort? i.e. ip address? 
(defun recieve (self)
  (protocol:recieve (protocol self) (socket-stream self)))



