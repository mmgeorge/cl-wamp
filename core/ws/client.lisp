(defpackage :wamp/ws/client
  (:use :cl)
  (:import-from :usocket)
  (:import-from :fast-http)
  (:import-from :flexi-streams)
  (:import-from :wamp/ws/protocol/base)
  (:import-from :wamp/ws/protocol/websocket)
  (:local-nicknames (:protocol :wamp/ws/protocol/base)
                    (:websocket :wamp/ws/protocol/websocket))
  (:export #:client #:make-client #:recieve #:send #:send-error #:protocol #:socket-stream
           #:status #:stop #:ping))

(in-package :wamp/ws/client)


(defclass client (usocket:stream-usocket)
  ((protocol :accessor protocol :initarg :protocol :type 'protocol:protocol )
   (socket-stream :accessor socket-stream :initarg :socket-stream)
   (status :accessor status :initform :open)
   ))


(defun make-client (socket protocol)
  (let ((stream (flexi-streams:make-flexi-stream (usocket:socket-stream socket) :external-format :utf-8)))
    (change-class socket 'client :protocol protocol :socket-stream stream)))


;; or can we pass down stateless information of some sort? i.e. ip address? 
(defun recieve (self)
  (protocol:recieve (protocol self) (socket-stream self)))


(defun send (self data &key (start 0) (end (length data)))
  (protocol:send (protocol self) (socket-stream self) data :start start :end end))


(defun send-error (self message)
  (protocol:send-error (protocol self) (socket-stream self) message))


(defun ping (self buffer &key start end)
  (websocket:ping (protocol self) (socket-stream self) buffer :start start :end end))

(defun pong (self buffer &key start end)
  (websocket:pong (protocol self) (socket-stream self) buffer :start start :end end))


(defun stop (self)
  (let ((stream (socket-stream self)))
    (loop while (listen stream)
          for byte = (read-byte stream nil nil)
          do (format t "Got terminating byte ~b~%" byte))
    (usocket:socket-close self)))
