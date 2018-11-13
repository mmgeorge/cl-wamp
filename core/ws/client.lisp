(defpackage :wamp/ws/client
  (:use :cl)
  (:import-from :usocket)
  (:import-from :fast-http)
  (:import-from :flexi-streams)
  (:import-from :wamp/ws/protocol/base)
  (:import-from :wamp/ws/protocol/websocket)
  (:import-from :wamp/ws/protocol/http)
  (:local-nicknames (:protocol :wamp/ws/protocol/base)
                    (:http :wamp/ws/protocol/http)
                    (:websocket :wamp/ws/protocol/websocket))
  (:export #:client #:make-client #:recieve #:send #:send-error #:protocol #:socket-stream
           #:status #:stop #:ping #:pong

           #:upgrade-accept #:upgrade-request))

(in-package :wamp/ws/client)


(defparameter %accept-key "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")


(defclass client (usocket:stream-usocket)
  ((protocol :accessor protocol :initarg :protocol :type 'protocol:protocol )
   (socket-stream :accessor socket-stream :initarg :socket-stream)
   (status :accessor status :initform :open)
   ))


(defun make-client (socket protocol)
  (let ((stream (flexi-streams:make-flexi-stream (usocket:socket-stream socket) :external-format :utf-8)))
    (change-class socket 'client :protocol protocol :socket-stream stream)))


(defun make-connect (host port)
  "Make a new client by connecting to the the target HOST on the given PORT"
  (let* ((socket (usocket:socket-connect host port))
        (protocol (http:make-http))
        (client (make-client socket protocol)))
    (upgrade-request client 'websocket:websocket)))


(defun make-accept (acceptor)
  "Make a new client by accepting a new socket for the given ACCEPTOR"
  (let ((socket (usocket:socket-accept acceptor))
        (protocol (http:make-http)))
    (make-client socket protocol)))


(defgeneric upgrade-request (self protocol &rest initargs))
(defgeneric upgrade-accept (self protocol &rest initargs))
;(defgeneric send ())
;(defgeneric recv ())

(defmethod upgrade-request (self protocol &rest initargs)
  (when (protocol:upgrade-request (protocol self) (socket-stream self))
    (setf (protocol self)
          (apply #'make-instance protocol initargs))))


(defmethod upgrade-accept (self protocol &rest initargs)
  (when (protocol:upgrade-accept (protocol self) (socket-stream self))
    (format t "setting proto to ~a " protocol)
    (setf (protocol self)
          (apply #'make-instance protocol initargs))))


;; or can we pass down stateless information of some sort? i.e. ip address? 
(defun recieve (self)
  (protocol:recieve (protocol self) (socket-stream self))

  )


;; for user override
;;(defmethod handle-message ((self client)))

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
