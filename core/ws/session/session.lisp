(defpackage :wamp/ws/session/session
  (:use :cl)
  (:import-from :usocket)
  (:import-from :fast-http)
  (:import-from :flexi-streams)
  (:export #:session #:recieve #:send #:send-error #:protocol #:socket-stream
           #:status #:stop #:ping #:pong
           #:upgrade-accept #:upgrade-request))

(in-package :wamp/ws/session/session)


(defclass session (usocket:stream-usocket)
  ((socket-stream :reader socket-stream :initform nil)
   (status :accessor status :initform :open)
   (buffer :accessor buffer :initform nil)))


(defmethod initialize-instance :after ((self session) &key (bufsize (expt 2 (+ 9 15))))
  (setf (slot-value self 'socket-stream)
        (flexi-streams:make-flexi-stream (usocket:socket-stream self) :external-format :utf-8))
  (setf (slot-value self 'buffer)
        (make-array bufsize :element-type '(unsigned-byte 8))))


(defmethod update-instance-for-different-class :after
    ((sock usocket:usocket) (self session) &key (bufsize (expt 2 (+ 9 15))))
  (format t "MAKE IT!")
  (setf (slot-value self 'socket-stream)
        (flexi-streams:make-flexi-stream (usocket:socket-stream sock) :external-format :utf-8))
  ;(Describe (slot-value self 'socket-stream))
  (setf (slot-value self 'buffer)
        (make-array bufsize :element-type '(unsigned-byte 8))))


(defgeneric upgrade-request (self))
(defgeneric upgrade-accept (self))
(defgeneric recieve (self))
(defgeneric send (self data &key start end))

;; (defun send-error (self message)
;;   (protocol:send-error (protocol self) (socket-stream self) message))


(defun stop (self)
  (let ((stream (socket-stream self)))
    (loop while (listen stream)
          for byte = (read-byte stream nil nil)
          do (format t "Got terminating byte ~b~%" byte))
    (usocket:socket-close self)))
