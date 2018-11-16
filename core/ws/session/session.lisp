(defpackage :wamp/ws/session/session
  (:use :cl)
  (:import-from :usocket)
  (:import-from :fast-http)
  (:import-from :flexi-streams)
  (:export #:session #:recieve #:send #:send-error #:protocol #:socket-stream
           #:status #:stop
           #:upgrade-accept #:upgrade-request

           #:index #:buffer
           ))

(in-package :wamp/ws/session/session)


(defclass session (usocket:stream-usocket)
  ((socket-stream :reader socket-stream :initform nil)
   (status :accessor status :initform :open)
   (buffer :accessor buffer :initform nil)
   (index :accessor index :initform 0)
   ))


(defmethod initialize-instance :after ((self session) &key bufsize)
  (setf (slot-value self 'socket-stream)
        (flexi-streams:make-flexi-stream (usocket:socket-stream self) :external-format :utf-8))
  (setf (slot-value self 'buffer)
        (make-array bufsize :element-type '(unsigned-byte 8))))


(defmethod update-instance-for-different-class :after
    ((sock usocket:usocket) (self session) &key bufsize)
  (format t "update instance! ~a" bufsize)
  (unless (socket-stream self)
    (setf (slot-value self 'socket-stream)
          (flexi-streams:make-flexi-stream (usocket:socket-stream sock) :external-format :utf-8)))
                                        ;(Describe (slot-value self 'socket-stream))
  (unless (buffer self)
    (setf (slot-value self 'buffer)
          (make-array bufsize :element-type '(unsigned-byte 8)))))


(defgeneric upgrade-request (self))
(defgeneric upgrade-accept (self request))
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
