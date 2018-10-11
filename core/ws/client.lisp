(defpackage :wamp/ws/client
  (:use :cl)
  (:import-from :usocket)
  (:export #:client #:make-client #:read-input #:protocol))

(in-package :wamp/ws/client)


(defclass client (usocket:stream-usocket)
  ((buffer :accessor buffer :initform (make-array 0 :fill-pointer t :element-type '(unsigned-byte 8)))
   (protocol :reader protocol :type '(or :http :websocket) :initform :http)))


(defun make-client (socket)
  (change-class socket 'client))


(defun read-input (self)
  (let* ((stream (usocket:socket-stream self))
         (buffer (buffer self))
         (finished (buffered-read stream buffer)))
    (when finished
      ;;(loop for byte across buffer do (write-byte byte stream))
      ;;(force-output stream)
      (buffer-reset self)
      buffer)))


(defun buffer-reset (self)
  (setf (buffer self) (make-array 0 :fill-pointer t :element-type '(unsigned-byte 8))))


(defun buffered-read (stream buffer &optional (end-char 0))
  (loop while (listen stream)
        for byte = (read-byte stream nil nil)
        when (eq byte end-char) do
          (return t)
        do (vector-push-extend byte buffer)))
