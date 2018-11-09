(defpackage :wamp/ws/protocol/http
  (:use :cl)
  (:import-from :fast-http)
  (:import-from :wamp/ws/protocol/base)
  (:import-from :local-time)
  (:local-nicknames (:protocol :wamp/ws/protocol/base))
  (:export #:http #:make-http))

(in-package :wamp/ws/protocol/http)


(define-condition http-error (error)
  ((text :initarg :text :reader text)
   (client :initarg :client :reader client))
  (:report (lambda (condition stream)
             (format stream "[~{~a~^.~}:~a] http error: ~a~%"
                     (coerce (usocket:get-peer-address (client condition)) 'list)
                     (usocket:get-peer-port (client condition))
                     (text condition)))))


(defclass http (protocol:protocol)
  ((buffer :accessor buffer :initform (make-array 1024 :element-type '(unsigned-byte 8)))
   (request :initform nil)
   (parser :initform nil)
   (recieved-p :accessor recieved-p :initform nil)
   ))


(defun make-http ()
  (make-instance 'http))


(defun request (self)
  (with-slots (request) self
    (or request (setf request (fast-http:make-http-request)))))


(defun parser (self)
  (with-slots (parser) self
    (flet ((cb () (setf (recieved-p self) t) (format t "GOT!! ~%")))
      (or parser
          (setf parser (fast-http:make-parser
                        (request self)
                        :header-callback (lambda (headers) (format t "GOT HEAD ~a~%" headers))
                        :finish-callback #'cb))))))


(defmethod protocol:recieve ((self http) stream)
  (with-accessors ((buffer buffer) (request request) (parser parser)) self
    (let ((length (buffered-read stream buffer 0)))
      (format t "GOT LENGTH OF ~a buf ~a" length buffer)
      (funcall parser buffer :end length)
      (format t "~a~%" (request self))
      (format t "HEAD: ~a ~%~% " (fast-http:http-headers (request self)))
      (and (recieved-p self)
           (yield-request self)))))


(defmethod protocol:send-error ((self http) stream message)
  (format stream "HTTP/1.1 400 Bad Request~c~c" #\return #\newline)
  (format stream "Server: cl-wamp~c~c" #\return #\newline)
  (format stream "Date: " )
  (local-time:format-timestring stream (local-time:now) :format local-time:+rfc-1123-format+)
  (format stream "~c~c" #\return #\newline)
  (format stream "Content-Length: ~a~c~c" (length message) #\return #\newline)
  (format stream "Content-Type: text/plain; charset=utf-8~c~c" #\return #\newline)
  (format stream "~c~c" #\return #\newline)
  (format stream "~a" message)
  (force-output stream))


(defun yield-request (self )
  (let ((request (request self)))
    (setf (recieved-p self) nil)
    (setf (slot-value self 'request) nil)
    request))


(defun buffered-read (stream buffer start)
  (loop while (listen stream)
        for i from start to (length buffer)
        for byte =  (read-byte stream nil nil)
        do (setf (aref buffer i) byte)
        finally (return (1+ i))))

