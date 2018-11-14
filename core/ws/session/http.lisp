(defpackage :wamp/ws/session/http
  (:use :cl)
  (:import-from :fast-http)
  (:import-from :local-time)
  (:import-from :ironclad)
  (:import-from :cl-base64)
  (:import-from :wamp/ws/session/session #:session)
  (:local-nicknames (:session :wamp/ws/session/session))
  (:export #:http #:make-http))

(in-package :wamp/ws/session/http)


(defparameter %accept-key "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")


(defclass http (session)
  ((request :initform nil)
   (parser :initform nil)
   (recieved-p :accessor recieved-p :initform nil)
   (upgrade-request :accessor upgrade-request :initform nil)
   (upgrade-key :accessor upgrade-key :initform nil)))


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


;; Exports

(defmethod session:recieve ((self http))
  (with-slots ((stream session::socket-stream)) self
    (cond ((upgrade-request self) (error "Outstanding upgrade request"))
          ((upgrade-key self) (error "has key"))
          (t (read-socket self stream)))))


(defmethod session:upgrade-request ((self http))
  (with-slots ((stream session::socket-stream)) self
    (format stream "GET /ws HTTP/1.1~c~c" #\return #\newline)
    (format stream "Host: owny.dev.io~c~c" #\return #\newline)
    (format stream "Upgrade: websocket~c~c" #\return #\newline)
    (format stream "Connection: Upgrade~c~c" #\return #\newline)
    (format stream "Sec-WebSocket-Key: ~a~c~c" (generate-nonce self) #\return #\newline)
    (format stream "Sec-WebSocket-Version: 13~c~c" #\return #\newline)
    (format stream "~c~c" #\return #\newline)
    (force-output stream)))


(defmethod session:upgrade-accept ((self http))
  (with-slots ((stream session::socket-stream)) self
      (unless (upgrade-request self)
        (error "Cannot accept upgrade request - no upgrade request found"))
    (let* ((headers (fast-http:http-headers (upgrade-request self)))
           (nonce (gethash "sec-websocket-key" headers))
           (key (concatenate 'string nonce %accept-key))
           ;;(protocols (gethash "sec-websocket-protocol" headers))
           ;;(extensions (gethash "sec-websocket-extensions" headers))
           )
      (format stream "HTTP/1.1 101 Switching Protocols~c~c" #\return #\newline)
      (format stream "Upgrade: websocket~c~c" #\return #\newline)
      (format stream "Connection: Upgrade~c~c" #\return #\newline)
      (format stream "Sec-WebSocket-Accept: ")
      (format stream (base64:usb8-array-to-base64-string
                      (ironclad:digest-sequence :sha1 (ironclad:ascii-string-to-byte-array key))))
      (format stream " ~c~c~c~c" #\return #\newline #\return #\newline)
      (force-output stream)
      t)))


;; (defmethod protocol:send-error ((self http) stream message)
;;   (format stream "HTTP/1.1 400 Bad Request~c~c" #\return #\newline)
;;   (format stream "Server: cl-wamp~c~c" #\return #\newline)
;;   (format stream "Date: " )
;;   (local-time:format-timestring stream (local-time:now) :format local-time:+rfc-1123-format+)
;;   (format stream "~c~c" #\return #\newline)
;;   (format stream "Content-Length: ~a~c~c" (length message) #\return #\newline)
;;   (format stream "Content-Type: text/plain; charset=utf-8~c~c" #\return #\newline)
;;   (format stream "~c~c" #\return #\newline)
;;   (format stream "~a" message)
;;   (force-output stream))

;; Internal

(defun read-socket (self stream)
  (with-accessors ((buffer session::buffer) (request request) (parser parser)) self
    (let ((length (buffered-read stream buffer 0)))
      (format t "GOT LENGTH OF ~a buf ~a" length buffer)
      (funcall parser buffer :end length)
      (format t "~a~%" (request self))
      (format t "HEAD: ~a ~%~% " (fast-http:http-headers (request self)))
      (and (recieved-p self)
           (yield-request self)))))


(defun generate-nonce (self)
  (setf (upgrade-key self)
        (base64:usb8-array-to-base64-string (ironclad:make-random-salt 16))))


(defun upgrade-request-p (request)
  (let ((headers (fast-http:http-headers request)))
    (gethash "upgrade" headers)))


(defun yield-request (self )
  (let ((request (request self)))
    (setf (recieved-p self) nil)
    (setf (slot-value self 'request) nil)
    (when (upgrade-request-p request)
      (setf (upgrade-request self) request))
    request))


(defun buffered-read (stream buffer start)
  (loop while (listen stream)
        for i from start to (length buffer)
        for byte =  (read-byte stream nil nil)
        do (setf (aref buffer i) byte)
        finally (return (1+ i))))

