(defpackage :wamp/ws/session/http
  (:use :cl :wamp/ws/session/session)
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
  ((message :accessor message :initform nil)
   (request :accessor request :initform nil :documentation "Current outstanding request")
   (parser :initform nil)
   (recieved-p :accessor recieved-p :initform nil)
   (upgrade-key :accessor upgrade-key :initform nil)))


(defun parser (self)
  (with-slots (parser) self
    (flet ((cb () (setf (recieved-p self) t)))
      (or parser
          (let ((message (if (upgrade-key self)
                             (fast-http:make-http-response)
                             (fast-http:make-http-request))))
            (setf (request self) message)
            (setf (message self) message)
            (setf parser (fast-http:make-parser
                          message
                          :body-callback (lambda (data start end)
                                           (let* ((chunk-len (- end start)))
                                             (replace (buffer self) data :start1 (index self)
                                                                         :start2 start :end2 end)
                                             (setf (index self)
                                                   (+ (index self) chunk-len))))
                          :finish-callback #'cb)))))))

;; Exports


(defmethod session:recieve ((self http))
  (with-accessors ((stream session::socket-stream)) self
    (cond ((upgrade-key self)
           (read-socket self stream))
          (t (read-socket self stream)))))


(defmethod session:upgrade-request ((self http) &key host port)
  (with-accessors ((stream session::socket-stream)) self
    (format stream "GET /ws HTTP/1.1~c~c" #\return #\newline)
    (format stream "Host: ~a:~a~c~c" host port #\return #\newline)
    (format stream "Upgrade: websocket~c~c" #\return #\newline)
    (format stream "Connection: Upgrade~c~c" #\return #\newline)
    (format stream "Sec-WebSocket-Key: ~a~c~c" (generate-nonce self) #\return #\newline)
    (format stream "Sec-WebSocket-Version: 13~c~c" #\return #\newline)
    (format stream "~c~c" #\return #\newline)
    (force-output stream)))



(defmethod session:upgrade-accept ((self http) request)
  (with-accessors ((stream session::socket-stream)) self
    (let* ((headers (fast-http:http-headers request))
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


(defmethod session:send-text ((self http) (condition error) &key start end)
  (declare (ignore start end))
  (with-accessors ((stream session::socket-stream)) self
    (let ((message (format nil "~a" condition)))
      (format stream "HTTP/1.1 400 Bad Request~c~c" #\return #\newline)
      (format stream "Server: cl-wamp~c~c" #\return #\newline)
      (format stream "Date: " )
      (local-time:format-timestring stream (local-time:now) :format local-time:+rfc-1123-format+)
      (format stream "~c~c" #\return #\newline)
      (format stream "Content-Length: ~a~c~c" (length message) #\return #\newline)
      (format stream "Content-Language: en~c~c" #\return #\newline)
      (format stream "Content-Type: text/plain; charset=utf-8~c~c" #\return #\newline)
      (format stream "~c~c" #\return #\newline)
      (format stream "~a" message)
      (force-output stream))))


;; Internal

(defun read-socket (self stream)
  (with-accessors ((buffer session::buffer) (request request) (parser parser)) self
    (let ((length (buffered-read stream buffer 0)))
      (funcall parser buffer :end length)
      (and (recieved-p self)
           (yield self)))))


(defun generate-nonce (self)
  (setf (upgrade-key self)
        (base64:usb8-array-to-base64-string (ironclad:make-random-salt 16))))


(defun upgrade-request-p (request)
  (let ((headers (fast-http:http-headers request)))
    (gethash "upgrade" headers)))


(defun yield (self )
  (let ((message (message self))
        (index (index self)))
    (setf (recieved-p self) nil)
    (setf (slot-value self 'parser) nil)
    (setf (slot-value self 'message) nil)
    (setf (slot-value self 'index) 0)
  (if (> index 0)
      (values message (buffer self) 0 index)
      message)))


(defun buffered-read (stream buffer start)
  (read-sequence buffer stream :start start))
  
  ;; (loop while (listen stream)
  ;;       for i from start to (length buffer)
  ;;       for byte =  (read-byte stream nil nil)
  ;;       do (setf (aref buffer i) byte)
  ;;       finally (return (1+ i))))

