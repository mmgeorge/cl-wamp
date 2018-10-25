(defpackage :wamp/ws/server
  (:use :cl :alexandria :fast-http :quri)
  (:import-from :usocket)
  (:import-from :bordeaux-threads)
  (:import-from :fast-http)
  (:import-from :ironclad)
  (:import-from :cl-base64)
  (:import-from :wamp/ws/client)
  (:import-from :wamp/ws/protocol/http)
  (:import-from :wamp/ws/protocol/websocket)
  (:local-nicknames (:client :wamp/ws/client)
                    (:protocol/http :wamp/ws/protocol/http)
                    (:protocol/websocket :wamp/ws/protocol/websocket))
  (:export #:server))

(in-package :wamp/ws/server)

(defparameter %accept-key "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(defvar *log-output* *standard-output*)

;; conditions

(define-condition protocol-error (error)
  ((text :initarg :text :reader text)
   (client :initarg :client :reader client))
  (:report (lambda (condition stream)
             (format stream "[~{~a~^.~}:~a] Protocol violation: ~a~%"
                     (coerce (usocket:get-peer-address (client condition)) 'list)
                     (usocket:get-peer-port (client condition))
                     (text condition)))))



(define-condition auth-error (error)
  ((text :initarg :text :reader text)
   (client :initarg :client :reader client))
  (:report (lambda (condition stream)
             (format stream "[~{~a~^.~}:~a] Authorization error: ~a~%"
                     (coerce (usocket:get-peer-address (client condition)) 'list)
                     (usocket:get-peer-port (client condition))
                     (text condition)))))


(defclass server ()
  ((host :reader host :type 'string :initarg :host)
   (port :reader port :type 'fixnum :initarg :port)
   (path :reader path :type 'string :initarg :path)
   (origins :reader origins :type 'list :initarg :origins)
   (authority :accessor authority :type 'string :initarg :authority)
   ;; First socket denotes the master/acceptor socket
   (sockets :accessor sockets :initform nil)
   (thread :accessor thread :initform nil)
   ))


(defun make-server (url &key (host nil) (origins nil))
  (let ((uri (uri url)))
    (make-instance 'server :host (uri-host uri)
                           :port (uri-port uri)
                           :path (uri-path uri)
                           :origins origins
                           :authority host )))


;; Methods 

(defgeneric start (server))
(defgeneric stop (server))


(defmethod start ((self server))
  (let ((socket (create-socket (host self) (port self)))
        (os *standard-output*))
    (setf (sockets self) (list socket))
    (setf (thread self)
          (bt:make-thread (lambda () (let* ((*standard-output* os)) (poll self)) :name "acceptor")))
    self))


(defmethod stop ((self server))
  (when (running-p self)
    (when (bt:thread-alive-p (thread self))
      (bt:destroy-thread (thread self)))
    (loop for socket in (sockets self) do (usocket:socket-close socket))
    (setf (sockets self) nil))
  self)


(defun report (control-string &rest args)
  (when *log-output*
    (multiple-value-bind (second minute hour date month year) (get-decoded-time)
      (apply #'format *log-output*
             (concatenate 'string "~a/~a/~a ~2,'0d:~2,'0d:~2,'0d UTC " control-string "~%")
             (append (list hour minute second date month year) args)))))


(defun running-p (self)
  (> (length (sockets self)) 1))


(defun acceptor (self)
  (car (sockets self)))


;; Internal

(defun create-socket (host port)
  (usocket:socket-listen host port :element-type '(unsigned-byte 8) :reuse-address t))


;; for works a special debug print that reference *worker-number* of some sort ? 

;; todo - loop over clients - if exceeded handshake timeout then kill
(defun poll (self)
  (with-slots (sockets) self
    (loop
      (loop for socket in (usocket:wait-for-input sockets :ready-only t) do
        (if (eq socket (acceptor self))
            (push-client self (client:make-client (usocket:socket-accept socket)
                                                  (protocol/http:make-http)))
            (safe-handle-client self socket))))))


(defun safe-handle-client (self client)
  (handler-case (handle-client self client)
    (protocol-error (condition)
      (report "~a" condition)
      ))
  )


(defun handle-client (self client)
  (when-let ((data (client:recieve client)))
    (format t "GOT A MESSAGE .... HANDLING IT~%")
    (handle-message self client data)))


(defun handle-message (self client data)
  (ecase (type-of (client:protocol client))
     (protocol/http:http (process-handshake self client data))))
  

(defun write-to-stream (stream src &optional (start 0) end)
  (let ((end (or end (length src))))
    (loop for i from start to end
          for byte = (aref src i)
          do (write-byte byte stream))
    (force-output stream)))



(defun process-handshake (self client request)
  (let ((headers (http-headers request)))
    (when (and (check-protocol self client request headers)
               (check-auth self client request headers))
      (send-handshake client headers)
      (setf (client:protocol client) (protocol/websocket:make-websocket))
      )))


(defun send-handshake (client headers)
  (let* ((stream (client:socket-stream client))
        ;(protocols (gethash "sec-websocket-protocol" headers))
        ;(extensions (gethash "sec-websocket-extensions" headers))
        (nonce (gethash "sec-websocket-key" headers))
        (key (concatenate 'string nonce %accept-key))
         )

    ;; use flexi stream here? 
    
    (format stream "HTTP/1.1 101 Switching Protocols~c~c" #\return #\newline)
    (format stream "Upgrade: websocket~c~c" #\return #\newline)
    (format stream "Connection: Upgrade~c~c" #\return #\newline)
    (format stream "Sec-WebSocket-Accept: ")
    (format stream (base64:usb8-array-to-base64-string
                    (ironclad:digest-sequence :sha1 (ironclad:ascii-string-to-byte-array key))))
    (format stream " ~c~c~c~c" #\return #\newline #\return #\newline)
    (force-output stream)
    
    
    
  ))


;; -> nil | string
(defun expected-host-p (self header-host)
  (flet ((default-port-p (port) (or (eq port 80) (eq port 443))))
    (with-slots (authority port) self
      (or (not authority)
          (string-equal header-host 
                        (if (default-port-p port)
                            (format nil "~a" authority)
                            (format nil "~a:~a" authority port)))))))


;; See https://tools.ietf.org/html/rfc6455#section-4
(defun check-protocol (self client request headers)
  (flet ((perror (fmt &rest args)
           (error 'protocol-error :client client :text (apply #'format nil fmt args))))
    (let ((method (http-method request))
          (version (http-version request))
          (host (gethash "host" headers))
          (upgrade (gethash "upgrade" headers))
          (nonce (gethash "sec-websocket-key" headers))
          (ws-version (gethash "sec-websocket-version" headers))
          ;; optional
                                        ;(user-agent (gethash "user-agent" headers))
          )
      (cond ((not (eq method :get)) (perror "Invalid method ~a" method))
            ((not (>= version 1.1)) (perror "Invalid http version ~a" version))
            ((not (expected-host-p self host)) (perror "Unexpected host ~a host" host))
            ((not (string-equal upgrade "websocket")) (perror "Unexpected upgrade ~a" upgrade))
            ((not (eq ws-version 13)) (perror "Unsupported websocket version ~a" ws-version))
            ((null nonce) (perror "Unsupported websocket version ~a" ws-version))
            (t)))))


(defun valid-origin-p (self origin)
  (or (null (origins self))
      (member origin (origins self))))


(defun check-auth (self client request headers)
  (flet ((aerror (fmt &rest args)
           (error 'auth-error :client client :text (apply #'format nil fmt args))))
    (let ((origin (gethash "origin" headers))
          (path (uri-path (uri (http-resource request)))))
      (cond ((not (valid-origin-p self origin)) (aerror "Invalid origin ~a" origin))
            ((not (string-equal path (path self))) (aerror "Invalid path ~a" path))
            (t)))))


    ;;   (format t "hello world!")
  
;;     ;; change me!
;;     ;;; PASS IN ACTUAL BUF HERE...
;;     (funcall (parser client) data :end (client:len client) )
;;     (setf (client:len client) -1)

;;     (format t "~a" http)
;;     ;;(write-byte 101 (usocket:socket-stream client))
;;     ;;(force-output (usocket:socket-stream client))
;;     )

;; can we rebind standard-output for easier debuging? 


(defun process-websocket-frame (self client data)
  (declare (ignore self client data)))


(defun push-client (self client)
  (with-slots (sockets) self
    (push client (cdr (last sockets)))
    ))


(defvar *res* nil)
(defvar *server* nil)
(defvar *client* nil)
(defvar *client-real* nil)
(defvar *client-read-thread* nil)

(defun test ()
  (when *client-read-thread*
    (when (bt:thread-alive-p *client-read-thread*)
          (bt:destroy-thread *client-read-thread*))
    (setf *client-read-thread* nil))
  (when *client*
    (usocket:socket-close *client*)
    (setf *client* nil))
  (when *server*
    (stop *server*)
    (setf *server* nil))
  (sleep 0.150)
  (let ((os *standard-output*))
    (setf *server* (make-server "ws://0.0.0.0:8081/ws" :host "dev.owny.io"))
    (start *server*)
    (setf *client* (usocket:socket-connect "localhost" 8081 ;:element-type  '(unsigned-byte 8)
                                           ))
    (setf *client-read-thread*
          (bt:make-thread
           (lambda ()
             (loop
               (format os "~a" (read-char (usocket:socket-stream *client*) nil nil))))))
    *server*
    ))


(defun write-test-header ()
  (let ((stream (usocket:socket-stream *client*))
        (message (with-output-to-string (os)
                   (format os "GET /ws HTTP/1.1~c~c" #\Return #\NewLine)
                   (format os "Host: dev.owny.io:8081~c~c" #\Return #\NewLine)
                   (format os "Connection: Upgrade~c~c" #\Return #\NewLine)
                   (format os "Upgrade: websocket~c~c" #\Return #\NewLine)
                   (format os "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==~c~c" #\Return #\NewLine)
                   (format os "Sec-WebSocket-Version: 13~c~c~c~c" #\Return #\NewLine #\Return #\NewLine))))
    (format t "writing ~a~%" message)
    (loop for char across message do
      (write-char char stream))
    ;;(write-byte 0 stream)
    (force-output stream)))


(defun write-test-bytes ()
  (write-byte 10 (usocket:socket-stream *client*))
  (write-byte 0 (usocket:socket-stream *client*))
  (force-output (usocket:socket-stream *client*))
  )
