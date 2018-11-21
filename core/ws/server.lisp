(defpackage :wamp/ws/server
  (:use :cl :alexandria :fast-http :quri)
  (:import-from :usocket)
  (:import-from :bordeaux-threads)
  (:import-from :local-time)
  (:import-from :fast-http)
  (:import-from :ironclad)
  (:import-from :cl-base64)
  (:import-from :wamp/ws/session/session)
  (:import-from :wamp/ws/session/http)
  (:import-from :wamp/ws/session/websocket)
  (:local-nicknames (:session :wamp/ws/session/session)
                    (:session/http :wamp/ws/session/http)
                    (:session/websocket :wamp/ws/session/websocket))
  (:export #:server))

(in-package :wamp/ws/server)

(defvar *log-output* *standard-output*)


(define-condition protocol-error (error)
  ((text :initarg :text :reader text)
   (session :initarg :session :reader session))
  (:report (lambda (condition stream)
             (format stream "[~{~a~^.~}:~a] Protocol violation: ~a~%"
                     (coerce (usocket:get-peer-address (session condition)) 'list)
                     (usocket:get-peer-port (session condition))
                     (text condition)))))



(define-condition auth-error (error)
  ((text :initarg :text :reader text)
   (session :initarg :session :reader session))
  (:report (lambda (condition stream)
             (format stream "[~{~a~^.~}:~a] Authorization error: ~a~%"
                     (coerce (usocket:get-peer-address (session condition)) 'list)
                     (usocket:get-peer-port (session condition))
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
   (buffer-size :reader buffer-size :initarg :buffer-size)
   ))


(defun make-server (url &key (host nil) (origins nil) (buffer-size 2))
  "Create a new server instance. BUFFER-SIZE controls how large each session buffer will be,
   as long as the maximum length of a websocket message. Size is calculated as 2 ^ (9 + BUFFER-SIZE)"
  (check-type buffer-size (integer 0 15))
  (let ((uri (uri url))
        (buffer-size (expt 2 (+ 9 buffer-size))))
    (make-instance 'server :host (uri-host uri)
                           :port (uri-port uri)
                           :path (uri-path uri)
                           :origins origins
                           :authority host
                           :buffer-size buffer-size)))



;; (defun make-connect (host port &key bufsize)
;;   "Make a new session by connecting to the the target HOST on the given PORT"
;;   (let* ((socket (usocket:socket-connect host port))
;;          (session (make-session socket :bufsize bufsize)))
;;     (upgrade-request session)))


;; (defun make-accept (acceptor &key bufsize)
;;   "Make a new session by accepting a new socket for the given ACCEPTOR"
;;   (let ((socket (usocket:socket-accept acceptor)))
;;     (make-session socket :bufsize bufsize)))


;; Exports 

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

;; Internal

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


(defun create-socket (host port)
  (usocket:socket-listen host port :element-type '(unsigned-byte 8) :reuse-address t))


(defun make-session (socket bufsize)
  (format t "Accepted a new session ~a~%" socket)
  (change-class socket 'session/http:http :bufsize bufsize))


(defun push-session (self session)
  (with-slots (sockets) self
    (push session (cdr (last sockets)))))


;; for works a special debug print that reference *worker-number* of some sort ? 

;; todo - loop over sessions - if exceeded handshake timeout then kill
(defun poll (self)
  (with-slots (sockets) self
    (loop
      (loop for socket in (usocket:wait-for-input sockets :ready-only t) do
        (cond ((eq socket (acceptor self))
               (push-session self (make-session (usocket:socket-accept socket) (buffer-size self))))
              ((eq (session:status socket) :shutdown) (close-socket self socket))
              (t (safe-handle-session self socket)))))))


;; Dispatch message handling

(defun close-socket (self socket)
  (session:stop socket)
  (setf (sockets self)
        (delete socket (sockets self))))


(defun safe-handle-session (self session)
  (handler-case (handle-session self session)
    (protocol-error (condition)
      (report "~a" condition)
      (session:send session condition)
      (close-socket self session))
    (t (e)
      (report "Got an error ~a closing client" e)
      (close-socket self session))))


(defun handle-session (self session)
  ;; session:recieve returns nil or message when the message has been fully read
  (when-let ((data (session:recieve session)))
    ;;(format t "GOT A MESSAGE .... HANDLING IT~%")
    (handle-message self session data)))


(defun handle-message (self session data)
  (ecase (type-of session)
    (session/http:http (process-handshake self session data))
    (session/websocket:websocket (process-message self session data))))


(defun write-to-stream (stream src &optional (start 0) end)
  (let ((end (or end (length src))))
    (loop for i from start to end
          for byte = (aref src i)
          do (write-byte byte stream))
    (force-output stream)))


;; Handshake (http)

(defun process-handshake (self session request)
  (let ((headers (http-headers request)))
    (when (and (check-protocol self session request headers)
               (check-auth self session request headers))
      (session:upgrade-accept session request)
      (change-class session 'session/websocket:websocket)
      )))


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
(defun check-protocol (self session request headers)
  (flet ((perror (fmt &rest args)
           (error 'protocol-error :session session :text (apply #'format nil fmt args))))
    (let ((method (http-method request))
          (version (http-version request))
          (host (gethash "host" headers))
          (upgrade (gethash "upgrade" headers))
          (nonce (gethash "sec-websocket-key" headers))
          (ws-version (gethash "sec-websocket-version" headers))
          ;; optional
          ;;(user-agent (gethash "user-agent" headers))
          )
      (cond ((not (eq method :get)) (perror "Invalid method ~a" method))
            ((not (>= version 1.1)) (perror "Invalid http version ~a" version))
            ((not (expected-host-p self host)) (perror "Unexpected host ~a" host))
            ((not (string-equal upgrade "websocket")) (perror "Unexpected upgrade ~a" upgrade))
            ((not (eq ws-version 13)) (perror "Unsupported websocket version ~a" ws-version))
            ((null nonce) (perror "Unsupported websocket version ~a" ws-version))
            (t)))))


(defun valid-origin-p (self origin)
  (or (null (origins self))
      (member origin (origins self))))


(defun check-auth (self session request headers)
  (flet ((aerror (fmt &rest args)
           (error 'auth-error :session session :text (apply #'format nil fmt args))))
    (let ((origin (gethash "origin" headers))
          (path (uri-path (uri (http-resource request)))))
      (cond ((not (valid-origin-p self origin)) (aerror "Invalid origin ~a" origin))
            ((not (string-equal path (path self))) (aerror "Invalid path ~a" path))
            (t)))))


;; Websocket message handling

(defun process-message (self session message)
  (declare (ignore self))
  ;;(format t "procing message type ~a~%" (car message))
  (destructuring-bind (opsym data end) message
    (case opsym
      (:binary (session:send session data :start 0 :end end))
      (:text (progn
               (format t "Got message: ~a~%"
                       (flexi-streams:octets-to-string data :end end :external-format :utf-8))
               (session:send session data :start 0 :end end)))
      (:close (progn
                (format t "Shuting down socket~%")
                (usocket:socket-shutdown session :io)
                (setf (session:status session) :shutdown)))
      (:ping (session/websocket:pong session data :start 0 :end end))
      (:pong (format t "Got pong with body ~a (end:~a)~%" data end)))))



(defvar *res* nil)
(defvar *server* nil)
(defvar *session* nil)
(defvar *session-real* nil)
(defvar *session-read-thread* nil)

(defun test ()
  (when *session-read-thread*
    (when (bt:thread-alive-p *session-read-thread*)
          (bt:destroy-thread *session-read-thread*))
    (setf *session-read-thread* nil))
  (when *session*
    (usocket:socket-close *session*)
    (setf *session* nil))
  (when *server*
    (stop *server*)
    (setf *server* nil))
  (sleep 0.150)
  (let ((os *standard-output*))
    (setf *server* (make-server "ws://0.0.0.0:8081/ws" :host "dev.owny.io"))
    (start *server*)
    (setf *session* (usocket:socket-connect "localhost" 8081))
    (setf *session-read-thread*
          (bt:make-thread
           (lambda ()
             (loop
               (format os "~a" (read-char (usocket:socket-stream *session*) nil nil))))))
    *server*
    ))


(defun write-test-header ()
  (let ((stream (usocket:socket-stream *session*))
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
  (write-byte 10 (usocket:socket-stream *session*))
  (write-byte 0 (usocket:socket-stream *session*))
  (force-output (usocket:socket-stream *session*))
  )
