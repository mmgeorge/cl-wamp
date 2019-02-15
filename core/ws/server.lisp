(defpackage :wamp/ws/server
  (:use :cl :alexandria :fast-http :quri)
  (:import-from :usocket)
  (:import-from :bordeaux-threads)
  (:import-from :local-time)
  (:import-from :fast-http)
  (:import-from :ironclad)
  (:import-from :cl-base64)
  (:import-from :wamp/ws/conditions #:protocol-error #:auth-error #:connection-error)
  (:import-from :wamp/ws/session/session)
  (:import-from :wamp/ws/session/http)
  (:import-from :wamp/ws/session/websocket)
  (:import-from :wamp/ws/evented #:evented)
  (:local-nicknames (:session :wamp/ws/session/session)
                    (:session/http :wamp/ws/session/http)
                    (:session/websocket :wamp/ws/session/websocket)
                    (:evented :wamp/ws/evented))
  (:export #:server #:make-server #:start #:stop #:sockets))

(in-package :wamp/ws/server)


(defvar *log-output* *standard-output*)


(defclass server (evented)
  ((host :reader host :type 'string :initarg :host)
   (port :reader port :type 'fixnum :initarg :port)
   (path :reader path :type 'string :initarg :path)
   (origins :reader origins :type 'list :initarg :origins)
   (authority :accessor authority :type 'string :initarg :authority)
   ;; First socket denotes the master/acceptor socket
   (sockets :accessor sockets :initform nil)
   (thread :accessor thread :initform nil)
   (tcp-server :accessor tcp-server :initform nil)
   
   (main-thread :reader main-thread :initform (bt:current-thread))
   (buffer-size :reader buffer-size :initarg :buffer-size)
   ;; one-of :open, :shutting-down, or :closed
   (status :accessor status :initform :closed)))


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


;; Exports 

(defgeneric start (server))
(defgeneric stop (server))
(defgeneric recieve-client (server session stream))

;; Server lifecycle

(defvar *my-sock* nil)

(defmethod start ((self server))
  (when (tcp-server self)
    (error "Cannot start server as it is already started!~%"))
  (flet ((read-cb (socket stream) (recieve-client self socket stream))
         (connect-cb (socket) (accept-client self socket)))
    (with-slots (host port) self
      (setf (tcp-server self)
            (as:tcp-server host port #'read-cb :connect-cb #'connect-cb :stream t))
      self)))

(defmethod stop ((self server))
  (unless (tcp-server self)
    (error "Cannot stop server as it is not started!~%"))
  (with-slots (tcp-server sockets) self
    (as:close-tcp-server tcp-server)
    (setf tcp-server nil)
    (setf sockets nil)))


;; Session lifecycle

(defmethod accept-client (self socket-wrapper)
  (let ((socket (as:streamish socket-wrapper)))
    (setf (as:socket-data socket)  ;; store session in socket-data
          (make-session self socket (buffer-size self)))))

(defun make-session (self socket bufsize)
  (let ((session (make-instance 'session/http:http :socket socket :bufsize bufsize)))
    ;;(change-class socket 'session/http:http :bufsize bufsize)
    (evented:handle self (make-instance 'evented:event :session session :name :accept-session ))
    session))


(defun close-session (self session)
  (declare (ignore self))
  (let ((socket (session:socket session)))
    (setf (as:socket-data socket) nil)
    (as:close-socket socket)))


;; (defmacro with-bound-stream (session stream &body body)
;;   `(progn 
;;      (setf (session:socket-stream ,session)
;;            (flexi-streams:make-flexi-stream ,stream :external-format :utf-8))
;;      ,@body
;;      (setf (session:socket-stream ,session) nil)))
        

;; Session message handling

(defmethod recieve-client (self socket-wrapper stream)
  (let* ((socket (as:streamish socket-wrapper))
         (session (as:socket-data socket)))
    (setf (session:socket-stream session)
          (flexi-streams:make-flexi-stream stream :external-format :utf-8))
    (handler-case (handle-session self session)
      (protocol-error (e)
        (session:send session e)
        (close-session self session)
        (evented:handle self e))
      (connection-error (e)
        (close-session self session)
        (evented:handle self e))
      (t (e)
        (report "Got an error ~a closing client" e)
        (close-session self session)))))

;; Internal

(defun handle-session (self session)
  (when-let ((data (session:recieve session)))
    (handle-message self session data)))


(defun handle-message (self session data)
  (ecase (type-of session)
    (session/http:http (process-handshake self session data))
    (session/websocket:websocket (process-message self session data))))


;; Handshake (http)

(defun process-handshake (self session request)
  (let ((headers (http-headers request)))
    (when (and (check-protocol self session request headers)
               (check-auth self session request headers))
      (session:upgrade-accept session request)
      (change-class session 'session/websocket:websocket))))


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
  (flet ((perror (name details)
           (error 'protocol-error :session session :name name :details details)))
    (let ((method (http-method request))
          (version (http-version request))
          (host (gethash "host" headers))
          (upgrade (gethash "upgrade" headers))
          (nonce (gethash "sec-websocket-key" headers))
          (ws-version (gethash "sec-websocket-version" headers))
          ;; optional
          ;;(user-agent (gethash "user-agent" headers))
          )
      (cond ((not (eq method :get)) (perror :invalid-method method))
            ((not (>= version 1.1)) (perror :invalid-http-version version))
            ((not (expected-host-p self host)) (perror :unexpected-host host))
            ((not (string-equal upgrade "websocket")) (perror :unexpected-upgrade upgrade))
            ((not (eq ws-version 13)) (perror :unsupported-websocket-version ws-version))
            ((null nonce) (perror :invalid-websocket-key nil))
            (t)))))


(defun valid-origin-p (self origin)
  (or (null (origins self))
      (member origin (origins self))))


(defun check-auth (self session request headers)
  (flet ((aerror (name details)
           (error 'auth-error :session session :name name :details details)))
    (let ((origin (gethash "origin" headers))
          (path (uri-path (uri (http-resource request)))))
      (cond ((not (valid-origin-p self origin)) (aerror :invalid-origin origin))
            ((not (string-equal path (path self))) (aerror :invalid-path path))
            (t)))))


;; Websocket message handling

(defun process-message (self session message)
  ;;(format t "procing message type ~a~%" (car message))
  (destructuring-bind (opsym data end) message
    (case opsym
      (:binary (session:send session data :start 0 :end end))
      (:text (progn
               (format t "Got message: ~a~%"
                       (flexi-streams:octets-to-string data :end end :external-format :utf-8))
               (session:send session data :start 0 :end end)))
      (:close (progn
                (format t "Shutting down socket~%")
                (close-session self session)
                (setf (session:status session) :shutdown)))
      (:ping (session/websocket:pong session data :start 0 :end end))
      (:pong (format t "Got pong with body ~a (end:~a)~%" data end)))))


(defun report (control-string &rest args)
  (when *log-output*
    (multiple-value-bind (second minute hour date month year) (get-decoded-time)
      (apply #'format *log-output*
             (concatenate 'string "~a/~a/~a ~2,'0d:~2,'0d:~2,'0d UTC " control-string "~%")
             (append (list hour minute second date month year) args)))))


;; (defun acceptor (self)
;;   (car (sockets self)))


;; todo - loop over sessions - if exceeded handshake timeout then kill
;; (defun poll (self)
;;   (with-slots (sockets status) self
;;     (loop
;;       (when (eq status :shutting-down)
;;         (loop for socket in sockets do (usocket:socket-close socket))
;;         (setf status :closed)
;;         (setf sockets nil)
;;         (return-from poll))
;;       (loop for socket in (usocket:wait-for-input sockets :timeout 0.1 :ready-only t) do
;;         (cond ((eq socket (acceptor self))
;;                (push-session self (make-session self (usocket:socket-accept socket) (buffer-size self))))
;;               ((eq (session:status socket) :shutdown) (close-socket self socket))
;;               (t (safe-handle-session self socket)))))))


;; Dispatch message handling



(defvar *res* nil)
(defvar *server* nil)
(defvar *session* nil)
(defvar *session-real* nil)
(defvar *session-read-thread* nil)

(defun test ()
  (when *server*
    (stop *server*)
    (setf *server* nil))
  (sleep 0.150)
  (setf *server* (make-server "ws://0.0.0.0:8081/ws" :host "dev.owny.io"))
  (start *server*)
    ;; (setf *session* (usocket:socket-connect "localhost" 8081))
    ;; (setf *session-read-thread*
    ;;       (bt:make-thread
    ;;        (lambda ()
    ;;          (loop
    ;;            (format os "~a" (read-char (usocket:socket-stream *session*) nil nil))))))
    *server*)


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
