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
  (:local-nicknames
    (:session :wamp/ws/session/session)
    (:session/http :wamp/ws/session/http)
    (:session/websocket :wamp/ws/session/websocket)
    (:evented :wamp/ws/evented)
    (:util :wamp/util))
  (:export #:server #:make-server #:start #:stop #:sessions #:cleanup-frequency #:read-timeout
           #:recieve-text #:recieve-binary #:echo-server))

(in-package :wamp/ws/server)


(defvar *log-output* *standard-output*)

(defclass server (evented)
  ((host :reader host :type 'string :initarg :host)
   (port :reader port :type 'fixnum :initarg :port)
   (path :reader path :type 'string :initarg :path)
   (origins :reader origins :type 'list :initarg :origins :initform nil)
   (authority :accessor authority :type 'string :initarg :hostname :initform nil)
   (sessions :accessor sessions :initform nil)
   (thread :accessor thread :initform nil)
   (tcp-server :accessor tcp-server :initform nil)
   (cleanup-frequency :accessor cleanup-frequency :initarg :cleanup-frequency :initform 5)
   (cleanup-idler :accessor cleanup-idler :initform nil)
   (cleanup-since :accessor cleanup-since :initform (get-universal-time))
   (bufsize :reader bufsize :initarg :bufsize)))


(defmethod initialize-instance :after ((self server) &key url (bufsize 9))
  "BUFSIZE controls how large each session buffer will be, as long as the maximum length 
   of a websocket message. Size is calculated as 2 ^ (9 + BUFSIZE)"
  (check-type bufsize (integer 0 15))
  (check-type url string)
  (let ((uri (uri url))
        (buffer-size bufsize)) 
    (with-slots (host port path origin authority bufsize) self
      (setf host (uri-host uri))
      (setf port (uri-port uri))
      (setf path (uri-path uri))
      (setf bufsize buffer-size))))


;; Exports 

(defgeneric start (self))
(defgeneric stop (self))
(defgeneric accept-client (self socket-stream))
(defgeneric recieve (self session stream))
(defgeneric recieve-text (self session text))
(defgeneric recieve-binary (self session buffer &key start end))

;; Server lifecycle

(defmethod start ((self server))
  (if (tcp-server self)
      (warn "Cannot start server as it is already started!~%")
      (flet ((read-cb (socket stream)
               (recieve self socket stream))
             (connect-cb (socket)
               (accept-client self socket)))
        (with-slots (host port) self
          (setf (tcp-server self)
                (as:tcp-server host port #'read-cb :connect-cb #'connect-cb :stream t))
          (setf (cleanup-idler self)
                (as:idle #'(lambda () (cleanup-sessions self))))
          self))))


(defmethod stop ((self server))
  (if (null (tcp-server self))
      (warn "Cannot stop server as it is not started!~%")
      (with-slots (tcp-server sessions) self
        (as:close-tcp-server tcp-server)
        (setf tcp-server nil)
        (as:free-idler (cleanup-idler self))
        (setf (cleanup-idler self) nil)
        (setf sessions nil))))


;; Session lifecycle

(defmethod accept-client (self socket-wrapper)
  (let* ((socket (as:streamish socket-wrapper))
         (session (make-instance 'session/http:http :socket socket :bufsize (bufsize self))))
    (session:set-peername session socket)
    (setf (as:socket-data socket) session)
    (setf (sessions self)
          (cons session (sessions self)))
    (evented:handle self (make-instance 'evented:event :session session :name :accept-session))))


(defun close-session (self session)
  (let ((socket (session:socket session)))
    (setf (sessions self) (delete session (sessions self)))
    (setf (as:socket-data socket) nil)
    (as:close-socket socket)))


(defun cleanup-sessions (self)
  "Remove and handle any disconnected sessions"
  (with-accessors ((cleanup-since cleanup-since)
                   (cleanup-frequency cleanup-frequency)
                   (sessions sessions)) self
    (let* ((current-time (get-universal-time))
           (delta (- current-time cleanup-since)))
      (when (> delta cleanup-frequency)
        (setf cleanup-since current-time)
        (setf sessions
              (loop for session in sessions
                    for socket = (session:socket session)
                    for timestamp-delta = (- current-time (session:timestamp session))
                    when (typep session 'session/websocket:websocket) do
                      (session/websocket:ping session nil)
                    if (or (as:socket-closed-p socket)
                           (> timestamp-delta (* 3 cleanup-frequency))) do
                             (evented:handle
                              self
                              (make-instance 'evented:event :session session :name :lost-connection))
                    else
                      collect session))))))

;; Session message handling

(defmethod recieve (self socket-wrapper stream)
  (let* ((socket (as:streamish socket-wrapper))
         (session (as:socket-data socket)))
    (setf (session:socket-stream session)
          (flexi-streams:make-flexi-stream stream :external-format :utf-8))
    (handler-case (handle-session self session)
      (protocol-error (e)
        (session:send-text session e)
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
  (destructuring-bind (opsym data end) message
    (case opsym
      (:binary (recieve-binary self session data :start 0 :end end)) 
      (:text (recieve-text self session (flex:octets-to-string data :end end :external-format :utf-8)))
      (:close (progn
                (format t "Shutting down socket~%")
                (close-session self session)
                (setf (session:status session) :shutdown)))
      (:ping (session/websocket:pong session data :start 0 :end end))
      (:pong (setf (session:timestamp session) (get-universal-time))))))


(defun report (control-string &rest args)
  (when *log-output*
    (multiple-value-bind (second minute hour date month year) (get-decoded-time)
      (apply #'format *log-output*
             (concatenate 'string "~a/~a/~a ~2,'0d:~2,'0d:~2,'0d UTC " control-string "~%")
             (append (list hour minute second date month year) args)))))


(defclass echo-server (server) ())

(defmethod recieve-text ((self echo-server) session text)
  (let ((octets (flex:string-to-octets text :external-format :utf-8)))
    (session:send-text session octets :start 0 :end (length octets))))

(defmethod recieve-binary ((self echo-server) session buffer &key start end)
  (session:send-binary session buffer start end))


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
  (setf *server* (make-instance 'echo-server :url "ws://0.0.0.0:8088/ws" :host "dev.owny.io"))
  (start *server*)
    *server*)
