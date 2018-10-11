(defpackage :wamp/ws/server
  (:use :cl)
  (:import-from :usocket)
  (:import-from :bordeaux-threads)
  (:import-from :wamp/ws/client)
  (:local-nicknames (:client :wamp/ws/client))
  (:export #:server))

(in-package :wamp/ws/server)


(defclass server ()
  ((host :reader host :type 'string :initarg :host)
   (port :reader port :type 'fixnum :initarg :port)
   ;; First socket denotes the master/acceptor socket
   (sockets :accessor sockets :initform nil)
   (thread :accessor thread :initform nil)
   ))


(defun make-server (host port)
  (make-instance 'server :host host :port port ))


;; Methods 

(defgeneric start (server))
(defgeneric stop (server))


(defmethod start ((self server))
  (let ((socket (create-socket (host self) (port self))))
    (setf (sockets self) (list socket))
    (setf (thread self) (bt:make-thread (lambda () (poll self)) :name "acceptor"))
    self))


(defmethod stop ((self server))
  (when (running-p self)
    (when (bt:thread-alive-p (thread self))
      (bt:destroy-thread (thread self)))
    (loop for socket in (sockets self) do (usocket:socket-close socket))
    (setf (sockets self) nil))
  self)


(defun running-p (self)
  (> (length (sockets self)) 1))


(defun acceptor (self)
  (car (sockets self)))


;; Internal

(defun create-socket (host port)
  (usocket:socket-listen host port :element-type '(unsigned-byte 8) :reuse-address t))


;; todo - loop over clients - if exceeded handshake timeout then kill
(defun poll (self)
  (with-slots (sockets) self
    (loop
      (loop for socket in (usocket:wait-for-input sockets :ready-only t) do
        (if (eq socket (acceptor self))
            (push-client self (client:make-client (usocket:socket-accept socket)))
            (handle-client self socket))))))


(defun handle-client (self client)
  (let ((data-or-nil (client:read-input client)))
    (when data-or-nil
      ;; todo - enqueue on thread pool?
      (handle-message self client data-or-nil))))


(defun handle-message (self client data)
  (case (client:protocol client)
    (:http (process-handshake self client data))
    (:websocket (process-websocket-frame self client data))
    (t (error "Unknown protocol"))))


(defun process-handshake (self client data)
  (declare (ignore self data))
  (write-byte 101 (usocket:socket-stream client))
  (force-output (usocket:socket-stream client)))


(defun process-websocket-frame (self client data)
  (declare (ignore self client data)))


(defun push-client (self client)
  (with-slots (sockets) self
    (push client (cdr (last sockets)))
    (write-byte 245 (usocket:socket-stream client))
    (force-output (usocket:socket-stream client))
    ))


(defvar *server* nil)
(defvar *client* nil)
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
    (setf *server* (make-server "localhost" 8081))
    (start *server*)
    (setf *client* (usocket:socket-connect "localhost" 8081 :element-type '(unsigned-byte 8)))
    (setf *client-read-thread*
          (bt:make-thread
           (lambda ()
             (loop
               (format os "~a~%" (read-byte (usocket:socket-stream *client*) nil nil))))))
    *server*
    ))
