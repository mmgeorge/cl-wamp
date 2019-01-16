(defpackage :wamp/ws/client
  (:use :cl :alexandria)
  (:import-from :usocket)
  (:import-from :fast-http)
  (:import-from :flexi-streams)
  (:import-from :bordeaux-threads)
  (:import-from :wamp/ws/session/http #:http)
  (:import-from :wamp/ws/session/websocket #:websocket)
  (:import-from :wamp/ws/session/session #:session)
  (:local-nicknames (:session :wamp/ws/session/session))
  (:export #:client #:recieve #:send #:destroy))

(in-package :wamp/ws/client)


(defclass client ()
  ((session :reader session :initform nil)
   (main-thread :reader main-thread :initform (bt:current-thread))
   (thread :accessor thread :initform nil)
   ;; one-of :open, :shutting-down, or :closed
   (status :accessor status :initform :open)))


(defgeneric recieve (self message &key))
(defgeneric send (self message &key))


(defmethod initialize-instance :after ((self client) &key host port (bufsize 1024))
  "Initialize a new websocket connection, blocking until the session has been established"
  (let* ((sock (usocket:socket-connect host port  :element-type '(unsigned-byte 8)))
         (session (change-class sock 'http  :bufsize bufsize))
         (os *standard-output*))
    (session:upgrade-request session)
    (usocket:wait-for-input session)
    (multiple-value-bind (response buffer start end) (session:recieve session)
      (case (fast-http:http-status response)
        ;; TODO - Must verify correct upgrade response sent
        
        (101 (setf (slot-value self 'session) (change-class sock 'websocket)))
        (t (error
            "Unable to establish connection, encountered ~a:~%   ~a~%"
            (fast-http:http-status response)
            (flexi-streams:octets-to-string buffer :start start :end end :external-format :utf-8)))))
    (setf (thread self) (bt:make-thread (lambda ()
                                          (let ((*standard-output* os))
                                            (poll self)))))))


(defmethod recieve ((self client) message &key)
  (declare (ignore self))
  (destructuring-bind (type buffer end) message
    (declare (ignore type))
    (format t "Client: You got mail! ~a~%"
            (flexi-streams:octets-to-string buffer :start 0 :end end :external-format :utf-8))))


(defun poll (self)
  (with-slots (session status) self
    (loop
      (when (eq status :shutting-down)
        (usocket:socket-close (session self))
        (setf (status self) :closed)
        (return-from poll))
      (when (usocket:wait-for-input session :timeout 0.1)
        (unless (eq (Status self) :shutting-down)
        (when-let (message (session:recieve session))
          ;(bt:interrupt-thread (main-thread self) #'(lambda () (funcall #'recieve self message)))
          (funcall #'recieve self message )
          ))))))
  

(defun destroy (self)
  (when (eq (status self) :open)
    (with-slots (thread) self
      (unless (and thread (bt:thread-alive-p thread))
        (error "Unable to destroy client. Polling thread is already destroyed~%"))
      (setf (status self) :shutting-down)
      (unless (eq (bt:current-thread)
                  (thread self))
        (loop until (eq (status self) :closed)))
      (setf (thread self) nil))))


(defmethod send ((self client) (message simple-array) &key)
  (let ((message (flexi-streams:string-to-octets message)))
    (session:send (session self) message :start 0 :end (length message))))



(defvar *client* nil)


(defun test ()
  (when *client* (destroy *client*))
  (let ((message (flexi-streams:string-to-octets "Successfully connected to server")))
    (setf *client* (make-instance 'client :host "dev.owny.io" :port 8081))
    (session:send (session *client*) message :start 0 :end (length message))))


(defun send-test (msg)
  (let ((message (flexi-streams:string-to-octets msg))) 
    (session:send (session *client*) message :start 0 :end (length message))))


