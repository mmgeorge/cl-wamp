(defpackage :wamp/ws/client
  (:use :cl :alexandria)
  (:import-from :usocket)
  (:import-from :cl-async)
  (:import-from :fast-http)
  (:import-from :flexi-streams)
  (:import-from :bordeaux-threads)
  (:import-from :wamp/util)
  (:import-from :wamp/ws/session/http)
  (:import-from :wamp/ws/session/websocket #:websocket)
  (:import-from :wamp/ws/session/session #:session)
  (:local-nicknames
    (:session/http :wamp/ws/session/http)
    (:session :wamp/ws/session/session)
    (:util :wamp/util)
    (:session/websocket :wamp/ws/session/websocket))
  (:export #:client #:recieve-text #:recieve-binary #:send #:destroy))

(in-package :wamp/ws/client)


(defclass client ()
  ((session :reader session :initform nil)
   (host :reader host :initarg :host)
   (port :reader port :initarg :port)
   (bufsize :reader bufsize :initform 1024 :initarg :bufsize)
   (resolver :accessor resolver) ;; when NIL, handshake has finished
   ;;(main-thread :reader main-thread :initform (bt:current-thread))
   ;;(thread :accessor thread :initform nil)
   ;; one-of :open, :shutting-down, or :closed
   (status :accessor status :initform :open)
   

   ))

;;(defgeneric recieve (self message &key))
(defgeneric start (self))
(defgeneric stop (self))
(defgeneric send (self message &key))
(defgeneric recieve-binary (self buffer end &key))
(defgeneric recieve-text (self text &key))
(defgeneric recieve (self stream))

;(defmethod initialize-instance :after ((self client) &key host port (bufsize 1024))
  ;"Initialize a new websocket connection, blocking until the session has been established"
 ; (declare (ignore self host port bufsize)))


(defmethod start ((self client))
  (let* ((resolver (util:create-resolver)))
    (flet ((read-cb (socket stream) (declare (ignore socket)) (recieve self stream))
           (connect-cb (socket)
             (declare (ignore socket))
             (session:upgrade-request (session self))))
      (with-slots (host port bufsize session handshake) self
        (let* ((async-stream (as:tcp-connect host port #'read-cb :connect-cb #'connect-cb :stream t))
               (socket (as:streamish async-stream)))
          (setf (resolver self) resolver)
          (setf session (make-instance 'session/http:http :socket socket :bufsize (bufsize self)
                                                          :server-sock t))
          (setf (session:socket-stream session)
                (flexi-streams:make-flexi-stream async-stream :external-format :utf-8))
          (bb:wait (util:resolver-promise resolver)
            self))))))


(defmethod stop ((self client))
  (when-let* ((session (session self))
              (socket (session:socket session)))
    (as:close-socket socket)))


(defun handle-upgrade-response (self response buffer start end)
  (format t "handle upgrade response ~%")
  (case (fast-http:http-status response)
    ;; TODO - Must verify correct upgrade response sent
    (101 (change-class (session self)  'websocket))
    (t (progn
         ;(session:stop session)
         (error
          "Unable to establish connection, encountered ~a:~%   ~a~%"
          (fast-http:http-status response)
          (flexi-streams:octets-to-string buffer :start start :end end :external-format :utf-8)))))
  (let ((resolver (resolver self)))
    (setf (resolver self) nil)
    (funcall (util:resolver-resolve-fn resolver))))


(defun handshake-pending-p (self)
  (not (null (resolver self))))


;; change to method on session type 
(defmethod recieve ((self client) stream)
  (with-accessors ((session session)) self
    (if (handshake-pending-p self)
        (multiple-value-bind (response buffer start end) (session:recieve session)
          (handle-upgrade-response self response buffer start end))
        (when-let ((message (session:recieve session)))
          (process-message self message)))))


(defmethod recieve-text ((self client) text &key)
  (format t "~a~%" text))


(defun process-message (self message)
  (destructuring-bind (opsym data end) message
    (case opsym
      (:binary (recieve-binary self data end))
      (:text (recieve-text self (flexi-streams:octets-to-string data :end end :external-format :utf-8)))
      (:close (destroy self))
      (:ping (session/websocket:pong (session self) data :start 0 :end end))
      (:pong (format t "Got pong with body ~a (end:~a)~%" data end)))))


(defmethod send ((self client) (message simple-array) &key)
  (let ((message (flexi-streams:string-to-octets message)))
    (session:send (session self) message :start 0 :end (length message))))



(defvar *client* nil)


(defun test ()
  (when *client* (stop *client*))
  (let ((message (flexi-streams:string-to-octets "Successfully connected to server")))
    (setf *client* (make-instance 'client :host "dev.owny.io" :port 8081))
    (bb:wait (start *client*)
      (session:send (session *client*) message :start 0 :end (length message)))))


(defun send-test (msg)
  (let ((message (flexi-streams:string-to-octets msg))) 
    (session:send (session *client*) message :start 0 :end (length message))))


