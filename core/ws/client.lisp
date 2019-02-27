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
  (:export #:client #:start #:stop #:recieve-text #:recieve-binary
           #:send-text #:send-binary #:ping #:simple-client))

(in-package :wamp/ws/client)


(defclass client ()
  ((session :reader session :initform nil)
   (host :reader host :initarg :host)
   (port :reader port :initarg :port)
   (bufsize :reader bufsize :initform 9 :initarg :bufsize)
   (resolver :accessor resolver) ;; when NIL, handshake has finished
   (when-pong :accessor when-pong :initform nil)
   (status :accessor status :initform :open)))

(defgeneric start (self))
(defgeneric stop (self))
(defgeneric recieve (self stream))
(defgeneric recieve-binary (self buffer &key start end))
(defgeneric recieve-text (self text))
(defgeneric send-text (self text))
(defgeneric send-binary (self buffer &key start end))
(defgeneric ping (self data-or-nil &key start end))


(defmethod start ((self client))
  (let* ((resolver (util:create-resolver)))
    (flet ((read-cb (socket stream)
             (declare (ignore socket))
             (recieve self stream))
           (connect-cb (socket)
             (declare (ignore socket))
             (session:upgrade-request (session self) :host (host self) :port (port self))))
      (with-slots (host port bufsize session handshake) self
        (let* ((async-stream (as:tcp-connect host port #'read-cb :connect-cb #'connect-cb :stream t))
               (socket (as:streamish async-stream)))
          (setf (resolver self) resolver)
          (setf session (make-instance 'session/http:http :socket socket :bufsize (bufsize self)))
          (setf (session:socket-stream session)
                (flexi-streams:make-flexi-stream async-stream :external-format :utf-8))
          (as:with-delay (1)
            (unless (bb:promise-finished-p (util:resolver-promise resolver))
              (funcall (util:resolver-reject-fn resolver)
                       "Timeout - unable to establish websocket session")))
          (bb:wait (util:resolver-promise resolver)
            self))))))


(defmethod stop ((self client))
  (when-let* ((session (session self))
              (socket (session:socket session)))
    (as:close-socket socket)))


(defun handle-upgrade-response (self response buffer start end)
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


(defun process-message (self message)
  (destructuring-bind (opsym data end) message
    (case opsym
      (:binary (recieve-binary self data end))
      (:text (recieve-text self (flex:octets-to-string data :end end :external-format :utf-8)))
      (:close (stop self))
      (:ping (session/websocket:pong (session self) data :start 0 :end end))
      (:pong
       (let ((resolve-fn (util:resolver-resolve-fn (when-pong self))))
         (setf (when-pong self) nil)
         (funcall resolve-fn data end))))))


(defmethod recieve ((self client) stream)
  (with-accessors ((session session)) self
    (if (handshake-pending-p self)
        (multiple-value-bind (response buffer start end) (session:recieve session)
          (handle-upgrade-response self response buffer start end))
        (when-let ((message (session:recieve session)))
          (process-message self message)))))


(defmethod send-text ((self client) (message simple-array))
  (let ((message (flexi-streams:string-to-octets message)))
    (session:send-text (session self) message :start 0 :end (length message))))

(defmethod send-binary ((self client) (message simple-array) &key (start 0) end)
  (session:send-binary (session self) message :start start :end end))


(defmethod ping ((self client) data-or-nil &key (start 0) end)
  (when (when-pong self)
    (error "An outstanding ping request is already pending"))
  (let ((resolver (util:create-resolver)))
    (setf (when-pong self) resolver)
    (session/websocket:ping (session self) data-or-nil :start start :end end)
    (util:resolver-promise resolver)))



(defclass simple-client (client) ())

(defmethod recieve-text (self message)
  (format t "simple-client: got text ~a~%" message))

(defmethod recieve-binary (self buffer &key start end)
  (format t "simple-client: got binary ~a ~%" (subseq buffer start end)))


(defvar *client* nil)


(defun test ()
  (when *client* (stop *client*))
  (let ((message (flexi-streams:string-to-octets "Successfully connected to server")))
    (setf *client* (make-instance 'simple-client :host "dev.owny.io" :port 8088))
    (bb:wait (start *client*)
      (session:send-text (session *client*) message :start 0 :end (length message)))))
