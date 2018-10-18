(defpackage :wamp/transport/rawsocket
  (:use :cl :wamp/message-type)
  (:import-from :cl-json)
  (:import-from :usocket)
  (:import-from :bordeaux-threads)

  (:import-from :wamp/message-type)
  (:import-from :wamp/transport/transport #:transport)
  (:import-from :wamp/decorators #:dtype)
  )

(in-package :wamp/transport/rawsocket)

(defmacro debug-print (control &rest args)
  `(progn
     (format t "wamp/transport/rawsocket: ")
     (funcall #'format ,@(cons t (cons control args)))
     (format t "~%")))


(defclass rawsocket (transport)
  ((url :reader url :initarg :url :type string)
   (port :reader port :initarg :port :type number)
   (thread :accessor thread :initform nil)
   (socket :accessor socket :initform nil)

   (serializer :reader serializer :initarg :serializer)
   (max-len :reader max-len :initarg :max-len)
   (server-max-len :accessor server-max-len :initform nil)))


(defun make-rawsocket (url port &key
                                  (max-len 15)
                                  (serializer :json)
                                  (on-open (lambda () ()))
                                  (on-close (lambda () ()))
                                  (on-message (lambda (message) (format t "YOU GOT MAIL:~%~a~%" message)))
                                  (on-error (lambda (error) (declare (ignore error)))))
  (make-instance 'rawsocket :url url :port port
                            :serializer serializer
                            :max-len max-len
                            :on-open on-open
                            :on-close on-close
                            :on-message on-message
                            :on-error on-error ))


(defmethod initialize-instance :after ((self rawsocket) &key))


(defun create-socket (self addr port)
  (let* ((sock (usocket:socket-connect addr port :element-type '(unsigned-byte 8)))
         (stream (usocket:socket-stream sock))
         (debug-stream *standard-output*))
    (handler-case 
        (progn
          (debug-print "starting socket")
          (write-handshake stream :max-length (max-len self) :serializer (serializer self))
          (debug-print "Waiting for response")
          (usocket:wait-for-input sock)
          (debug-print "Got response")
          (setf (server-max-len self) (read-requested-length stream :expected-serializer :json))
          (setf (thread self) (bt:make-thread (lambda () (poll self stream debug-stream)))))
      (t (e)
        (format t "Encountered error in opening socket ~a" e)
        (usocket:socket-close sock)
        (when (thread self) 
          (bt:destroy-thread (thread self))
          (setf (thread self) nil))))
    sock))



(defmethod transport:start ((self rawsocket))
  (setf (socket self)
        (create-socket self (url self) (port self))))


(defmethod transport:stop ((self rawsocket))
  (when (thread self)
    (bt:destroy-thread (thread self))
    (setf (thread self) nil))
  (when (socket self)
    (usocket:socket-close (socket self))
    (setf (socket self) nil)))


(defmethod transport:send ((self rawsocket) (message list))
  ;; before method here!?? serialization should be on base class ...
  (let ((serialized (transport:serialize self message)))
    (write-frame (usocket:socket-stream (socket self))
                 serialized :type :message )))


(defmethod transport:deserialize ((self rawsocket) (message string))
  (the list
       (let ((parsed (json:decode-json-from-string message)))
         (check-type parsed list "a well-formed wamp-message")
         (check-type (car parsed) fixnum "a message type")
         (rplaca parsed (wamp/message-type:code-to-message-t (car parsed)))
         parsed)))


;; ++ Internal ++

(defun poll (self stream out-stream)
  (unwind-protect
       (loop
         (if (listen stream)
             (multiple-value-bind (type message) (read-frame stream out-stream)
               (case type
                 (:message (-handle-message self message))
                 (t (error "unknown stuff"))))
             (format out-stream "nothing to read...~%"))
           (sleep 3)
           )
    (usocket:socket-close (socket self))))


(defun pack-u4-u4 (a b)
  (let ((out 0))
    (setf (ldb (byte 4 4) out) a)
    (setf (ldb (byte 4 0) out) b)
    out))


(defun unpack-u4-u4 (a)
  (values
   (ldb (byte 4 4) a)
   (ldb (byte 4 0) a)))
        

(defun serializer-to-num (serializer)
  (case serializer
    (:json 1)
    (:message-pack 2)
    (:t 0)))



(defun write-handshake (stream &key (max-length 15) (serializer :json))
  (let ((serializer-value (serializer-to-num serializer)))
    (write-magic stream)
    (write-byte (pack-u4-u4 max-length serializer-value) stream)
    (write-byte 0 stream) ;; reserved
    (write-byte 0 stream)
    (force-output stream)))


(defun write-magic (stream)
  (write-byte #x7f stream))

(defun read-magic (stream)
  (or (eq (read-byte stream nil nil) #x7f)
      (error "No magic found")))


(defun raise-handshake-error (error-code)
  (case error-code
    (0 (error "illegal"))
    (1 (error "serializer unsupported"))
    (2 (error "maximum message length unacceptable"))
    (3 (error "use of reserved bits (unsupported feature)"))
    (4 (error "maximum connection count reached"))
    (t (error "unknown handshake error"))))


(defun read-requested-length (stream &key expected-serializer)
  (let ((serializer-value (serializer-to-num expected-serializer)))
    (read-magic stream)
    (multiple-value-bind (length-or-error serializer)
        (unpack-u4-u4 (read-byte stream nil nil))
      (when (eq serializer 0)
        (raise-handshake-error length-or-error))
      (when (not (eq serializer serializer-value ))
        (error "router returned a serializer that was not requested"))
      (read-byte stream nil nil) ;; reserved
      (read-byte stream nil nil) ;; reserved
      (expt 2 (+ 9 length-or-error)))))



(defun to-transport-type (type)
  (case type
    (0 :message)
    (1 :ping)
    (2 :pong)
    (t (error "Unknown transport type"))))


(defun from-transport-type (type)
  (case type
    (:message 0)
    (:ping 1)
    (:pong 2)
    (t (error "Unknown transport type"))))


(defun read-transport-type (stream debug-stream)
  (let ((out (to-transport-type
              (ldb (byte 3 0)
                   (read-byte stream nil nil)))))
  
    (format debug-stream "Got transport-type ~a" out )
    out
  ))


(defun write-transport-type (stream type)
  (write-byte (from-transport-type type) stream))



(defun write-frame-length (stream length)
  (write-byte (ldb (byte 8 16) length) stream)
  (write-byte (ldb (byte 8 8) length) stream)
  (write-byte (ldb (byte 8 0) length) stream))


(defun read-frame-length (stream)
  (let ((out 0))
    (setf (ldb (byte 8 16) out) (read-byte stream nil nil))
    (setf (ldb (byte 8 8) out) (read-byte stream nil nil))
    (setf (ldb (byte 8 0) out) (read-byte stream nil nil))
    out))


(defun read-message (stream)
  (let* ((length (read-frame-length stream))
         (buf (make-array length :element-type 'character :adjustable nil)))
    (dotimes (i length)
      (setf (aref buf i)
            (code-char (read-byte stream nil nil))))
    buf))


(defun read-ping (stream)
  (let* ((length (read-frame-length stream))
         (buf (make-array length :element-type '(unsigned-byte 8) :adjustable nil)))
    (dotimes (i length)
      (setf (aref buf i)
            (read-byte stream nil nil)))))


(defun read-frame (stream debug-stream)
  (format debug-stream "reading-frame")
  (let* ((type (read-transport-type stream debug-stream))
         (body (case type
                 (:message (read-message stream))
                 (:ping (read-ping stream))
                 (t nil))))
    (values type body)))


(defun write-frame (stream message &key type)
  (write-transport-type stream type)
  (write-frame-length stream (length message))
  (loop for char across message do
    (write-byte (char-code char) stream))
  (force-output stream))


(defmethod transport:serialize ((self rawsocket) (message list))
  (the (values string &optional)
       (progn (rplaca message (wamp/message-type:message-t-to-code (car message)))
              (json:encode-json-to-string message))))


(defun -handle-message (self message)
  (debug-print "send: Recieved message: ~a" message)
  (let ((message (handler-case (transport:deserialize self message)
                   ;; Ignore any malformed messages
                   (t () (format t "Ignoring malformed message"))) ))
    (when message
      (funcall (transport:on-message self) (car message) (cdr message)))))

