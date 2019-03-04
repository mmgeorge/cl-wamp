(defpackage :wamp/ws/session/session
  (:use :cl)
  (:import-from :usocket)
  (:import-from :fast-http)
  (:import-from :flexi-streams)
  (:export #:session #:recieve #:send-error #:protocol #:socket-stream #:socket #:set-peername
           #:send-text #:send-binary #:send-close
           #:normalize-reason
           #:status #:stop
           #:upgrade-accept #:upgrade-request
           #:index #:buffer
           #:port #:address #:timestamp))

(in-package :wamp/ws/session/session)


(defclass session ()
  ((status :accessor status :initform :open)
   (buffer :accessor buffer :initform nil)
   (index :accessor index :initform 0)
   (family :accessor family)
   (port :accessor port :initform 0)
   (address :accessor address :initform #(0 0 0 0))
   (socket :accessor socket)
   (timestamp :accessor timestamp :initform (get-universal-time))
   (stream :accessor socket-stream))) ;; set by server on read-cb


(defgeneric upgrade-request (self &key))
(defgeneric upgrade-accept (self request))
(defgeneric recieve (self))
(defgeneric send-text (self data &key start end))
(defgeneric send-binary (self data &key start end))
(defgeneric send-close (self code reason)
  (:documentation "Send a close message with CODE where REASON, if non-nil, is either a condition or text"))

(defun normalize-reason (reason?)
  (etypecase reason?
    (null "")
    (condition (format nil "~A" reason?))
    (t reason?)))


(defmethod send-close (self code (reason null))
  (call-next-method self code (format nil "~A" reason)))


(defmethod send-close :after (self code reason)
  (setf (status self) :closing))


(defmethod initialize-instance :after ((self session) &key socket bufsize)
  "BUFSIZE indicates the size of the underlying buffer and max accepted websocket message length.
   Calulated at (expt 2 (+ bufsize 9)) octets"
  (check-type bufsize (integer 0 15))
  (setf (socket self) socket)
  (setf (buffer self) (make-array (expt 2 (+ bufsize 9)) :element-type '(unsigned-byte 8))))


(defun u32-to-u8888 (u32)
  (let ((out (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref out 0) (ldb (byte 8 0) u32))
    (setf (aref out 1) (ldb (byte 8 8) u32))
    (setf (aref out 2) (ldb (byte 8 16) u32))
    (setf (aref out 3) (ldb (byte 8 24) u32))
    out))


(defun read-ss-family (value)
  (ecase value
    (0 :AF_UNSPEC)
    (1 :AF_UNIX)
    (2 :AF_INET)
    (3 :AF_AX25)
    (4 :AF_IPX)
    (5 :AF_APPLETALK)
    (6 :AF_NETROM)
    (7 :AF_BRIDGE)
    (8 :AF_AAL5)
    (9 :AF_X25)
    (10 :AF_INET6)
    (12 :AF_MAX)))


(defun set-peername (self sock)
  ;; cl-async returns a async-io-stream as a "socket"
  ;; (as:streamish io-stream) includes the tcp-socket, and
  ;; (as:socket-c streamish) holds the actual libuv socket
  (cffi:with-foreign-objects ((addr '(:struct uv:sockaddr-storage)) (namelen 'uv:socklen-t))
      (setf (cffi:mem-ref namelen 'uv:socklen-t) ;; use socketlen_t
            (cffi:foreign-type-size '(:struct uv:sockaddr-storage)))  
      (uv:uv-tcp-getpeername (as:socket-c sock) addr namelen)
      (let ((ss-family (cffi:foreign-slot-value addr '(:struct uv:sockaddr-storage) 'uv:ss-family)))
        (ecase (read-ss-family ss-family)
          (:AF_INET (set-ip4-peername self addr))
          (:AF_INET6 (set-ip6-peername self addr))))))


(defun set-ip4-peername (self addr)
  (let* ((sin-port (cffi:foreign-slot-value addr '(:struct uv:sockaddr-in) 'uv:sin-port))
         (sin-addr (cffi:foreign-slot-value addr '(:struct uv:sockaddr-in) 'uv:sin-addr))
         (s-addr (cffi:foreign-slot-value sin-addr '(:struct uv:in-addr) 'uv:s-addr)))
    (setf (family self) :AF_INET)
    (setf (port self) sin-port)
    (setf (address self) (u32-to-u8888 s-addr))))

    
(defun set-ip6-peername (self addr)
  (let* ((sin6-port (cffi:foreign-slot-value addr '(:struct uv:sockaddr-in6) 'uv:sin6-port))
         (sin6-addr (cffi:foreign-slot-value addr '(:struct uv:sockaddr-in6) 'uv:sin6-addr))
         (s6-addr (cffi:foreign-slot-value sin6-addr '(:struct uv:in6-addr) 'uv:s6-addr)))
    (setf (family self) :AF_INET6)
    (setf (port self) sin6-port)
    (setf (address self) s6-addr)))


(defun stop (self)
  (declare (ignore self)))
  
