(defpackage :wamp/ws/protocol/websocket
  (:use :cl)
  (:import-from :wamp/ws/protocol/base)
  (:local-nicknames (:protocol :wamp/ws/protocol/base))
  (:export #:websocket #:make-websocket))

(in-package :wamp/ws/protocol/websocket)

(defclass websocket ()
  ())


(defun make-websocket ()
  (make-instance 'websocket))


(defmethod protocol:recieve ((self websocket) stream)
  (declare (ignore self))
  (read-frame stream))


;; See https://tools.ietf.org/html/rfc6455#section-5.2
(defun read-frame (stream &key (expects-rsv nil))
  (multiple-value-bind (fin rsv opcode len mask) (read-header stream expects-rsv)
    (format t "Got fin:~a rsv:~a opcode:~a len:~a mask~a~%" fin rsv opcode len mask)
    (let ((buffer (make-array len :element-type '(unsigned-byte 8))))
      (if mask
          (read-masked-data stream buffer (1- len) mask)
          (read-data stream buffer (1- len)))
      (format t "Got op ~a~%~%buf:~% ~a" opcode buffer)
      (values opcode buffer fin rsv))))


(defun read-data (stream buffer end)
  (loop for i from 0 to end
        for byte = (read-byte stream nil nil)
        do (setf (aref buffer i) byte)
        finally (return (1+ i))))


(defun read-masked-data (stream buffer end mask-key)
  (loop for i from 0 to end
        for mask = (ldb (byte 8 (* 8 (- 3 (mod i 4)))) mask-key)
        for byte = (read-byte stream nil nil)
        for unmasked-byte = (logxor byte mask)
        do (setf (aref buffer i) unmasked-byte)
        finally (return (1+ i))))


(defun read-header (stream expects-rsv)
    (multiple-value-bind (fin rsv opcode)
        (decode-byte-0 (read-byte stream nil nil) expects-rsv)
      (multiple-value-bind (has-mask payload-len) (decode-byte-1 (read-byte stream nil nil))
        (let ((len (decode-len stream payload-len))
              (mask (decode-mask stream has-mask)))
          (values fin rsv opcode len mask)))))


(defun decode-byte-0 (byte-0 expects-rsv)
    (format t "0-byte:~b" byte-0)

  (let ((fin (= 1 (ldb (byte 1 7) byte-0)))
        (rsv (ldb (byte 3 4) byte-0))
        (opcode (opcode-symbol (ldb (byte 4 0) byte-0))))
    (unless (or expects-rsv (= rsv 0)) (error "unxpected rsv ~a" rsv))
    (values fin rsv opcode)))


(defun opcode-symbol (opcode)
  (case opcode
    (#x0 :continuation)
    (#x1 :text)
    (#x2 :binary)
    (#x8 :close)
    (#x9 :ping)
    (#xA :pong)
    (t (error "invalid op"))))


(defun decode-byte-1 (byte-1)
    (format t "1-byte:~b" byte-1)

  (let ((mask (= 1 (ldb (byte 1 7) byte-1)))
        (len (ldb (byte 7 0) byte-1)))
    (values mask len)))
    

(defun decode-len (stream payload-len)
  (format t "DECODE LEN ~a"payload-len)
  (case payload-len
    (126 (decode-2-byte-len stream))
    (127 (decode-8-byte-len stream))
    (t payload-len)))


(defun decode-2-byte-len (stream)
  (let ((out 0))
    (setf (ldb (byte 8 8) out) (read-byte stream nil nil))
    (setf (ldb (byte 8 0) out) (read-byte stream nil nil))
    out))


(defun decode-8-byte-len (stream)
  (let ((out 0))
    (setf (ldb (byte 8 56) out) (read-byte stream nil nil))
    (setf (ldb (byte 8 48) out) (read-byte stream nil nil))
    (setf (ldb (byte 8 40) out) (read-byte stream nil nil))
    (setf (ldb (byte 8 32) out) (read-byte stream nil nil))
    (setf (ldb (byte 8 24) out) (read-byte stream nil nil))
    (setf (ldb (byte 8 16) out) (read-byte stream nil nil))
    (setf (ldb (byte 8 8) out) (read-byte stream nil nil))
    (setf (ldb (byte 8 0) out) (read-byte stream nil nil))
    out))


(defun decode-mask (stream has-mask)
  (when has-mask
    (let ((out 0))
      (setf (ldb (byte 8 24) out) (read-byte stream nil nil))
      (setf (ldb (byte 8 16) out) (read-byte stream nil nil))
      (setf (ldb (byte 8 8) out) (read-byte stream nil nil))
      (setf (ldb (byte 8 0) out) (read-byte stream nil nil))
      out)))
