(defpackage :wamp/ws/session/websocket
  (:use :cl :alexandria :wamp/ws/session/session)
  (:import-from :wamp/ws/session/session #:session)
  (:import-from :wamp/ws/session/http #:http)
  (:import-from :wamp/ws/conditions #:connection-error)
  (:local-nicknames (:session :wamp/ws/session/session))
  (:export #:websocket #:make-websocket #:ping #:pong #:status-code))

(in-package :wamp/ws/session/websocket)


(defvar *max-frame-len* 65535)


(defclass websocket (session)
  ((mask-frames :accessor mask-frames-p :initform nil)
   ;; Current parser state
   (current-op :accessor current-op :initform nil)
   (status-code :accessor status-code :initform nil)))


(defgeneric send (self data &key start end))
(defgeneric pong (self data-or-nil &key start end))
(defgeneric ping (self data-or-nil &key start end))

;; Exports

(defmethod session:recieve ((self websocket))
  (with-accessors ((stream session::socket-stream)) self
    (when-let ((message (multiple-value-list (read-frame self stream))))
      (when (car message)
        (reset self)
        message))))


(defmethod session:send-text ((self websocket) data &key start end (max-len *max-frame-len*))
  (with-accessors ((stream session::socket-stream) (use-mask mask-frames-p)) self
    (write-frames stream :text data :start start :end end :use-mask (mask-frames-p self) :max-len max-len)))


(defmethod session:send-binary ((self websocket) data &key start end (max-len *max-frame-len*))
  (with-accessors ((stream session::socket-stream) (use-mask mask-frames-p)) self
    (write-frames stream :binary data :start start :end end :use-mask use-mask :max-len max-len)))


(defmethod ping ((self websocket) data-or-nil &key (start 0) end)
  (write-ping-pong-frame self :ping data-or-nil :start start :end end))


(defmethod pong ((self websocket) data-or-nil &key (start 0) end)
  (write-ping-pong-frame self :pong data-or-nil :start start :end end))


(defmethod session:send-close ((self websocket) code reason?)
  (let* ((message (session:normalize-reason reason?))
         (data (flex:string-to-octets message :external-format :utf-8)))
    (with-accessors ((stream session::socket-stream) (use-mask mask-frames-p)) self
      (write-frame stream :close data :code code :use-mask use-mask :start 0 :end (length data)))))


(defun write-ping-pong-frame (self opsym buffer &key start end)
  (with-accessors ((stream session::socket-stream)) self
    (if buffer
        (write-frame stream opsym buffer :start start :end end)
        (write-frame stream opsym nil :start 0 :end 0))))


(defun write-frames (stream opsym data &key start end max-len use-mask)
  (let ((end (or end (length data))))
    (loop for frame-start = start then (+ frame-start max-len)
          for frame-end = (min end (+ frame-start max-len))
          for fin = (eq end frame-end)
          do (write-frame stream opsym data :fin fin :start frame-start :end frame-end :use-mask use-mask)
          until fin)))
  

;; Internal

(defun reset (self)
  (setf (index self) 0)
  (setf (current-op self) nil))


(defun control-frame-p (opsym)
  (or (eq opsym :close) (eq opsym :ping) (eq opsym :pong)))


;; See https://tools.ietf.org/html/rfc6455#section-5.2
(defun read-frame (self stream &key (expects-rsv nil))
  ;;(when (not (listen stream))
  ;; (error 'connection-error :session self :name :lost-connection))
  (multiple-value-bind (fin rsv opsym len mask) (read-header stream expects-rsv)
    (declare (ignore rsv))
      ;;(format t "Got fin:~a opsym:~a len:~a mask~a~%" fin opsym len mask)
      (if (control-frame-p opsym)
          (read-control-frame self stream opsym len mask)
          (read-standard-frame self stream fin opsym len mask))))


(defun read-control-frame (self stream opsym len mask)
  (declare (ignore self))
  ;;(format t "Reading control frame ~A: ~A~%" opsym len)
  (if (> len 0)
      (let* ((buffer (make-array len :element-type '(unsigned-byte 8)))
             (index (if mask
                        (read-masked-body stream buffer mask 0 len)
                        (read-body stream buffer 0 len))))
        (values opsym buffer index))
      (values opsym nil nil)))


(defun read-standard-frame (self stream fin opsym len mask)
  (with-accessors ((index session::index) (buffer session::buffer)) self
    (when (> (+ index len) (length buffer))
      (error "Cannot read frame - message length ~a too large for buffer" len))
    (setf (current-op self) opsym)
    (setf index
          (if mask
              (read-masked-body stream buffer mask index len)
              (read-body stream buffer index len)))
    ;;(format t "Read std frame ~%")
    (when fin (values (current-op self) buffer index))))


(defun write-frame (stream opsym data &key code (fin t) (start 0) end (rsv nil) use-mask)
  (let* ((end (or end (length data)))
         (len (+ (- end start) (if code 2 0)))
         (mask (and use-mask (random 2147483647))))
    (write-header stream fin opsym len mask rsv)
    (if mask
        (error "Writing masked frames is not currently support")
        (write-body stream data start end :code code)))
  (force-output stream))


(defun read-header (stream expects-rsv)
    (multiple-value-bind (fin rsv opcode)
        (decode-byte-0 (read-byte stream ) expects-rsv)
      (multiple-value-bind (has-mask payload-len) (decode-byte-1 (read-byte stream ))
        (let ((len (read-len stream payload-len))
              (mask (read-mask stream has-mask)))
          (values fin rsv opcode len mask)))))


(defun write-header (stream fin opcode len mask rsv)
  (multiple-value-bind (small-len ext-len-bytes) (encode-len len)
    ;(format t "~%Got len: ~a small-len ~a ~a~%" len small-len ext-len-bytes)
    ;(format t "Writing b1: ~b ~%" (encode-byte-0 fin rsv opcode))
    ;(format t "Writing b2: ~b ~%" (encode-byte-1 mask small-len))
    (write-byte (encode-byte-0 fin rsv opcode) stream)
    (write-byte (encode-byte-1 mask small-len) stream)
    (write-len stream len ext-len-bytes)
    (write-mask stream mask)))


(defun decode-byte-0 (byte-0 expects-rsv)
  (let ((fin (= 1 (ldb (byte 1 7) byte-0)))
        (rsv (ldb (byte 3 4) byte-0))
        (opcode (opcode-symbol (ldb (byte 4 0) byte-0))))
    (unless (or expects-rsv (= rsv 0)) (error "unxpected rsv ~a" rsv))
    ;(format t "0-byte:~b fin:~a rsv:~a" byte-0 rsv opcode)
    (values fin rsv opcode)))


(defun encode-byte-0 (fin rsv opsym)
  (let ((opcode (symbol-opcode opsym))
        (rsv (or rsv 0))
        (fin (if fin 1 0))
        (out 0))
    (setf (ldb (byte 1 7) out) fin)
    (setf (ldb (byte 3 4) out) rsv)
    (setf (ldb (byte 4 0) out) opcode)
    out))


(defun opcode-symbol (opcode)
  (case opcode
    (#x0 :continuation)
    (#x1 :text)
    (#x2 :binary)
    (#x8 :close)
    (#x9 :ping)
    (#xA :pong)
    (t (error "invalid opcode ~a" opcode))))


(defun symbol-opcode (opsym)
  (case opsym
    (:continuation #x0)
    (:text #x1)
    (:binary #x2)
    (:close #x8)
    (:ping #x9)
    (:pong #xA )
    (t (error "invalid opcode type ~a" opsym))))


(defun decode-byte-1 (byte-1)
  (let ((mask (= 1 (ldb (byte 1 7) byte-1)))
        (len (ldb (byte 7 0) byte-1)))
    (values mask len)))


(defun encode-byte-1 (mask len)
  (let ((out 0))
    (when mask (setf (ldb (byte 1 7) out) mask))
    (setf (ldb (byte 7 0) out) len)
    ;(format t "encode-byte-1 mask:~a len:~a ~b~%" mask len out)
    out))


(defun encode-len (payload-len)
  "Encode the length of a websocket frame. Returns two values LEN and LEN-EXT-BYTES. 
   Depending on the size of the payload LEN is a flag which is used to interpret 
   how to read LEN-EXT. See RFC6455 for details"
  (cond ((< payload-len 126) (values payload-len nil))
        ((<= payload-len 65535) (values 126 2))
        (t (values 127 8))))


(defun read-len (stream payload-len)
  ;;(format t "DECODE LEN ~a"payload-len)
  (case payload-len
    (126 (read-2-byte-len stream))
    (127 (read-8-byte-len stream))
    (t payload-len)))


(defun write-len (stream len ext-len-bytes)
  (when ext-len-bytes
    (case ext-len-bytes
      (2 (write-2-byte-len stream len))
      (8 (write-8-byte-len stream len))
      (t (error "Tried to encode paylod len with invalid byte length ~a" ext-len-bytes)))))


(defun read-2-byte-len (stream)
  (let ((out 0))
    (setf (ldb (byte 8 8) out) (read-byte stream ))
    (setf (ldb (byte 8 0) out) (read-byte stream ))
    out))


(defun write-2-byte-len (stream len)
  (write-byte (ldb (byte 8 8) len) stream)
  (write-byte (ldb (byte 8 0) len) stream))


(defun read-8-byte-len (stream)
  (let ((out 0))
    (setf (ldb (byte 8 56) out) (read-byte stream ))
    (setf (ldb (byte 8 48) out) (read-byte stream ))
    (setf (ldb (byte 8 40) out) (read-byte stream ))
    (setf (ldb (byte 8 32) out) (read-byte stream ))
    (setf (ldb (byte 8 24) out) (read-byte stream ))
    (setf (ldb (byte 8 16) out) (read-byte stream ))
    (setf (ldb (byte 8 8) out) (read-byte stream ))
    (setf (ldb (byte 8 0) out) (read-byte stream ))
    out))


(defun write-8-byte-len (stream len)
  (write-byte (ldb (byte 8 56) len) stream)
  (write-byte (ldb (byte 8 48) len) stream)
  (write-byte (ldb (byte 8 40) len) stream)
  (write-byte (ldb (byte 8 32) len) stream)
  (write-byte (ldb (byte 8 24) len) stream)
  (write-byte (ldb (byte 8 16) len) stream)
  (write-byte (ldb (byte 8 8) len) stream)
  (write-byte (ldb (byte 8 0) len) stream))


(defun read-mask (stream has-mask)
  (when has-mask
    (let ((out 0))
      (setf (ldb (byte 8 24) out) (read-byte stream ))
      (setf (ldb (byte 8 16) out) (read-byte stream ))
      (setf (ldb (byte 8 8) out) (read-byte stream ))
      (setf (ldb (byte 8 0) out) (read-byte stream ))
      out)))


(defun write-mask (stream mask)
  (when mask
    (write-byte (ldb (byte 8 24) mask) stream)
    (write-byte (ldb (byte 8 16) mask) stream)
    (write-byte (ldb (byte 8 8) mask) stream)
    (write-byte (ldb (byte 8 0) mask) stream)))


(defun read-body (stream buffer start end)
  (read-sequence buffer stream :start start :end end))


(defun write-body (stream data start end &key code)
  (when code
    (check-type code (integer 0 65535))
    (write-byte (ldb (byte 8 8) code) stream)
    (write-byte (ldb (byte 8 0) code) stream))
  (write-sequence data stream :start start :end end))


(defun read-masked-body (stream buffer mask-key start end)
  (loop for i from start below end
        for mask = (ldb (byte 8 (* 8 (- 3 (mod i 4)))) mask-key)
        for byte = (read-byte stream )
        for unmasked-byte = (logxor byte mask)
        do (setf (aref buffer i) unmasked-byte)
        finally (return (1+ i))))
