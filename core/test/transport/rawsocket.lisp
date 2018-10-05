(defpackage :owny/test/transport/rawsocket
  (:use :cl)
  (:import-from :usocket))

(in-package :owny/test/transport/rawsocket)




(defun echo-input (stream input)
  (format t "Got response ~a" input)
  (write-byte input stream)
  (force-output stream)
  )


(defun handle-client (client)
  (format t "Handling a client ~a" client)
  (with-open-stream (stream (usocket:socket-stream client))
    (loop for input = (read-byte stream nil nil)
          while input do 
            (echo-input stream input))
    t))


(defun create-echo-server (&optional (addr "localhost") (port 9092))
  (let ((sock (usocket:socket-listen addr port :reuse-address t :element-type '(unsigned-byte 8))))
    (unwind-protect
         (loop while (handle-client (usocket:socket-accept sock)))
      (usocket:socket-close sock)
    )))
