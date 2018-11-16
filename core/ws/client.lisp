(defpackage :wamp/ws/client
  (:use :cl :alexandria)
  (:import-from :usocket)
  (:import-from :fast-http)
  (:import-from :flexi-streams)
  (:import-from :bordeaux-threads)
  (:import-from :wamp/ws/session/http #:http)
  (:import-from :wamp/ws/session/websocket #:websocket)
  (:import-from :wamp/ws/session/session)
  (:local-nicknames (:session :wamp/ws/session/session))
  (:export))


(in-package :wamp/ws/client)


(defclass client ()
  ((session :reader session :initarg :session)))


(defun make-client (server port &key (bufsize 1024))
  "Create a new websocket connection, blocking until the session has been established"
  (let* ((sock (usocket:socket-connect server port  :element-type '(unsigned-byte 8)))
         (session (change-class sock 'http  :bufsize bufsize)))
    (session:upgrade-request session)
    (usocket:wait-for-input session)
    (multiple-value-bind (response buffer start end) (session:recieve session)
      (case (fast-http:http-status response)
        ;; TODO - Must actually verify correct upgrade response sent
        
        (101 (make-instance 'client :session (change-class sock 'websocket)))
        (t (error
            "Unable to establish connection, encountered ~a:~%   ~a~%"
            (fast-http:http-status response)
            (flexi-streams:octets-to-string buffer :start start :end end :external-format :utf-8)))))))



(defvar *client* nil)
(defvar *session-read-thread* nil)

(defun test ()
  (let ((message (flexi-streams:string-to-octets "heyiifications"))
        ;(os *standard-output*)
        )
    (when (and *session-read-thread* (bt:thread-alive-p *session-read-thread*))
          (bt:destroy-thread *session-read-thread*))
    (setf *client* (make-client "dev.owny.io" 8081))
    (session:send (session *client*) message :start 0 :end (length message))
    (usocket:wait-for-input (session *client*))
    (when-let ((result (session:recieve (session *client*))))
      (destructuring-bind (type buffer end) result
        (declare (ignore type))
        (format t "Got message ~a" (flexi-streams:octets-to-string buffer :start 0 :end end :external-format :utf-8))
      ))
    
    ;; (setf *session-read-thread*
    ;;       (bt:make-thread
    ;;        (lambda ()
    ;;          (let ((*standard-output* os))
    ;;            (multiple-value-bind (type buffer start end) (session:recieve (session *client*))
    ;;              (format os "WE GOT IT! ~a"
    ;;                      (flexi-streams:octets-to-string buffer :start 0 :end end :external-format :utf-8)
    ;;                      ))))))

    ))


