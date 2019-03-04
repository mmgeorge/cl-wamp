(defpackage :wamp/ws/server.test 
  (:use :cl :expect :blackbird :wamp/ws/server :wamp/ws/evented :wamp/util)
  (:import-from :wamp/ws/client)
  (:local-nicknames
    (:client :wamp/ws/client)
    (:server :wamp/ws/server)
    (:session :wamp/ws/session/session)))

(in-package :wamp/ws/server.test)


(defclass test-client (client:client)
  ((on-message :reader on-message :initarg :on-message)))


(defmethod client:recieve-text ((self test-client) text)
  (funcall (on-message self) self text))


(defixture f-server ()
  (with-cleanup
      (start (make-instance 'echo-server :url "ws://0.0.0.0:8084/ws" :hostname "localhost"))
    (lambda (server)
      (stop server))))


(defixture f-client ()
  (with-cleanup
      (start (make-instance 'test-client :host "localhost" :port 8084))
    (lambda (client)
      (stop client))))


(deftest-of accept-client ((server f-server))
  "Accept a client, handle dropped connection"
  (setf (cleanup-frequency server) 0.1)
  (expect (eq (length (sessions server)) 0))
  (let* ((client (make-instance 'client:client :host "localhost" :port 8084)))
    (wait (client:start client)
      (expect (eq (length (sessions server)) 1))
      (wait (client:stop client)
        (wait (delay 1)
          (expect (eq (length (sessions server)) 0)))))))


(deftest-of echo-message (f-server)
  "Echo client message"
  (bb:attach
   (bb:with-promise (resolve reject)
     (let* ((client (make-instance 'test-client
                                   :host "localhost" :port 8084
                                   :on-message #'(lambda (client message)
                                                   (resolve client message)))))
       (bb:wait (client:start client)
         (client:send-text client "hello world"))))
   (lambda (client message)
     (client:stop client)
     (expect (string-equal message "hello world")))))
    
  
 
