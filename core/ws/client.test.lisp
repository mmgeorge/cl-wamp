(defpackage :wamp/ws/client.test 
  (:use :cl :expect :wamp/ws/client)
  (:import-from :wamp/ws/server)
  (:local-nicknames (:server :wamp/ws/server)))

(in-package :wamp/ws/client.test)


(defixture f-server ()
  (with-cleanup
      (server:start (make-instance 'server:server :url "ws://0.0.0.0:8084/ws" :hostname "localhost"))
    (lambda (server)
      (server:stop server))))


(defixture f-client ()
  (with-cleanup
      (start (make-instance 'client :host "localhost" :port 8084))
    (lambda (client)
      (stop client))))


(deftest-of ping (f-server (client f-client))
  "no application data"
   (bb:attach
    (ping client nil)
    (lambda (data end)
      (expect (eq nil data))
      (expect (eq nil end)))))


(deftest-of ping (f-server (client f-client))
  "with application data"
  (bb:attach
   (ping client "abcd")
   (lambda (data end)
     (expect (equalp data #(97 98 99 100)))
     (expect (eq end 4)))))


(deftest-of ping (f-server (client f-client))
  "with application data - start, end"
  (bb:attach
   (ping client "abcd" :start 1 :end 2)
   (lambda (data end)
     (expect (equalp data #(98)))
     (expect (eq end 1)))))
