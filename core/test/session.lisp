(defpackage :wamp/test/session
  (:use :cl :rove :wamp/session)
  (:import-from :rove)
  (:import-from :blackbird #:attach #:promise-finished-p)
  (:import-from :wamp/session)
  (:import-from :wamp/transport/transport)
  (:import-from :wamp/transport/websocket)
  (:import-from :wamp/util #:local-nicknames))

(in-package :wamp/test/session) 


(defvar *sock2* (websocket:make-websocket ""))

 
(deftest session-open-test 
  (testing "handshake sets session id"
    (let* ((session (make-session *sock2* "realm1"))
           (is-open (start session)))
      (transport:mock *sock2* '(2 101))
      (attach is-open (lambda (x) (ok (equal (id session) 101)))))))


(deftest session-open-timeout-test
  (testing "handshake timeout leaves id unset"
    (let* ((session (make-session *sock2* "realm1"))
           (is-open (start session)))
      ;; Block until the promise is finished
      (loop while (not (promise-finished-p is-open)) do (sleep 1))
      (ok (equal (id session) 0)))))
      ;(ok (signals (attach is-open (lambda (x) ())) 'timeout-exceeded)))))
