(defpackage :wamp/test/session
  (:use :cl :rove :wamp/session)
  (:import-from :rove)
  (:import-from :blackbird #:attach #:promise-finished-p)
  (:import-from :wamp/session)
  (:import-from :wamp/transport/transport)
  (:import-from :wamp/util #:local-nicknames))

(in-package :wamp/test/session) 


(defvar *server* "ws://138.68.246.180:8080/ws")

(deftest session-open-test 
  (testing "handshake sets session id"
    (let* ((session (make-session *server* "realm1"))
           (is-open (start session)))
      (attach is-open (lambda (x) (ok (not (equal (id session) 0))))))))
