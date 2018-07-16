(defpackage :wamp/test/session
  (:use :cl :rove
   :wamp/session
   :wamp/transport)
  (:import-from :rove)
  (:import-from :blackbird)
  (:import-from :wamp/session)
  (:import-from :wamp/transport)
  (:import-from :wamp/message-type))

(in-package :wamp/test/session) 

(defvar *sock2* (make-websocket ""))


(deftest session-open-test 
  (testing "hello-welcome handshake is successful"
    (let* ((session (make-session *sock2* "realm1"))
          (is-open (session-open session)))
      (transport-mock *sock2* '(2 101))
      (bb:wait is-open
        (ok (equal (session-id session) 101))))))
