(defpackage :wamp/test/transport
  (:use :cl :rove :wamp/transport :wamp/message-type)
  (:import-from :rove)
  (:import-from :wamp/message-type)
  (:import-from :wamp/transport))

(in-package :wamp/test/transport)

(defvar *sock* (make-websocket ""))


(deftest transport-deserialize-test
  (testing "decode a json message"
    (ok (equal (transport-deserialize *sock* "[2,101,{}]")
               '(wamp/message-type:welcome 101 nil)))))

(deftest transport-serialize-test
  (testing "serialize a messge"
    (ok (equal (transport-serialize *sock* '(wamp/message-type:welcome 101)) "[2,101]"))))
