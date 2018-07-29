(defpackage :wamp/test/transport
  (:use :cl :rove  :wamp/message-type)
  (:import-from :rove)
  (:import-from :wamp/message-type)
  (:import-from :wamp/transport/transport)
  (:import-from :wamp/transport/websocket))

(in-package :wamp/test/transport)

(defvar *sock* (websocket:make-websocket ""))


(deftest transport-deserialize-test
  (testing "decode a json message"
    (ok (equal (transport:deserialize *sock* "[2,101,{}]")
               '(wamp/message-type:welcome 101 nil)))))

(deftest transport-serialize-test
  (testing "serialize a messge"
    (ok (equal (transport:serialize *sock* (list 'wamp/message-type:welcome 101)) "[2,101]"))))
