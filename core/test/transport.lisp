(defpackage :wamp/test/transport
  (:use :cl :rove :wamp/transport)
  (:import-from :wamp/transport))

(in-package :wamp/test/transport)

(defvar *sock* (make-websocket "test"))

(deftest transport-mock-test
  (testing "identity - decode a simple list"
    (ok (equal (transport-mock *sock* '(1 2 3 4)) '(1 2 3 4)))))

