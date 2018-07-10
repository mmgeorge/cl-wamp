
(asdf:defsystem "wamp"
  :class :package-inferred-system
  :pathname "core"
  :depends-on (:wamp/exports)
  :in-order-to ((asdf:test-op (asdf:test-op "wamp/test"))))

(asdf:defsystem "wamp/test"
  :class :package-inferred-system
  :depends-on (:wamp/test/transport)
  :perform (test-op (op c) (uiop:symbol-call :rove '#:run c)))

(asdf:register-system-packages :rove '(:rove))
(asdf:register-system-packages :websocket-driver '(:websocket-driver))
(asdf:register-system-packages :cl-json '(:cl-json))
