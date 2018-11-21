#-asdf3.3 (error "Building requires asdf 3.3 but ~a is installed" (asdf:asdf-version))

(asdf:defsystem "wamp"
  :class :package-inferred-system
  :pathname "core"
  :depends-on ("wamp/session"
               "wamp/transport/websocket"
               "wamp/ws/server"
               "wamp/ws/client"
               "wamp/test/ws/client")
  :in-order-to ((asdf:test-op (asdf:test-op "wamp/test"))))


(asdf:defsystem "wamp/test"
  :class :package-inferred-system
  :pathname "core"
  :depends-on (:wamp/test/transport
               :wamp/test/session)
  :perform (test-op (op c) (uiop:symbol-call :rove '#:run c)))


;;(asdf:register-system-packages :cl-async '(:cl-async))
(asdf:register-system-packages :blackbird '(:blackbird))
(asdf:register-system-packages :lparallel '(:lparallel))
(asdf:register-system-packages :cl-json '(:cl-json))
