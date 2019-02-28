#-asdf3.3 (error "Building requires asdf 3.3 but ~a is installed" (asdf:asdf-version))

(asdf:defsystem "wamp"
  :class :package-inferred-system
  :pathname "core"
  :depends-on ("wamp/session"
               "wamp/transport/websocket"
               "wamp/ws/server"
               "wamp/ws/client"))


(asdf:register-system-packages :cl-async '(:cl-async))
(asdf:register-system-packages :blackbird '(:blackbird))
(asdf:register-system-packages :lparallel '(:lparallel))
(asdf:register-system-packages :cl-json '(:cl-json))
