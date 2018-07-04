(asdf:defsystem :wamp
  :class :package-inferred-system
  :depends-on (:wamp/exports))

(asdf:register-system-packages :wookie '(:wookie))
(asdf:register-system-packages :websocket-driver '(:websocket-driver))
