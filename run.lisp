(ql:quickload "expect")
(ql:quickload "wamp")
(ql:quickload "cl-async")
(ql:quickload "blackbird")

(as:start-event-loop
 (lambda ()
   (bb:wait (expect:run-tests "wamp")
     (as:exit-event-loop))
   (as:add-event-loop-exit-callback
    #'(lambda ()
        (format t "Event loop exited..~%")))))

