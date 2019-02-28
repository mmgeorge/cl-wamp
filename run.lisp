(ql:quickload "expect")
(ql:quickload "wamp")
(ql:quickload "cl-async")

(as:start-event-loop
 (lambda ()
   (expect:run-tests "wamp")
   (as:add-event-loop-exit-callback
    #'(lambda ()
        (format t "Event loop exited..~%")))))

