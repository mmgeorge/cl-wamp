(ql:quickload "expect")
(ql:quickload "wamp")
(ql:quickload "cl-async")
(ql:quickload "blackbird")

(as:start-event-loop
 (lambda ()
   (bb:attach
    (expect:run-tests "wamp" t)
    (lambda (report)
      (format t "Got failed: ~A~%" (expect/report/report:nested-failed-length report))
      
      (as:exit-event-loop)))
   (as:add-event-loop-exit-callback
    #'(lambda ()
        (format t "Event loop exited..~%")))))

