(ql:quickload "expect")
(ql:quickload "wamp")
(ql:quickload "cl-async")
(ql:quickload "blackbird")


(as:start-event-loop
 (lambda ()
   (bb:attach
    (expect:run-tests "wamp" t)
    (lambda (report)
      (let* ((path (make-pathname :directory '(:relative "test-output")))
             (output (expect/report/output/junit:make path)))
        (expect/report/report:write-to-output report output))
      (as:exit-event-loop)
      (uiop:quit (expect/report/report:nested-failed-length report))))))

