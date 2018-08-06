
(let ((real-stdout *standard-output*)
      (*standard-output* (make-string-output-stream)))
  (asdf:test-system "wamp/test")
  (let ((output (get-output-stream-string *standard-output*)))
    (format real-stdout "~a" output)
    (if (search "failed" output)
        (error "It failed!")
        (format t "It worked!"))))
