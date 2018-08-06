
(let ((*standard-output* (make-string-output-stream)))
  (asdf:test-system "wamp/test")
  (let ((output (get-output-stream-string *standard-output*)))
    (format t "~a" output)
    (if (search "failed" output)
        (format t "It failed!")
        (format t "It worked!"))
