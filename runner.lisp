
(let ((error-output *error-output*)
      (standard-output *standard-output*)
      (ignore-stream (make-broadcast-stream)))

  ;; Squelch errors
  (setf *error-output* ignore-stream)
  (setf *standard-output* ignore-stream)
  
  (asdf:load-system :wamp)
  (asdf:load-system :wamp/test)
  
  (setf *error-output* error-output)
  (setf *standard-output* standard-output))

;(setf *error-output* (make-broadcast-stream))
;(asdf:load-system )
(asdf:test-system :wamp)
