# cl-wamp
A [WAMP](https://wamp-proto.org/) client for Common Lisp applications. Currently supports the basic client profile for RPC (pub-sub support is planned but not yet implemented). 

## Basic Usage
First let's declare a new package for our application, importing `:wamp/session` and `:blackbird` (required as the `cl-wamp` makes use of promises). 

```cl
(defpackage :myapp/main
  (:use cl) 
  (:import-from :wamp/session #:make-session)
  (:import-from :blackbird #:wait))

;; provide a new nickname to :wamp/session
(package-rename :wamp/session :wamp/session '(:session))
```
Next let's create a new `session` that listens to a WAMP router we have hosted locally. A given WAMP router may have multiple realms, arbitarily we have chosen one named realm1. 

```cl
;; Create a session on "realm1", connecting to a local WAMP router listening on 8080. 
(defvar *session* (make-session "ws://localhost:8080/ws" "realm1"))
```

Now we can register a function with the session. When the `session` starts, it will register all registered functions with the router. The method returns a `(promise-of registration)` which can later be used to unregister the function. 
```cl
(defun myadd (a b)
  (+ a b))

(session:register *session* "com.myapp.myadd" #'myadd)
```
Using the `wait` method of the `blackbird` promise library, let wait for the `session` to start, and afterwards, call our function:
```cl
(wait (session:open *session*)
  (attach (session:call *session* "com.myapp.myadd" :args '(1 2))
     (lambda (args kwargs)
       (declare (ignore kwargs))
       (format t "Got! ~a" args)))) ;; -> Got! 3
  
```

## Reference 
TBC

## License 
MIT
