(defpackage :wamp/message-type
  (:use :cl)
  (:export #:message-t
           #:hello
           #:welcome
           #:abort
           #:goodbye
           #:error
           #:publish
           #:published
           #:subscribe
           #:unsubscribe
           #:unsubscribed
           #:event
           #:call
           #:result
           #:unregister
           #:unregistered
           #:invocation
           #:yield))

(in-package :wamp/message-type)

(deftype message-t ()
  '(member
    hello
    welcome
    abort
    goodbye
    error
    publish
    published
    subscribe
    unsubscribe
    unsubscribed
    event
    call
    result
    unregister
    unregistered
    invocation
    yield))
