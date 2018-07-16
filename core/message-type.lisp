(defpackage :wamp/message-type
  (:use :cl)
  (:nicknames :mtype)
  (:export #:message-t
           
           #:hello
           #:welcome
           #:abort
           #:goodbye
           #:error
           #:publish
           #:published
           #:subscribe
           #:subscribed
           #:unsubscribe
           #:unsubscribed
           #:event
           #:call
           #:result
           #:register
           #:registered
           #:unregister
           #:unregistered
           #:invocation
           #:yield

           #:code-to-message-t
           #:message-t-to-code))

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
    subscribed
    unsubscribe
    unsubscribed
    event
    call
    result
    register
    registered
    unregister
    unregistered
    invocation
    yield))

(defun code-to-message-t (code)
  (declare (fixnum code))
  (the message-t
       (case code
         (1 'hello)
         (2 'welcome)
         (3 'abort)
         (6 'goodbye)
         (8 'error)
         (16 'publish)
         (17 'published)
         (32 'subscribe)
         (33 'subscribed)
         (34 'unsubscribe)
         (35 'unsubscribed)
         (36 'event)
         (48 'call)
         (50 'result)
         (64 'register)
         (65 'registered)
         (66 'unregister)
         (67 'unregistered)
         (68 'invocation)
         (70 'yield)
         (t (error "Encountered unknown message type: ~a" code)))))


(defun message-t-to-code (message-type)
  (declare (type message-t message-type))
  (the fixnum
       (case message-type
         (hello 1)
         (welcome 2)
         (abort 3)
         (goodbye 6)
         (error 8)
         (publish 16)
         (published 17)
         (subscribe 32)
         (subscribed 33)
         (unsubscribe 34)
         (unsubscribed 35)
         (event 36)
         (call 48)
         (result 50)
         (register 64)
         (registered 65)
         (unregister 66)
         (unregistered 67)
         (invocation 68)
         (yield 70)
         (t (error "Encountered unknown message type: ~a" message-type)))))
