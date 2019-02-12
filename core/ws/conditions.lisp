(defpackage :wamp/ws/conditions
  (:use :cl)
  (:import-from :wamp/ws/evented #:condition-event)
  (:export :protocol-error :auth-error :connection-error))

(in-package :wamp/ws/conditions)


(define-condition protocol-error (condition-event error) ())
(define-condition auth-error (condition-event error) ())
(define-condition connection-error (condition-event error) ())
