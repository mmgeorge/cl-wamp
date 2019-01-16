(defpackage :wamp/ws/conditions
  (:use :cl)
  (:import-from :wamp/ws/evented #:condition-event)
  (:export :protocol-error :auth-error :connection-error))

(in-package :wamp/ws/conditions)


(define-condition protocol-error (condition-event) ())
(define-condition auth-error (condition-event) ())
(define-condition connection-error (condition-event) ())
