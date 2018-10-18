(defpackage :wamp/ws/protocol/base
  (:use :cl)
  (:export #:protocol #:recieve #:send))

(in-package :wamp/ws/protocol/base)


(defclass protocol ()
  ())


;; -> message | nil
(defgeneric recieve (self client))

;; -> nil
;;#[export, ftype message stream -> nil]
(defgeneric send (self target client))
