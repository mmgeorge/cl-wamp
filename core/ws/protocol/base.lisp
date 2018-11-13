(defpackage :wamp/ws/protocol/base
  (:use :cl)
  (:export #:protocol #:recieve #:send #:send-error
           #:upgrade-request #:upgrade-accept))

(in-package :wamp/ws/protocol/base)


(defclass protocol ()
  ())


;; -> message | nil
(defgeneric recieve (self client))

;; -> nil
;;#[export, ftype message stream -> nil]
(defgeneric send (self target data &key start end))

(defgeneric send-error (self target message))
(defgeneric upgrade-request (self target-protocol))
(defgeneric upgrade-accept (self target-protocol))



