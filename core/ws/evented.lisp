(defpackage :wamp/ws/evented
  (:use :cl :blackbird)
  (:import-from :bordeaux-threads)
  (:export #:evented #:event #:condition-event
           #:handle #:next  #:*event-output-stream* ))

(in-package :wamp/ws/evented)


(defvar *event-output-stream* *standard-output*)


(defclass event ()
  ((timestamp :reader timestampe :initform (get-decoded-time))
   (name :initarg :name :reader name)
   (details :initarg :details :reader details)
   (session :initarg :session :reader session)))


(define-condition condition-event ()
  ((timestamp :reader timestamp :initform (get-decoded-time))
   (name :initarg :name :reader name)
   (details :initarg :details :reader details :initform nil)
   (session :initarg :session :reader session))
  (:report (lambda (condition stream)
             (print-event condition :stream stream))))


(defmethod print-event (event &key (stream *event-output-stream*))
  (format stream "[~{~a~^.~}:~a] ~a: ~a ~a~%"
          (coerce (usocket:get-peer-address (session event)) 'list)
          (usocket:get-peer-port (session event))
          (type-of event)
          (name event)
          (if (details event) (details event) "")))


(defclass evented ()
  ((resolvers :accessor resolvers :initform nil)
   (lock :reader lock :initform (bt:make-lock "resolver-lock"))))


(defmethod next ((self evented))
  (bt:with-lock-held ((lock self))
    (create-promise
     (lambda (resolver rejector)
       (declare (ignore rejector))
       (setf (resolvers self)
             (nconc (resolvers self) (list resolver)))))))
     


(defmethod handle ((self evented) event)
  (bt:with-lock-held ((lock self))
    (loop for resolver in (resolvers self) do
      (progn 
      (funcall resolver event)))
    (setf (resolvers self) nil)))
