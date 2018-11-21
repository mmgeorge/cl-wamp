(defpackage :wamp/test/ws/client
  (:use :cl)
  (:import-from :expect #:deftest-of #:expect #:run-tests)
  (:import-from :wamp/ws/client :client)
  (:local-nicknames (:client :wamp/ws/client))
  (:export))

(in-package :wamp/test/ws/client)


;;(define-fixture client ())




;; (defclass test-client (client)
;;   ((message :initform nil)))

;; (defvar *client* (make-instance 'test-client :host "dev.owny.io" :port 8081))

;; (defixture client
;;   "A test client"
;;   (make-instance 'test-client :host "dev.owny.io" :port 8081)
;;   (lambda (self) (destroy self)))
  
;; (defmethod client:recieve ((self test-client) message &key)
;;   (setf (slot-value self 'message) message))


;; (client:send *client* "hi world")

;; (deftest-of client:recieve ()
;;   (expect (string-equal (slot-value *client* 'message) var)))


;; (deftest-of client:recieve ()
;;   "Handle authentication"
;;   (expect (string-equal value 1)))





;ws/client.test.lsip
;client.lisp
;client.test.lisp
