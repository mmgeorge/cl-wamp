;; (defpackage :wamp/test/session
;;   (:use :cl :rove :wamp/session)
;;   ;;(:import-from :rove)
;;   (:import-from :blackbird #:attach #:promise-finished-p #:catcher)
;;   (:import-from :wamp/session)
;;   (:import-from :wamp/transport/transport)
;;   (:import-from :wamp/util #:local-nicknames))

;; (in-package :wamp/test/session) 


;; (defvar *server* "ws://138.68.246.180:8080/ws")


;; (defun await (promise)
;;   (loop while (not (promise-finished-p promise)) do (sleep 0.1)))


;; (deftest session-open-test 
;;   (testing "handshake sets session id"
;;     (let* ((session (make-session *server* "realm1" :log-output (make-broadcast-stream)))
;;            (is-open (start session)))
;;       (await is-open)
;;       (ok (not (equal (id session) 0))))))


;; (defun my-add (a b)
;;   (+ a b))


;; (deftest session-register-test
;;   (testing "session registration - singleton, before start"
;;     (let* ((session (make-session *server* "realm1"))
;;            (registration-promise (catcher
;;                                   (attach (register session "com.app.my-add" #'my-add :invoke "random")
;;                                           (lambda (reg) (describe reg)
;;                                             (format t "MADE IT!")
;;                                             (ok (not (equal (registration-id reg) 0)))))
;;                                   (t (e) (format t "~a" e)))))
;;       (start session)
;;       (await registration-promise)
;;       (describe registration-promise)
;;       (format t "~a" registration-promise)
;;       )))
