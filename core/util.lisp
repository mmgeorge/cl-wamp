(defpackage :wamp/util
  (:use :cl)
  (:import-from :blackbird #:create-promise)
  (:import-from :lparallel #:future)
  (:import-from :cl-async)
  (:export #:with-timed-promise
           #:timeout-exceeded
           #:promise-of
           #:define-ftype
           #:defunx
           #:start-event-loop
           #:create-resolver #:resolver #:resolver-promise #:resolver-resolve-fn #:resolver-reject-fn
           #:delay))

(in-package :wamp/util)


(defun start-event-loop ()
  (let ((slynk (find-package "SLYNK"))
        (slynk-mrepl (find-package "SLYNK-MREPL")))
    (progn
      (when slynk-mrepl
        (funcall (symbol-function (find-symbol "SEND-PROMPT" slynk-mrepl))
                 (symbol-value (find-symbol "*REPL*" slynk-mrepl))))
    (as:start-event-loop
     (lambda ()
       (as:add-event-loop-exit-callback
        #'(lambda ()
            (format t "Event loop exited..~%")))
       (as:idle
        (lambda ()
          (when slynk
            (let ((default-conn (funcall (symbol-function (find-symbol "DEFAULT-CONNECTION" slynk)))))
              (funcall (symbol-function (find-symbol "HANDLE-REQUESTS" slynk)) default-conn t)))
          )))))))


(unless lparallel:*kernel*
  (setf lparallel:*kernel* (lparallel:make-kernel 2)))


(defstruct resolver
  resolve-fn
  reject-fn
  promise)


(defun create-resolver ()
  (let* (resolve-fn
         reject-fn
         (promise
           (create-promise
            (lambda (resolve reject)
              (setf resolve-fn resolve)
              (setf reject-fn reject)))))
    (make-resolver :promise promise :resolve-fn resolve-fn :reject-fn reject-fn)))


    
(defmacro local-nicknames (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop for (package nickname) on body by #'cddr
             collect `
             #+sbcl (sb-ext:add-package-local-nickname ,nickname ,package )
             #-sbcl (rename-package ,package ,package '(,nickname)))))


(defun promise-of-type-p (promise type)
  (let ((result-values (slot-value promise 'values)))
    (or (null result-values)
        (and (typep (car result-values) type)
             (null (cdr result-values))))))


(deftype promise-of (type)
  (let ((predicate (gensym)))
    (setf (symbol-function predicate)
          #'(lambda (object) (promise-of-type-p object type)))
    `(and bb:promise (satisfies ,predicate))))


(define-condition timeout-exceeded (error)
  ((text :initarg :text :reader text)))


(defmacro with-timed-promise  (timeout
                               (resolve reject
                                &key (resolve-fn (gensym "resolve-fn"))
                                  (reject-fn (gensym "reject-fn")) name)
                               &body body)
  "Creates a promise that will reject after the given timeout has elapsed"
  `(create-promise
    (lambda (,resolve-fn ,reject-fn)
      (declare (ignorable ,resolve-fn ,reject-fn))
      (macrolet ((,resolve (&rest args)
                   (if (= 1 (length args))
                       `(apply ,',resolve-fn (multiple-value-list ,(car args)))
                       `(funcall ,',resolve-fn ,@args))))
        (flet ((,reject (condition) (funcall ,reject-fn condition)))
          (declare (ignorable #',reject))
          (future (sleep ,timeout) (,reject (make-condition 'timeout-exceeded))) 
          ,@body)))
    :name ,name))


(defun delay (time)
  (bb:with-promise (resolve reject)
    (as:delay #'(lambda () (resolve)) :time time)))
