(defpackage :wamp/decorators
  (:use :cl)
  (:export #:dtype))

(in-package :wamp/decorators)



(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun parse-decorator (expr body)
    ;; Each decorator in our decorators list by either me an atom
    ;; (if it takes no args) or a list
    (if (atom expr) `(,expr ,body) ;; 1- #[export] 
        (let ((name (car expr))
              (args (cadr expr)))
          (if args 
              `(,name ,args ,body) ;; 2- #[(ftype (fixnum) fixnum)]
              `(,name ,body))))))  ;; 3- #[(export)] - as in (1) (maybe prohibit this?)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-decorators (stream char char2)
    (declare (ignore char char2))
    (let* ((decorators (read-delimited-list #\] stream t))
           (fbody (read stream))
           (parsed (mapcar #'(lambda (x) (parse-decorator x fbody)) decorators)))
      `(progn ,@parsed))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun values-p (type-spec)
    (and (listp type-spec)
         (symbolp (car type-spec))
         (string-equal "values" (symbol-name (car type-spec))))))


(defmacro define-decorator (name (args form) &body body)
  `(defmacro ,name (,args ,form)
     `(progn ,,@body ,,form)))

;(set-macro-character #\@ #'parse-decorator)

(set-macro-character #\] (get-macro-character #\)))
(set-dispatch-macro-character #\# #\[ 'parse-decorators)


(define-decorator dtype (args form)
  (let* ((fun-arg-types (car args))
         (fun-ret-type (cadr args))
         (fun-name (cadr form))
         (fun-ret-values (if (values-p fun-ret-type) fun-ret-type
                             `(values ,fun-ret-type &optional))))
    `(declaim (ftype (function ,fun-arg-types ,fun-ret-values) ,fun-name))))


;; (defmacro dtype (name arg-types ret-type)
;;   (let ((ret (if (values-p ret-type)
;;                  ret-type
;;                  `(values ,ret-type &optional))))
;;     `(declaim (ftype (function ,arg-types ,ret) ,name))))


;; (defmacro dtype (args form)
;;   (let* ((fun-arg-types (car args))
;;          (fun-ret-type (cadr args))
;;          (fun-name (cadr form))
;;          (fun-ret-values (if (values-p fun-ret-type) fun-ret-type
;;                              `(values ,fun-ret-type &optional))))
;;     `(declaim (ftype (function ,fun-arg-types ,fun-ret-values) ,fun-name))))




