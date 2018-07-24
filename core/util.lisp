(defpackage :wamp/util
  (:use :cl)
  (:import-from :blackbird #:create-promise)
  (:import-from :lparallel #:future)
  (:export #:with-timed-promise
           #:timeout-exceeded
           #:promise-of
           #:defunc))

(in-package :wamp/util)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun amp-p (sym)
    "Detect an &arg"
    (and (symbolp sym) (eql #\& (char (symbol-name sym) 0)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun type-specifier-p (form)
    "Attempts to check whether FORM is a typespecifier. To ensure portability, 
     use (values [TYPESPECIFIER]*)"
    (or
     ;; First try to detect type in a platform-dependent way
     #+sbcl (sb-ext:valid-type-specifier-p form)
     #-sbcl (documentation type-specifier 'type)
     #+openmcl (ccl:type-specifier-p form)
     #+ecl (c::valid-type-specifier form)
     #+clisp (null (nth-value 1 (ignore-errors (ext:type-expand form))))

     ;; Fallback - check for a values form or a symbol
     (and (listp form)
          (symbolp (car form))
          (string-equal "values" (symbol-name (car form))))
     (symbolp form))))


(defmacro defunc (name arglist &body body)
  "A macro that can be used to define a function and it's associated ftype
   Examples: 
     Declare a function that adds two fixnums:
       (defun myadd ((fixnum a) (fixnum b)) fixnum 
         (+ a b)

     Additionally the macro supports agreggating types as with declare: 
       (defun myadd ((fixnum a b)) fixnum 
         (+ a b)

     Any valid type specifier works, so we can also declare multiple return values, i.e. 
       (defun my-multivalue-add ((fixnum a b)) (values fixnum fixnum)
         (values (+ a b) a b)"
  (let* ((f-args nil)
         (ftype-args nil)
         (ret-type (and (type-specifier-p (car body)) (car body))))
    (loop for form in arglist
          ;; If we have a typename, then the argument is a list of (TYPENAME &rest VARS)
          if (listp form) do
            (destructuring-bind (type &rest vars) form
              (dolist (var vars)
                (push type ftype-args)
o                (push var f-args)))
            ;; Either we have untyped fun arg
          else do
            (push form f-args)
            ;; &args we pass onto both the ftype-args and f-args
            (push (if (amp-p form) form '*) ftype-args))
    `(progn
       (declaim (ftype (function ,(reverse ftype-args) ,(or ret-type '*)) ,name))
       (defun ,name (,@(reverse f-args)) ,@(if ret-type (cdr body) body)))))


(unless lparallel:*kernel*
  (setf lparallel:*kernel* (lparallel:make-kernel 2)))


(defun promise-of-type-p (promise type)
  (let ((result-values (slot-value promise 'values)))
    (or (null result-values)
        (and (typep (car result-values) type)
             (null (cdr result-values))))))


(deftype promise-of (type)
  (let ((predicate (gentemp)))
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



