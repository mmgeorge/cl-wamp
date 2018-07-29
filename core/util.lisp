(defpackage :wamp/util
  (:use :cl)
  (:import-from :blackbird #:create-promise)
  (:import-from :lparallel #:future)
  (:export #:with-timed-promise
           #:timeout-exceeded
           #:promise-of
           #:define-ftype
           #:defunx))

(in-package :wamp/util)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun values-p (type-spec)
    (and (listp type-spec)
         (symbolp (car type-spec))
         (string-equal "values" (symbol-name (car type-spec))))))


(defmacro dtype (name arg-types ret-type)
  (let ((ret (if (values-p ret-type)
                 ret-type
                 `(values ,ret-type &optional))))
    `(declaim (ftype (function ,arg-types ,ret) ,name))))



(defmacro local-nicknames (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(loop for (package nickname) on body by #'cddr
             collect `

             #+sbcl (sb-ext:add-package-local-nickname ,nickname ,package )
             
             ;;(rename-package ,package ,package '(,nickname))


             )))


;; Remove all this out 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun amp-p (sym)
    "Detect an &arg"
    (and (symbolp sym) (eql #\& (char (symbol-name sym) 0)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun type-specifier-p (form)
    "Attempts to check whether FORM is a typespecifier. To ensure portability, 
     use (values [TYPESPECIFIER]*)"

    ;; Using this appears to work, but will log a style warning during compilation
    ;; #+sbcl (sb-ext:valid-type-specifier-p form)

    ;; TODO - clean up this logic
    ;; Technically we need to also make sure that body exists to avoid a case like
    ;;   (defunx fun ()
    ;;      (values) <- deduced as type
    ;;      (do-something))
    (if (and (listp form) (symbolp (car form)))
        (or (and (string-equal "values" (symbol-name (car form)))
                 (cadr form)
                 (symbolp (cadr form))
                 #+sbcl (sb-ext:defined-type-name-p (cadr form))
                 #-sbcl nil)
            #+sbcl (sb-ext:defined-type-name-p (car form))
            #+openmcl (ccl:type-specifier-p form)
            #+ecl (c::valid-type-specifier form)
            #+clisp (null (nth-value 1 (ignore-errors (ext:type-expand form)))))

        ;; Otherwise we are not a list
        (and (symbolp form)
             #+sbcl (sb-ext:defined-type-name-p form)
             #+openmcl (ccl:type-specifier-p form)
             #+ecl (c::valid-type-specifier form)
             #+clisp (null (nth-value 1 (ignore-errors (ext:type-expand form))))))))


(defmacro defunx (name arglist &body body)
  "A macro that can be used to define a function and it's associated ftype
   Examples: 
     Declare a function that adds two fixnums:
       (defun myadd ((a fixnum) (b fixnum)) fixnum 
         (+ a b)

     Additionally the macro supports agreggating types as with declare: 
       (defun myadd ((a b fixnum)) fixnum 
         (+ a b)

     Any valid type specifier works, so we can also declare multiple return values, i.e. 
       (defun my-multivalue-add ((a b fixnum)) (values fixnum fixnum)
         (values (+ a b) a b)"
  (let* ((f-args nil)
         (ftype-args nil)
         (ret-type (and (type-specifier-p (car body)) (car body))))
    (loop for form in arglist
          ;; If we have a typename, then the argument is a list of (VARS TYPE)
          if (listp form) do
            (let ((type (car (last form)))
                  (vars (butlast form)))
              (dolist (var vars)
                (push type ftype-args)
                (push var f-args)))
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



