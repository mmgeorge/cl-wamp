(defpackage :wamp/decorators
  (:use :cl)
  (:export #:dtype))

(in-package :wamp/decorators)

;; #[ ... ] denotes a decorator list
(set-macro-character #\] (get-macro-character #\)))
(set-dispatch-macro-character #\# #\[ 'parse-decorators)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-decorator (prev expr)
    "Parse a decorator EXPR where PREV is the result of a previous decorator's 
     evaluation. That is, a tuple of (fbody declarations)"
    ;; Each decorator in our decorators list by either me an atom
    ;; (if it takes no args) or a list
    (macroexpand
     (if (atom expr) `(,expr ,prev)  ;; 1- #[export] 
         (let ((name (car expr))
               (args (cdr expr)))
           (if args 
               `(,name ,prev ,@args) ;; 2- #[(ftype (fixnum) fixnum)]
               `(,name ,prev)))))))  ;; 3- #[(export)] - as in (1) (maybe prohibit this?)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-decorators (stream char char2)
    "Parse a STREAM of decorators in the form #[decorator+] where each decorator expression
     is either a list (if it takes arguments) or an atom. Each decorator refers to a macro
     that returns a list of (fbody declarations), which allows each decorator to mutate the
     target expression in turn. A decorator may optionally append a declaration to the 
     declaration list which gets run after the mutated expression"
    (declare (ignore char char2))
    (let* ((decorators (read-delimited-list #\] stream t))
           (fbody (read stream))
           (chain (reduce #'parse-decorator decorators :initial-value (list fbody nil))))
      `(progn 
         ,(car chain)       ;; fbody, with accumulated modifications
         ,@(cadr chain))))) ;; list-of declarations


(defmacro define-decorator (name (args form) &body body)
  `(defmacro ,name (,args ,form)
     `(progn ,,@body ,,form)))


;; Built-in decorators


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun values-p (type-spec)
    (and (listp type-spec)
         (symbolp (car type-spec))
         (string-equal "values" (symbol-name (car type-spec))))))


(defmacro dtype ((fbody declarations) f-arg-types f-ret-type)
  (let* ((f-name (cadr fbody))
         (f-ret-values (if (values-p f-ret-type) f-ret-type
                           `(values ,f-ret-type &optional))))
    (list fbody
          (cons `(declaim (ftype (function ,f-arg-types ,f-ret-values) ,f-name)) declarations))))


(defmacro echo ((fbody dec))
  (list fbody (cons `(format t "Echoing Function Name: ~a" ,(symbol-name (cadr fbody))) dec)))
