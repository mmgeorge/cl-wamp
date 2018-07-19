(defpackage :wamp/session
  (:use :cl :wamp/transport)
  (:import-from :lparallel #:future #:force #:delay #:fulfill #:chain)
  (:import-from :blackbird #:attach #:catcher)
  (:import-from :wamp/transport)
  (:import-from :wamp/message-type)
  (:import-from :wamp/util #:with-timed-promise)
  
  (:export #:session #:session-t
           #:session-id
           #:make-session #:session-open
           #:timeout-exceeded))

(in-package :wamp/session)

(deftype session-t () '(values session &optional))


(defstruct registration
  (id 0 :type integer :read-only t))


(defclass empty-options () ())


(defclass session ()
  ((transport :reader session-transport :initarg :transport :type transport)
   (realm :reader session-realm :initarg :realm :type string)
   (awaiting :reader session-awaiting-promises :initform (make-hash-table) :type 'hash-table)
   (id :accessor session-id :initform 0 :type 'fixnum)
   (timeout :reader session-timeout :initform 2 :type 'fixnum)))


(defparameter %empty-options (make-instance 'empty-options))
(defparameter %supported-features
  `(( caller . ,%empty-options )))


(defun make-session (transport realm)
  (declare (transport transport) (string realm))
  (the session-t
       (let ((session (make-instance 'session :transport transport :realm realm)))
         (setf (transport-on-message transport)
               (lambda (type args)
                 (-session-handle-message session type args)))
         session)))


(defun session-open (self)
  (declare (session self))
  (catcher (-session-handshake self :roles %supported-features)
           (t (e) (format t "Encountered an error while opening session: ~a~%" e))))


(defun session-register (self uri procedure &key match)
  (declare (string uri) (function procedure) ((member 'prefix 'wildcard) match))
  (the session-t
       (let ((option (-make-options (pairlis '(match) (list match)))))
         (-session-send-message self 'register (-make-message-id) option uri))))


;; ++  Internal ++


(defun -session-handshake (self &key (roles (error "Roles must be specified")))
  (declare (session self) (list roles))
  (attach (-session-send-await self 'mtype:hello 'mtype:welcome
                               (session-realm self)
                               (-make-options (pairlis '(roles) (list roles))))
          (lambda (message)
            (setf (session-id self) (car message)))))


(defun -session-send-message (self type &rest args)
  (declare (session self) (mtype:message-t type) (list args))
  (the session-t
       (progn (transport-send (session-transport self) (cons type args))
              self)))


(defun -make-options (assoc)
  "Encodes an association list as an assoc (removing nulls) or empty object."
  (declare (list assoc))
  (the list
       (if (null assoc) %empty-options
           (remove-if-not #'cdr assoc))))


(defun -session-send-await (self type await-type &rest args)
  "Send a message of a given TYPE with ARGS, awaiting a message of AWAIT-TYPE. 
   Returns a promise gg0yielding resulting message" 
  (declare (session self) (mtype:message-t type await-type) (list args))
  (with-timed-promise 1 (resolve reject :resolve-fn resolver)
    ;; Add the promise to the awaiting map. Resolved if a match
    ;; is found in session-handle-message
    (setf (gethash (mtype:message-t-to-code await-type)
                   (session-awaiting-promises self))
          resolver)
    (transport-send (session-transport self) (cons type args))))
    

(defun -session-handle-message (self type args)
  (declare (session self) (mtype:message-t type) (list args))
  (case type
    (mtype:welcome (-session-resolve-awaiting self (mtype:message-t-to-code type) args))
    (t (error "Encountered unknown message type"))))


(defun -session-resolve-awaiting (self hash args)
  (declare (session self) (integer hash) (list args))
  (let ((awaiting-promise (gethash hash (session-awaiting-promises self))))
    (when (null awaiting-promise)
      (error "Corresponding promise does not exist for message"))
    (funcall awaiting-promise args)
    (remhash hash (session-awaiting-promises self))))


(defvar *-id-counter* 0)


(defun -make-message-id ()
  (the fixnum
       (incf *-id-counter*)))
