(defpackage :wamp/session
  (:use :cl :blackbird
        :wamp/transport)
  (:import-from :blackbird)
  (:import-from :wamp/transport)
  (:import-from :wamp/message-type)
  (:export #:session #:session-t
           #:session-id
           #:make-session #:session-open))

(in-package :wamp/session)


(deftype session-t () '(values session &optional))
(deftype promise-t () '(values promise &optional))

(defstruct registration
  (id 0 :type integer :read-only t))

(defclass empty-options () ())

(defclass session ()
  ((transport :reader session-transport :initarg :transport :type transport)
   (realm :reader session-realm :initarg :realm :type string)
   (awaiting :reader session-awaiting :initform (make-hash-table) :type 'hash-table)
   (id :accessor session-id :initform 0 :type 'integer)))


(defun make-session (transport realm)
  (declare (transport transport) (string realm))
  (the session-t
       (let ((session (make-instance 'session :transport transport :realm realm)))
         (setf (transport-on-message transport)
               (lambda (type args)
                 (-session-handle-message session type args)))
         session)))


(defparameter %empty-options (make-instance 'empty-options))
(defparameter %supported-features
  `(( caller . ,%empty-options )))

(defun session-open (self)
  (declare (session self))
  (the promise-t
       (-session-hello self :roles %supported-features)))


(defun session-register (self uri procedure &key match)
  (declare (string uri) (function procedure) ((member 'prefix 'wildcard) match))
  (the session-t
       (let ((option (-make-options (pairlis '(match) (list match)))))
         (-session-send-message self 'register (-make-message-id) option uri))))

;; ++ Internal ++

(defun -session-hello (self &key (roles (error "Roles must be specified")))
  (declare (session self) (list roles))
  (the promise-t
       (catcher 
        (attach (-session-send-await self 'mtype:hello 'mtype:welcome
                                     (session-realm self)
                                     (-make-options (pairlis '(roles) (list roles))))
                (lambda (response) (setf (session-id self) (car response))))
        (t (e) (format t "Unable to establish wamp session: ~a~%" e)))))



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
  "Send a message of a given TYPE with ARGS, awaiting a message of AWAIT-TYPE before resolving. 
   Create a promise that awaits a message of TYPE with ID"
  (declare (session self) (mtype:message-t type await-type) (list args))
  (the promise-t
       (create-promise
        (lambda (resolve reject)
          (declare (ignore reject))
         ;; Add the resolver for the promise to the awaiting map. Resolved if a match
          ;; is found in session-handle-message
          (setf (gethash (mtype:message-t-to-code await-type)
                         (session-awaiting self))
                resolve)
         ;; Reject if we exceed the timeout
          ;; (async:delay (lambda () (reject :time (session-timeout self))))
          (transport-send (session-transport self) (cons type args))
          ))))
  



(defun -session-handle-message (self type args)
  (declare (session self) (mtype:message-t type) (list args))
  (case type
    (mtype:welcome (-session-resolve-awaiting self (mtype:message-t-to-code type) args))
    (t (error "Encountered unknown message type"))))


(defun -session-resolve-awaiting (self hash args)
  (declare (session self) (integer hash) (list args))
  (let ((resolver (gethash hash (session-awaiting self))))
    (when (null resolver)
      (error "Corresponding resolver does not exist for message"))
    (funcall resolver args)
    (remhash hash (session-awaiting self))))


(defvar *-id-counter* 0)


(defun -make-message-id ()
  (the fixnum
       (incf *-id-counter*)))

;;(defvar *sess (make-session (transport-open (make-websocket "ws://services.owny.io:8080" :on-open (lambda () (format t "Opened!!")))) "realm1"))
