(defpackage :wamp/session
  (:use :cl)
  (:import-from :lparallel #:future #:force #:delay #:fulfill #:chain)
  (:import-from :blackbird #:attach #:catcher #:promise #:wait)
  (:import-from :wamp/transport/transport #:transport)
  (:import-from :wamp/transport/websocket #:make-websocket)
  (:import-from :wamp/message-type)
  (:import-from :wamp/util #:with-timed-promise #:promise-of #:defunx)
  (:import-from :wamp/decorators #:dtype)
  (:export #:session #:make-session
           #:id
           #:start
           #:timeout-exceeded))

(in-package :wamp/session)

(deftype match-t () '(member prefix wildcard))

(defstruct registration
  (id 0 :type integer :read-only t))


(defclass empty-options () ())

(defparameter %empty-options (make-instance 'empty-options))

(defparameter %supported-features
  `(( caller . ,%empty-options )))


(defclass session ()
  ((transport :reader transport :initarg :transport :type transport)
   (realm :reader realm :initarg :realm :type string)
   (awaiting :reader awaiting-promises :initform (make-hash-table) :type 'hash-table)
   (id :accessor id :initform 0 :type 'fixnum)
   (timeout :reader timeout :initform 2 :type 'fixnum)))

;; ++ Lifecycle ++

#[(dtype (string string) session)]
(defun make-session (url realm)
  "Makes a new session instance for the given URL and REALM. A session represents the 
   communication channel between a wamp client and router. While WAMP technically allows 
   for multiple transportation types only websockets are currently supported"
  (let* ((transport (make-websocket url))
         (session (make-instance 'session :transport transport :realm realm))
         (on-message (lambda (type args) (-handle-message session type args))))
    (setf (transport:on-message transport) on-message)
    session))


#[(dtype (session) promise)]
(defun start (self)
  "Opens the session SELF. Returns a promise that resolves when the socket has opened
   and the basic handshake has been established"
  (catcher 
   (wait (transport:start (transport self))
     (-handshake self :roles %supported-features))
   (t (e) (format t "Encountered an error while starting session: ~a~%" e))))


#[(dtype (session &key (:roles list)) promise)]
(defun -handshake (self &key (roles (error "Roles must be specified")))
  "Initiate a handshake with the router. Returns a promise that resolves
   when the handshake has finished"
  (attach (-send-await self 'mtype:hello (-make-options (pairlis '(roles) (list roles))))
          (lambda (message)
            (setf (id self) (car message)))))


#[(dtype (session) promise)]
(defun stop (self)
  "Stops the session SELF. Returns a promise that resolves when the connection has been closed
   and the corresponding GOODBYE message has been recieved"
  (catcher 
   (wait (-send-await self 'mtype:goodbye %empty-options "wamp.close.system_shutdown")
     (transport:stop (transport self)))
   (t (e) (format t "Encountered an error while shutting down session session: ~a~%" e))))


;; ++ WAMP Procedures ++

#[(dtype (session string function &key (:match match-t)) (promise-of list))]
(defun register (self uri procedure &key match)
  "Register a given procedure with the match aguments
   and does something"
  (declare (ignore procedure))
  (let ((option (-make-options (pairlis '(match) (list match))))
        (id (-create-message-id)))
    (-send-await self 'mtype:register (list id option uri))))


;; ++  Internal ++


#[(dtype (list) list)]
(defun -make-options (assoc)
  "Encodes an association list as an assoc (removing nulls) or empty object."
  (if (null assoc) %empty-options
      (remove-if-not #'cdr assoc)))


#[(dtype (session mtype:message-t list) null)]
(defun -send (self type args)
  "Sends a message of TYPE with ARGS"
  (transport:send (transport self) (cons type args))
  nil)


#[(dtype (session mtype:message-t &rest t) promise)]
(defun -send-await (self type &rest args)
  "Send a message of a given TYPE with ARGS, awaiting a message of AWAIT-TYPE. 
   Returns a promise yielding the resulting message" 
  (with-timed-promise (timeout self) (resolve reject :resolve-fn resolver)
    ;; Add the promise to the awaiting map. Resolved if a match
    ;; is found in session-handle-message
    (setf (gethash (-lookup-response-hash type args)
                   (awaiting-promises self))
          resolver)
    (transport:send (transport self) (cons type args))))
    

#[(dtype (mtype:message-t &rest t) fixnum)]
(defun -lookup-response-hash (type &rest args)
  "Create a hash for the designated response of the given MESSAGE. This hash code
   is used to map replies from the websocket router to outstanding promises. 
   NIL is returned when no response is expected"
  (case type
    (mtype:hello (mtype:to-num 'mtype:welcome))
    (mtype:goodbye (mtype:to-num 'mtype:goodbye))
    (mtype:published (sxhash (cons (mtype:to-num 'mtype:published) (cadr args))))
    (mtype:subscribe (sxhash (cons (mtype:to-num 'mtype:subscribed) (cadr args))))
    (mtype:unsubscribe (sxhash (cons (mtype:to-num 'mtype:unsubscribed) (cadr args))))
    (mtype:call (sxhash (cons (mtype:to-num 'mtype:result) (cadr args))))
    (mtype:register (sxhash (cons (mtype:to-num 'mtype:registered) (cadr args))))
    (mtype:unregister (sxhash (cons (mtype:to-num 'mtype:unregistered) (cadr args))))
    (mtype:invocation (sxhash (cons (mtype:to-num 'mtype:yield) (cadr args))))
    (t (error "~a does not expect a response~%" type))))


#[(dtype (session mtype:message-t list) null)]
(defun -handle-message (self type args)
  (case type
    (mtype:welcome (-resolve-awaiting self (mtype:message-t-to-code type) args))
    (t (error "Encountered unknown message type")))
  nil)


#[(dtype (session integer list) null)]
(defun -resolve-awaiting (self hash args)
  (let ((awaiting-promise (gethash hash (awaiting-promises self))))
    (when (null awaiting-promise)
      (error "Recieved a message for which a corresponding resovler does not exist"))
    (funcall awaiting-promise args)
    (remhash hash (awaiting-promises self))
    nil))


(defvar *-id-counter* 0)


#[(dtype () fixnum)]
(defun -create-message-id ()
  (incf *-id-counter*))
