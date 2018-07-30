(defpackage :wamp/session
  (:use :cl)
  (:import-from :blackbird #:attach #:catcher #:promise #:wait #:chain)
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

(defconstant %DEBUG_PRINT% t)

(defmacro debug-print (control &rest args)
  (when %DEBUG_PRINT%
    `(progn
       (format t "wamp/session:")
       (funcall #'format ,@(cons t (cons control args)))
       (format t "~%"))))


(deftype match-t () '(member prefix wildcard))

(defclass empty-options () ())

(defparameter %empty-options (make-instance 'empty-options))

(defparameter %supported-features
  `(( caller . ,%empty-options )))


(defstruct registration
  "A struct that stores registration info for a procedure"
  (uri nil :type string)
  (id 0 :type integer) ;; The id returned by the router
  (procedure nil :type function))


(defclass session ()
  ((transport :accessor transport :initarg :transport :initarg nil :type transport)
   (url :reader url :initarg :url :type string)
   (realm :reader realm :initarg :realm :type string)
   (id :accessor id :initform 0 :type 'fixnum)
   (awaiting :reader awaiting-promises :initform (make-hash-table) :type 'hash-table)
   (registered :reader registered :initform (make-hash-table :test #'equal) :type 'hash-table)
   (timeout :reader timeout :initform 30 :type 'fixnum)))


;; ++ Lifecycle ++

#[(dtype (string string) session)]
(defun make-session (url realm)
  "Makes a new session instance for the given URL and REALM. A session represents the 
   communication channel between a wamp client and router. While WAMP technically allows 
   for multiple transportation types only websockets are currently supported"
  (let* ((session (make-instance 'session :url url :realm realm)))
    (-connect-transport session)
    session))


;; CODE should always be a fixnum, but the underlying websocket lib we are using returns
;; null instead of 1005 when no code is present 
#[(dtype (session &key (:code (or null fixnum)) (:reason t)) *)]
(defun -handle-close (self &key code reason)
  (declare (ignore reason))
  (when (and code (not (eq code 1000))) ;; Can't use = here because code can be NIL
    (setf (id self) 0)
    ;; If abnormal closure, try to reconnect
    (-reconnect self)))
  

#[(dtype (session) t)]
(defun -connect-transport (self)
  (let* ((on-message (lambda (type args) (-handle-message self type args)))
         (on-close (lambda (&key code reason) (-handle-close self :code code :reason reason)))
         (transport (make-websocket (url self) :on-message on-message :on-close on-close)))
    (setf (transport self) transport)))


#[(dtype (session) t)]
(defun -reconnect (self)
 (start self))


#[(dtype (session) promise)]
(defun start (self)
  "Opens the session SELF. Returns a promise that resolves when the socket has opened
   and the basic handshake has been established"
  (-connect-transport self)
  (transport:start (transport self))
  (catcher 
   (-handshake self :roles %supported-features)
   (t (e) (format t "Encountered an error while starting session: ~a~%" e))))


#[(dtype (session) promise)]
(defun stop (self)
  "Stops the session SELF. Returns a promise that resolves when the connection has been closed
   and the corresponding GOODBYE message has been recieved"
  (catcher 
   (wait (-send-await self 'mtype:goodbye (list %empty-options "wamp.close.system_shutdown"))
     (transport:stop (transport self)))
   (t (e) (format t "Encountered an error while shutting down session session: ~a~%" e))))


;; ++ WAMP Procedures ++

#[(dtype (session string function &key (:match match-t)) (promise-of registration))]
(defun register (self uri procedure &key match)
  "Register a the given PROCEDURE withthe given URI. Returns a promise with the 
   registration info when registratoin has been sucessfull completed"
  (let ((options (-make-options (pairlis '(match) (list match))))
        (id (-create-message-id))
        (registration (-register-function self uri procedure)))
    (chain (-send-await self 'mtype:register (list id options uri))
      (:attach (message)
               (format t "adding registration ~a" (cadr message))
               (setf (registration-id registration) (cadr message))
               registration)
      (:catcher (e)
                (format t "Encountered error while registering session: ~a~%" e)
                (-unregister-function self uri)))))


;; ++  Internal ++

#[(dtype (session &key (:roles list)) promise)]
(defun -handshake (self &key (roles (error "Roles must be specified")))
  "Initiate a handshake with the router. Returns a promise that resolves
   when the handshake has finished"
  (attach (-send-await self 'mtype:hello (list (realm self) (-make-options (pairlis '(roles) (list roles)))))
          (lambda (message)
            (setf (id self) (car message)))))


#[(dtype (session string function) registration)]
(defun -register-function (self uri procedure)
  (let ((entry (gethash uri (registered self))))
    (when (not entry)
      (error "Unable to register ~a as it is already registered!~%" uri))
    (setf (gethash uri (registered self)) (make-registration :uri uri :procedure procedure))))


#[(dtype (session string) t)]
(defun -unregister-function (self uri)
  (let ((did-remove (remhash uri (registered self))))
    (when (not did-remove)
      (error "Unable to unregister ~a as no such procedure exists!~%" uri))
    t))



#[(dtype (list) t)]
(defun -make-options (assoc)
  "Encodes an association list as an assoc (removing nulls) or empty object."
  (if (and (listp assoc) (some #'(lambda (pair) (cdr pair)) assoc))
      (remove-if-not #'cdr assoc)
      %empty-options))


#[(dtype (session mtype:message-t list) null)]
(defun -send (self type args)
  "Sends a message of TYPE with ARGS"
  (transport:send (transport self) (cons type args))
  nil)


#[(dtype (session mtype:message-t list) promise)]
(defun -send-await (self type args)
  "Send a message of a given TYPE with ARGS, awaiting a message of AWAIT-TYPE. 
   Returns a promise yielding the resulting message" 
  (with-timed-promise (timeout self) (resolve reject :resolve-fn resolver :reject-fn rejector)
    ;; Add the promise to the awaiting map. Resolved if a match
    ;; is found in session-handle-message
    (setf (gethash (-lookup-response-hash type args)
                   (awaiting-promises self))
          (list resolver rejector))
    (transport:send (transport self) (cons type args))))
    

#[(dtype (mtype:message-t list) fixnum)]
(defun -lookup-response-hash (type args)
  "Create a hash for the designated response of the given MESSAGE. This hash code
   is used to map replies from the websocket router to outstanding promises. 
   NIL is returned when no response is expected"
  (case type
    (mtype:hello (mtype:to-num 'mtype:welcome))
    (mtype:goodbye (mtype:to-num 'mtype:goodbye))
    (mtype:publish (sxhash (cons (mtype:to-num 'mtype:published) (car args))))
    (mtype:subscribe (sxhash (cons (mtype:to-num 'mtype:subscribed) (car args))))
    (mtype:unsubscribe (sxhash (cons (mtype:to-num 'mtype:unsubscribed) (car args))))
    (mtype:call (sxhash (cons (mtype:to-num 'mtype:result) (car args))))
    (mtype:register (sxhash (cons (mtype:to-num 'mtype:registered) (car args))))
    (mtype:unregister (sxhash (cons (mtype:to-num 'mtype:unregistered) (car args))))
    (mtype:invocation (sxhash (cons (mtype:to-num 'mtype:yield) (car args))))
    (t (error "~a does not expect a response~%" type))))


#[(dtype (mtype:message-t) *)]
(defun -response-with-id-p (type)
  "Check whether TYPE is a response type with an id"
  (member type '(mtype:published
                 mtype:subscribed
                 mtype:unsubscribed
                 mtype:result
                 mtype:registered
                 mtype:unregistered
                 mtype:yield)))


#[(dtype (mtype:message-t) *)]
(defun -response-without-id-p (type)
  "Check whether TYPE is a response type without an id"
  (member type '(mtype:welcome mtype:goodbye)))


#[(dtype (session mtype:message-t list) *)]
(defun -handle-message (self type args)
  (debug-print "handling message: type: ~a args: ~a~%" type args)
  (cond ((-response-without-id-p type) (-resolve-awaiting self (mtype:to-num type) args))
        ((-response-with-id-p type) (-resolve-awaiting self (sxhash (cons (mtype:to-num type) (car args))) args))
        ((eq 'mtype:error type) (-reject-awaiting self args))
        (t (error "Unable to handle unknown message type ~a~%" type))))


#[(dtype (session integer list) null)]
(defun -resolve-awaiting (self hash args)
  (let ((awaiting-promise (gethash hash (awaiting-promises self))))
    (when (null awaiting-promise)
      (error "Recieved a message for which a corresponding resolver does not exist~%"))
    (funcall (car awaiting-promise) args)
    (remhash hash (awaiting-promises self))
    nil))


#[(dtype (session list) null)]
(defun -reject-awaiting (self args)
  (let* ((rejected-type (car args))
         (hash (-lookup-response-hash (mtype:code-to-message-t rejected-type) (cdr args)))
         (awaiting-promise (gethash hash (awaiting-promises self))))
    (when (null awaiting-promise)
      (error "Recieved a message for which a corresponding rejector does not exist~%"))
    (funcall (cadr awaiting-promise) args)
    (remhash hash (awaiting-promises self))
    nil))


(defvar *-id-counter* 0)



;(defun tfun (a b) (+ a b))


(defvar *session*)
(defun test ()
  (setf *session* (make-session "ws://138.68.246.180:8080/ws" "realm1"))
  (attach (start *session*)
          (register *session* "tfun2" #'(lambda (x y) (+ x y)))))


#[(dtype () fixnum)]
(defun -create-message-id ()
  (incf *-id-counter*))
