(defpackage :wamp/session
  (:use :cl)
  (:import-from :blackbird #:create-promise #:attach #:catcher #:promise #:wait #:chain #:all)
  (:import-from :wamp/transport/transport #:transport)
  (:import-from :wamp/transport/websocket #:make-websocket)
  (:import-from :wamp/message-type)
  (:import-from :wamp/util  #:with-timed-promise #:promise-of #:defunx)
  (:import-from :wamp/decorators #:dtype)
  (:export #:session #:make-session
           #:id
           #:start

           #:timeout-exceeded))

(in-package :wamp/session)


(deftype match-t () '(member "prefix" "wildcard" :test #'string-equal))
(deftype invoke-t () '(member "first" "last" "roundrobin" "random" :test #'string-equal))

(defclass empty-options () ())

(defparameter %empty-options (make-instance 'empty-options))

(defparameter %supported-features
  `(( caller . ,%empty-options )))


(defstruct registration
  "A struct that stores registration info for a procedure"
  (uri nil :type string :read-only t)
  (options nil :type list :read-only t)
  (id 0 :type integer) ;; The id returned by the router
  (procedure nil :type function))


(defstruct resolving-info
  "Helper struct that encapsulates a promise and its resolver"
  (promise nil :type promise)
  (resolver nil :type function))


(defun create-resolving-info ()
  (let* ((resolver-capture nil)
         (promise
           (create-promise
            (lambda (resolver rejecter)
              (declare (ignore rejecter))
              (setf resolver-capture resolver)))))
    (make-resolving-info :promise promise :resolver resolver-capture)))
  

(defclass session ()
  ((transport
    :documentation "Internal wrapper over the underlying transportation mechanism"
    :accessor transport :initarg :transport :initarg nil :type transport)
   (url
    :documentation "Url fo the target websocket server"
    :reader url :initarg :url :type string)
   (realm
    :documentation "Current realm the session is operating on"
    :reader realm :initarg :realm :type string)
   (id
    :documentation "Id of the session. Determined during session handshake"
    :accessor id :initform 0 :type 'fixnum)
   (awaiting
    :documentation "sxhash(mtype, ?id) -> promise. Used to resolve responses"
    :reader awaiting-promises :initform (make-hash-table) :type 'hash-table)
   (resolving-info
    :documentation "A promise that resolves when the session has been opened"
    :accessor resolving-info :initform (create-resolving-info) :type '(or resolving-info nil))
   (registrations
    :documentation "fname -> registration. On (re)connection all functions will be registered"
    :reader registrations :initform (make-hash-table :test #'equal) :type 'hash-table)
   (rpcs
    :documentation "id -> registration. Used to lookup the registration associated with a given id"
    :reader rpcs :initform (make-hash-table) :type 'hash-table)
   (timeout
    :documentation "Time the session should wait for a server response before timing out"
    :reader timeout :initform 30 :type 'fixnum)
   (log-serious-conditions
    :documentation "When T, propogate serious condition messages on invocation"
    :reader log-serious-conditions :initarg :log-serious-conditions :initform t :type 'boolean)
   (log-output
    :documentation "Configurable output stream for error logs"
    :accessor log-output :initarg :log-output :type 'stream )
   (log-verbose-p
    :documentation "Whether to log debugging information"
    :accessor log-verbose-p :initarg :log-verbose-p :type 'boolean)))


;; ++ Lifecycle ++

#[(dtype (string string &key (:log-output stream) (:log-serious-conditions boolean) (:log-verbose boolean)) session)]
(defun make-session (url realm
                     &key (log-output *error-output*) (log-serious-conditions t) (log-verbose nil))
  "Makes a new session instance for the given URL and REALM. A session represents the 
   communication channel between a wamp client and router. While WAMP technically allows 
   for multiple transportation types only websockets are currently supported"
  (make-instance 'session :url url :realm realm
                          :log-output log-output
                          :log-serious-conditions log-serious-conditions
                          :log-verbose-p log-verbose))


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
  (-log self "Lost connection to ~a. Attempting to reconnect" (realm self))
  (setf (resolving-info self) (create-resolving-info))
  (start self))


#[(dtype (session) promise)]
(defun start (self)
  "Opens the session SELF. Returns a promise that resolves when the socket has opened
   and the basic handshake has been established"
  (clrhash (rpcs self))
  (clrhash (awaiting-promises self))
  (-connect-transport self)
  (transport:start (transport self))
  (chain (-handshake self :roles %supported-features)
    (:attach ()
             (-log self "Started session on ~a" (realm self))
             (-dispatch-registrations self))
    (:attach ()
             (and (resolving-info self)
                  (funcall (resolving-info-resolver (resolving-info self))))
             (setf (resolving-info self) nil))
    (:catcher (e) (-log self "Encountered an error while starting session: ~a~%" e))))


#[(dtype (session) promise)]
(defun stop (self)
  "Stops the session SELF. Returns a promise that resolves when the connection has been closed
   and the corresponding GOODBYE message has been recieved"
  (catcher 
   (wait (-send-await self 'mtype:goodbye (list %empty-options "wamp.close.system_shutdown"))
     (-log self "Terminating session on ~a" (realm self))
     (transport:stop (transport self)))
   (t (e) (-log self "Encountered an error while shutting down session session: ~a~%" e))))


;; ++ WAMP Procedures ++

#[(dtype (session string function &key (:match match-t) (:invoke invoke-t)) (promise-of registration))]
(defun register (self uri procedure &key match invoke)
  "Register a the given PROCEDURE with the given URI. Returns a promise with the 
   registration info on completion

   :MATCH - Specify how to match the passed URI. Either 'prefix' or 'wildcard'
   :INVOKE - Allow multiple registrations. One of 'first', 'last', 'random' 'roundrobin'"
  (let* ((options (-make-options (pairlis '(match invoke) (list match invoke))))
         (registration (-register-function self uri procedure options)))
    (-log self "Registering function ~a" (registration-uri registration))
    (if (resolving-info self)
        ;; If the session is not yet started, we simply add the registration and return.
        (wait (resolving-info-promise (resolving-info self)) registration)
        ;; Otherwise, we create the registration and send it to the router
        (chain (-dispatch-registration self registration)
          (:catcher (e)
                    (-log self "Encountered error while registering session: ~a~%" e)
                    (remhash (registration-uri registration) (registrations self)))))))


#[(dtype (session registration) (promise-of null))]
(defun unregister (self registration)
  (if (resolving-info self)
      ;; Wait for any reconnection to complete
      (wait (resolving-info self)
        (-unregister-function self registration))
      (-unregister-function self registration)))


#[(dtype (session registration) (promise-of null))]
(defun -unregister-function (self registration)
  (remhash (registration-uri registration) (registrations self))
  (remhash (registration-id registration) (registrations self))
  (chain (-send-await self 'mtype:unregister (list (-create-message-id) (registration-id registration)))
    (:attach ()
             (-log self "Successfully unregistered function: ~a" (registration-uri registration))
             nil)
    (:catcher (e)
              (-log self "Encountered error while unregistering session: ~a~%" e))))
  


#[(dtype (session string &key (:args list) (:kwargs list)) (promise-of (values * *)))]
(defun call (self uri &key args kwargs)
  (let ((options %empty-options)
        (id (-create-message-id)))
    (attach (-send-await self 'mtype:call (remove nil (list id options uri args kwargs)))
            (lambda (result) 
               (destructuring-bind (request-id options &optional args kwargs) result
                 (declare (ignore request-id options))
                 (values args kwargs))))))
    


;; ++  Internal ++

#[(dtype (session &key (:roles list)) (promise-of session))]
(defun -handshake (self &key (roles (error "Roles must be specified")))
  "Initiate a handshake with the router. Returns a promise that resolves
   when the handshake has finished"
  (attach (-send-await self 'mtype:hello (list (realm self) (-make-options (pairlis '(roles) (list roles)))))
          (lambda (message)
            (setf (id self) (car message))
            self)))


#[(dtype (session string function list) registration)]
(defun -register-function (self uri procedure options)
  (if (gethash uri (registrations self))
      (error "Unable to register ~a as it is already registered!~%" uri)
      (setf (gethash uri (registrations self))
            (make-registration :uri uri :procedure procedure :options options))))


#[(dtype (session registration) (promise-of registration))]
(defun -dispatch-registration (self registration)
  (with-slots (options uri) registration
    (attach (-send-await self 'mtype:register (list (-create-message-id) options uri))
            (lambda (message)
              (setf (registration-id registration) (cadr message))
              (-log self "Registered ~a as id ~a" (registration-uri registration) (registration-id registration))
              (-add-rpc self registration)
              registration))))


#[(dtype (session) promise)]
(defun -dispatch-registrations (self)
  "Dispatch all registrations, returning a promise when all registrations have completed"
  (let ((awaiting (loop for registration being the hash-values of (registrations self)
                        collect (-dispatch-registration self registration))))
    (all awaiting)))


#[(dtype (session registration) t)]
(defun -add-rpc (self registration)
  (if (gethash (registration-id registration) (rpcs self))
       (error "Unable to set RPC for ~a as such an RPC entry already exists!~%" registration) 
      (setf (gethash (registration-id registration) (rpcs self)) registration)))


#[(dtype (session registration) t)]
(defun -remove-rpc (self registration)
  (let ((did-remove (remhash (registration-id registration) (rpcs self))))
    (when (not did-remove)
      (error "Unable to remove RPC ~a as no such RPC entry exists!~%" registration))
    t))


#[(dtype (list) t)]
(defun -make-options (assoc)
  "Encodes an association list as an assoc (removing nulls) or empty object."
  (if (and (listp assoc) (some #'(lambda (pair) (cdr pair)) assoc))
      (remove-if-not #'cdr assoc)
      nil))


#[(dtype (session mtype:message-t list) null)]
(defun -send (self type args)
  "Sends a message of TYPE with ARGS"
  (-log-verbose self "Debug: Sending message: type: ~a args: ~a" type args)
  (transport:send (transport self) (cons type args))
  nil)


#[(dtype (session mtype:message-t list) promise)]
(defun -send-await (self type args)
  "Send a message of a given TYPE with ARGS, awaiting a message of AWAIT-TYPE. 
   Returns a promise yielding the resulting message"
  (-log-verbose self "Debug: Sending message: type: ~a args: ~a" type args)
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
  (-log-verbose self "Debug: Handling message: type: ~a args: ~a" type args)
  (cond ((eq 'mtype:invocation type) (-handle-invokation self args))
        ((-response-without-id-p type) (-resolve-awaiting self (mtype:to-num type) args))
        ((-response-with-id-p type) (-resolve-awaiting self (sxhash (cons (mtype:to-num type) (car args))) args))
        ((eq 'mtype:error type) (-reject-awaiting self args))
        (t (-log self "Error: Unable to handle unknown message type ~a" type))))


#[(dtype (session integer list) null)]
(defun -resolve-awaiting (self hash args)
  (let ((awaiting-promise (gethash hash (awaiting-promises self))))
    (when (null awaiting-promise)
      (-log self "Error: Recieved a message for which a corresponding resolver does not exist")
      (return-from -resolve-awaiting))
    (funcall (car awaiting-promise) args)
    (remhash hash (awaiting-promises self))
    nil))


#[(dtype (session list) null)]
(defun -reject-awaiting (self args)
  (let* ((rejected-type (car args))
         (hash (-lookup-response-hash (mtype:code-to-message-t rejected-type) (cdr args)))
         (awaiting-promise (gethash hash (awaiting-promises self))))
    (when (null awaiting-promise)
        (-log self "Error: Recieved a message for which a corresponding rejector does not exist")
        (return-from -reject-awaiting))
    (funcall (cadr awaiting-promise) args)
    (remhash hash (awaiting-promises self))
    nil))


#[(dtype (session fixnum *) null)]
(defun -yield (self request-id result)
  (-send self 'mtype:yield (list request-id %empty-options result)))


#[(dtype (session list) *)]
(defun -call-local-rpc (self message)
  (destructuring-bind (request-id registration-id options &optional args kwargs) message
    (declare (ignore options))
    (let* ((registration (gethash registration-id (rpcs self))))
      (-yield self request-id
              (multiple-value-list (apply (registration-procedure registration) (append args kwargs)))))))


#[(dtype (session list) *)]
(defun -handle-invokation (self message)
  (handler-case (-call-local-rpc self message)
    ;; Likely indicates an error in how the function was called
    ((or type-error #+sbcl sb-int:simple-program-error) (e)
      (-log self "Error: A TYPE-ERROR error during function invocation: ~a" e)
      (-send self 'mtype:error (list (mtype:to-num 'mtype:invocation)
                                     (car message) %empty-options "wamp.error.invalid_argument" nil
                                     `((details . ,(format nil "~a" e))))))

    ;; A fatal error or serious condition has occured. Returning a detailed error message is configurable
    (serious-condition (e)
      (-log self "Error: A SERIOUS-CONDITION occured during function invocation: ~a" e)
      (let* ((base-args (list (mtype:to-num 'mtype:invocation)
                              (car message) %empty-options "cl.wamp.error.serious_condition"))
             (all-args (if (log-serious-conditions self)
                           (append base-args (list nil `((details . ,(format nil "~a" e)))))
                           base-args)))
        (-send self 'mtype:error all-args)))))


#[(dtype (session string &rest *) null)]
(defun -log (self control-string &rest args)
  (multiple-value-bind (second minute hour date month year) (get-decoded-time) 
    (apply #'format (log-output self)
           (concatenate 'string "~a:~a:~a ~a/~a/~a (UTC) cl-wamp: " control-string "~%")
           (append (list hour minute second date month year) args)))
  nil)


#[(dtype (session string &rest *) null)]
(defun -log-verbose (self control-string &rest args)
  (when (log-verbose-p self)
    (apply #'-log (append (list self control-string) args))))


(defvar *-id-counter* 0)



(defun tfun3 (a b)
  (+ a b))



(defvar *session*)

(defun test ()
  (setf *session* (make-session "ws://138.68.246.180:8080/ws" "realm1" :log-verbose t :log-serious-conditions t))
  (register *session* "com.app.tfun3" #'tfun3 :invoke "random")
  (start *session*))


#[(dtype () fixnum)]
(defun -create-message-id ()
  (incf *-id-counter*))
