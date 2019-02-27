(defpackage :wamp/ws/session/websocket.test 
  (:use :cl :expect :alexandria :wamp/ws/session/websocket :wamp/ws/session/session)
  (:import-from :flexi-streams))

(in-package :wamp/ws/session/websocket.test)


(defixture f-websocket ()
  (let ((websocket (make-instance 'websocket :bufsize 15 :socket nil)))
    (setf (socket-stream websocket)
          (flex:make-in-memory-output-stream :element-type :utf-8))
    websocket))


(defun convert-socket-stream (session)
  (let ((sequence (flex:get-output-stream-sequence (socket-stream session))))
    (setf (socket-stream session)
          (flex:make-in-memory-input-stream sequence))))


(deftest-of send-text ((websocket f-websocket))
  "Send text message"
  (let ((message (flex:string-to-octets "hello world")))
    (send-text websocket message :start 0 :end (length message))
    (convert-socket-stream websocket)
    (destructuring-bind (opsym buffer end) (recieve websocket)
      (expect (eq opsym :text))
      (expect (eq end (length message)))
      (expect (string-equal (flex:octets-to-string buffer :end end) "hello world")))))


(deftest-of send-text ((websocket f-websocket))
  "Send text message - multiframe"
  (let ((message (flex:string-to-octets "hello world")))
    (send-text websocket message :start 0 :end (length message) :max-len 2)
    (convert-socket-stream websocket)
    (when-let ((recv (recieve websocket)))
      (destructuring-bind (opsym buffer end) recv
        (expect (eq opsym :text))
        (expect (eq end (length message)))
        (expect (string-equal (flex:octets-to-string buffer :end end) "hello world"))))))


(deftest-of send-binary ((websocket f-websocket))
  "Send binary message"
  (let ((message (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(97 98 99))))
    (send-binary websocket message :start 0 :end (length message))
    (convert-socket-stream websocket)
    (destructuring-bind (opsym buffer end) (recieve websocket)
      (expect (eq opsym :binary))
      (expect (eq end (length message)))
      (expect (equalp message (subseq buffer 0 end))))))

(deftest-of send-binary ((websocket f-websocket))
  "Send binary message - large"
  (let ((message
          (map 'vector #'random (make-array 100000 :element-type  '(unsigned-byte 8) :initial-element 255))))
    (send-binary websocket message :start 0 :end (length message))
    (convert-socket-stream websocket)
    (when-let ((recv (recieve websocket)))
      (destructuring-bind (opsym buffer end) recv
        (expect (eq opsym :binary))
        (expect (eq end (length message)))
        (expect (equalp message (subseq buffer 0 end)))))))


(deftest-of ping ((websocket f-websocket))
  "Send ping without message"
  (ping websocket nil)
  (convert-socket-stream websocket)
  (destructuring-bind (opsym buffer end) (recieve websocket)
    (expect (eq opsym :ping))
    (expect (eq end nil))
    (expect (eq buffer nil))))


(deftest-of ping ((websocket f-websocket))
  "Send ping with message"
  (let ((message #(97 98 99)))
    (ping websocket message)
    (convert-socket-stream websocket)
    (destructuring-bind (opsym buffer end) (recieve websocket)
      (expect (eq opsym :ping))
      (expect (eq end (length message)))
      (expect (equalp (subseq buffer 0 end) message)))))


(deftest-of pong ((websocket f-websocket))
  "Send pong with message"
  (let ((message #(97 98 99)))
    (pong websocket message :start 1 :end (length message))
    (convert-socket-stream websocket)
    (destructuring-bind (opsym buffer end) (recieve websocket)
      (expect (eq opsym :pong))
      (expect (eq end (- (length message) 1)))
      (expect (equalp (subseq buffer 0 end) (subseq message 1 (length message)))))))
