
;; Setup quicklisp
(load "quicklisp.lisp")
(quicklisp-quickstart:install)
(ql-util:without-prompting
  (ql:add-to-init-file))

;; Add current directory
(format t "Adding current directory to asdf ~a" (sb-posix:getcwd))
(push (sb-posix:getcwd) asdf:*central-registry*)

(format t "Adding current directory to asdf ~a" asdf:*central-registry*)


