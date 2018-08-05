
;; Setup quicklisp
(load "quicklisp.lisp")
(quicklisp-quickstart:install)
(ql-util:without-prompting
  (ql:add-to-init-file))

;; Add current directory
(push (sb-posix:getcwd) asdf:*central-registry*)


