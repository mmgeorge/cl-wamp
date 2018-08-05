
;; Setup quicklisp
(format t "Installing quicklisp package manager")
(load "quicklisp.lisp")
(quicklisp-quickstart:install)
(ql-util:without-prompting
  (ql:add-to-init-file))
(format t "Sucessfully installed quicklisp")

