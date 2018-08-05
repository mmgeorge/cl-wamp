
;; Setup quicklisp
(format t "Installing quicklisp package manager")
(load "quicklisp.lisp")
(setf *quickload-prompt* nil)
(setf ql-util:*do-not-prompt* t)
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(format t "Sucessfully installed quicklisp")

