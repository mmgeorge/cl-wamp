
;; Setup quicklisp
(format t "Installing quicklisp package manager")
(load "quicklisp.lisp")
(setf *quickload-prompt* nil)
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(format t "Sucessfully installed quicklisp")

