
(defvar *ql-path* (merge-pathnames (user-homedir-pathname) "quicklisp"))
(format t "QL-PATH ~a" *ql-path*)

(load "quicklisp.lisp")

(handler-case (quicklisp-quickstart:install)
  (t (e)
    (load (merge-pathnames *ql-path* "setup.lisp"))))



;; (if (not (probe-file *ql-path*))
;;     ;; Install quicklisp if it does not exist
;;     (progn
      
;;       )
;;     ;; Otherwise load the setup file
;;     (load (merge-pathnames *ql-path* "setup.lisp")))

(ql-util:without-prompting
  (ql:add-to-init-file))
