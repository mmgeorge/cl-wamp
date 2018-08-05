
(defvar *ql-path* (merge-pathnames (user-homedir-pathname) "quicklisp"))
(format t "QL-PATH ~a" *ql-path*)

(if (not (probe-file *ql-path*))
    ;; Install quicklisp if it does not exist
    (progn
      (load "quicklisp.lisp")
      (quicklisp-quickstart:install))
    ;; Otherwise load the setup file
    (load (merge-pathnames *ql-path* "setup.lisp")))

(ql-util:without-prompting
  (ql:add-to-init-file))
