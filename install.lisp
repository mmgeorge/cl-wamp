(load (merge-pathnames (user-homedir-pathname) "quicklisp.lisp"))
(load (merge-pathnames (merge-pathnames (user-homedir-pathname) "quicklisp") "setup.lisp")
      :if-does-not-exist nil)


(handler-case (quicklisp-quickstart:install)
  (t (e)
    (declare (ignore e))))

(with-open-file (stream (merge-pathnames (merge-pathnames (user-homedir-pathname) ".sbclrc"))
                        :direction :output :if-exists :append)
  (write `(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                                 (user-homedir-pathname))))
            (when (probe-file quicklisp-init)
              (load quicklisp-init)))
         :stream stream))
  


                                 )

  

;; (if (not (probe-file *ql-path*))
;;     ;; Install quicklisp if it does not exist
;;     (progn
      
;;       )
;;     ;; Otherwise load the setup file
;;     (load (merge-pathnames *ql-path* "setup.lisp")))

(ql-util:without-prompting
  (ql:add-to-init-file))
