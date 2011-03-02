(in-package :clsh)

(defvar *builtin-commands* nil)

(defmacro define-builtin-command (name func)
  (let ((obj (gensym)))
    `(if (not (find ,name *builtin-commands* :key #'name-of :test #'string=))
         (let ((,obj (make-instance 'builtin-command :func ,func :name ,name)))
           (push ,obj *builtin-commands*)
           ,obj))))

(defun echo-command (args)
  ;; return value...?
  (format t (clap:join " " args)))
  
(define-builtin-command "echo" #'echo-command)
;;(define-builtin-command "cd" #'cd-command)
