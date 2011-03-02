(in-package :clsh)

(defmethod execute-tokens ((env environment) tokens)
  (let ((command-token (car tokens))
        (command-args (cdr tokens)))
    (debug-format :verbose "command-token: ~A" command-token)
    (debug-format :verbose "command-args: ~A" command-args)
    (let ((command (resolve-command env command-token)))
      (debug-format :verbose "command: ~A" command)
      (exec-command command (mapcar #'string-of command-args)))
    ))

;; class definition
(defclass command ()
  ((name :accessor name-of
         :initarg :name)))

(defmethod exec-command ((command command) args)
  "virtual method"
  (error 'not-implemented))

(defclass builtin-command (command)
  ((func :initarg :func
         :accessor func-of)))

(defmethod exec-command ((command builtin-command) args)
  (funcall (func-of command) args))

(defclass utility-command (command)
  ((func :initarg :func
         :accessor func-of)))

(defclass executable-file-command (command)
  ())

(defclass shell-function (command)
  ())

(defmethod print-object ((object command) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "[~A]" (name-of object))))

(defmethod resolve-command ((env environment) token)
  (let ((command-str (string-of token)))
    ;; check command-str has a backslash or not
    (if (clap:find command-str "/")
        (or
         (find command-str *builtin-commands* :key #'name-of
               :test #'string=) ;a
         (find command-str (shell-functions-of env) :key #'name-of
               :test #'string=) ;b
         (find command-str *utility-commands* :key #'name-of
               :test #'string=) ;c
         (make-instance 'executable-file-command :name command-str)) ;d
        (make-instance 'executable-file-command :name command-str))))
