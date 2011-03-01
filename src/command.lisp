(in-package :clsh)

(defmethod execute-tokens ((env environment) tokens)
  (let ((command-token (car tokens))
        (command-args (cdr tokens)))
    (debug-format :verbose "command-token: ~A" command-token)
    (debug-format :verbose "command-args: ~A" command-args)
    (let ((command (resolve-command env command-token)))
      (debug-format :verbose "command: ~A" command)
      )
    ))

;; class definition
(defclass command ()
  ((name :accessor name-of
         :initarg :name)))

(defclass builtin-command (command)
  ((func :initarg :func
         :accessor func-of)))

(defclass executable-file-command (command)
  ())

(defclass shell-function (command)
  ())

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
         t                      ;d
         ))))
