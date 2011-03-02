(in-package :clsh)

(defclass environment ()
  ((exported-variables :accessor exported-variables-of
                       :initarg :exported-variables
                       :initform nil)
   (variables :accessor variables-of
              :initarg :variables
              :initform (clap:dict))
   (shell-functions :accessor shell-functions-of
                    :initarg :shell-functions
                    :initform nil)
   (aliases :accessor aliases-of
            :initarg :aliases
            :initform (clap:dict))))

(defun make-top-environment ()
  (make-instance 'environment
                 :variables (clap:copy (clap-os::table-of clap-os:*environ*))
                 :exported-variables (clap:keys clap-os:*environ*)))

(defmethod derive-environment ((env environment))
  (let ((exported (exported-variables-of env))
        (prev-vars (variables-of env)))
    (let ((variables (clap:dict
                      (mapcar #'(lambda (v)
                                  (cons v (clap:lookup prev-vars v)))
                              exported))))
      (make-instance 'environment :variables variables
                     :exported-variables exported))))

(defmethod derive-environment-with-assignments ((env environment) assignments)
  (debug-format :verbose "derive environment from ~A" assignments)
  (let ((new-env (derive-environment env)))
    (update-environment-with-assignments new-env assignments)))

(defmethod update-environment-with-assignment ((env environment) assignment)
  ;; here we split assignment
  (multiple-value-bind (var separator value)
      (clap:partition (string-of assignment) "=")
    (debug-format :debug "set variable ~A => ~A of ~A" var value env)
    (setf (clap:lookup (variables-of env) var) value)))

(defmethod update-environment-with-assignments ((env environment) assignments)
  (dolist (assign assignments)
    (update-environment-with-assignment env assign))
  env)

(defvar *env* (make-top-environment))

(defmethod resolve-parameter
    ((env environment) var &key (unset-return-value ""))
  (debug-format :verbose "lookup ~A" var)
  (let ((ret (clap:lookup (variables-of env) var)))
    (if ret ret unset-return-value)))

(defmethod set-parameter ((env environment) var value &key (exportp nil))
  (debug-format :verbose "set ~A <- ~A" var value)
  (setf (clap:lookup (variables-of env) var) value)
  (if exportp (export-parameter env var)) ;add to exported-variables
  value)

(defmethod export-parameter ((env environment) var)
  (pushnew var (exported-variables-of env) :test #'string=))

;; (yacc:parse-with-lexer (clsh::list-lexr (clsh::read-tokens (make-string-input-stream "echo $SHELL"))) clsh::*token-parser*)
