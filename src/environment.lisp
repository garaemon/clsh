(in-package :clsh)

(defclass environment ()
  ((exported-variables :accessor exported-variables-of
                       :initarg :exported-variables-of
                       :initform nil)
   (variables :accessor variables-of
              :initarg :variables
              :initform (clap:dict))
   (aliases :accessor aliases-of
            :initarg :aliases
            :initform (clap:dict))))

(defun make-top-environment ()
  (make-instance 'environment :variables clap-os:*environ*
                 :exported-variables-of (clap:keys clap-os:*environ*)))

(defmethod derive-environment ((env environment))
  (error 'not-implemented))

(defvar *env* (make-top-environment))

(defmethod resolve-parameter ((env environment) var &key (unset-return-value ""))
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
