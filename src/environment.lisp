(in-package :clsh)

(defclass environment ()
  ((variables :accessor variables-of
              :initarg :variables
              :initform (clap-os::*environ*))
   (aliases :accessor aliases-of
            :initarg :aliases
            :initform (clap:dict)))
  )

(defun make-top-environment ()
  (make-instance 'environment :variables (clap-os:*environ*)))

(defmethod derive-child-environment ((env environment))
  )

(defvar *env* (make-top-environment))
