(in-package :clsh)

(define-condition unknown-debug-level (simple-error)
  ((level :reader unknown-debug-level-level
          :initarg :level))
  (:report
   (lambda (c s)
     (format s "unknown debug level: ~A"
             (unknown-debug-level-level c))))
   (:documentation
    "this is a condition signaled when unknown debug level is specified"))
