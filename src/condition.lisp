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

(define-condition clsh-bug (simple-error)
  ()
  (:report
   (lambda (c s)
     (format s "this might be a bug of clsh. please report it")))
  (:documentation
   "this is a condition signaled when unconsidered case"))

(define-condition syntax-error (simple-error)
  ((err-str :reader syntax-error-err-str
            :initarg :err-str))
  (:report
   (lambda (c s)
     (format s "syntax error: ~A" (syntax-error-err-str c))))
  (:documentation
   "this is a condition signaled when syntax error is detected"))
