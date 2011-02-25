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

(define-condition unknown-user (simple-error)
  ((user :reader unknown-user-user
         :initarg :user))
  (:report
   (lambda (c s)
     (format s "unknown user: ~A" (unknown-user-user c))))
  (:documentation
   "this a condition signaled when unknown user name is specified in
tilda expansion"))

(define-condition not-implemented (simple-error)
  ()
  (:report
   (lambda (c s)
     (format s "now implemented yet")))
  (:documentation
   "this is a condition signaled when a user use the not implemented functions"))

(define-condition bad-substitution (simple-error)
  ((string :reader bad-substitution-string
           :initarg :string))
  (:report
   (lambda (c s)
     (format s "~A: bad substitution" (bad-substitution-string c))))
  (:documentation
   "this is a condition signaled when a bad substitution is detected"))

(define-condition parameter-not-set (simple-error)
  ((word :initarg :word
         :accessor parameter-not-set-word
         :initform "parameter null or not set")
   (parameter :initarg :parameter
              :accessor parameter-not-set-parameter))
  (:report
   (lambda (c s)
     (format s "~A: ~A"
             (parameter-not-set-parameter c)
             (parameter-not-set-word c))))
  (:documentation
   "this is a condition signaled when ${parameter:?[word]} "))

