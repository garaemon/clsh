(in-package :clsh)

(defvar *token-identifiers*
  '(:word                               ;will have word expansion
    :assignment_word :newline :token :name :io_number))

(defclass token ()
  ((string
    :initarg :string
    :initform nil
    :accessor string-of
    :documentation "the original string of the token")
   (identifier
    :initarg :identifier
    :initform nil
    :accessor identifier-of
    :documentation "the identifier of the token. it should be one of
*token-identifiers*")))

(defmethod print-object ((object token) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "token: ~A [~s]"
            (identifier-of object) (string-of object))))

;; NB:
(defmethod make-token ((string string) &optional (hint nil))
  "make a token from string"
  (case hint
    (:operator
     (debug-format :verbose "operator: make a token from ~s with ~A hint"
                   string hint)
     ;; find operator suitable for string
     (let ((op (find string *operators* :key #'string-of :test #'string=)))
       (if op op
           (make-instance 'token :string string :identifier :word))))
    (:io_number
     (debug-format :verbose "io_number: make a token from ~s with ~A hint"
                   string hint)
     (make-instance 'token :string string :identifier :io_number))
    (:newline
     (debug-format :verbose "newline: make a newline token from ~s"
                   string)
     (make-instance 'token :string string :identifier :newline))
    (t                                  ;no hint
     (debug-format :verbose "no hint processing ~s" string)
     ;; NB: 
     (let ((reserved-wordp (find string *reserved-words* :key #'string-of)))
       (if reserved-wordp
           reserved-wordp
           (make-instance 'token :string string :identifier :word))))
     ))

(defmethod make-token ((stream stream) &optional (hint nil))
  "make a token from string output stream"
  (make-token (get-output-stream-string stream) hint))

(defvar *token-context* '(:case :if))

(defmethod update-context ((token token) context)
  (debug-format :warning "update-context currently is not supported")
  nil)

(defmethod newline-token-p ((token token))
  (eq (identifier-of token) :newline))
