(in-package :clsh)

(defvar *token-identifiers*
  '(:word :assignment_word :newline :token :name :io_number))

(defclass token ()
  ((string
    :initarg :string
    :accessor string-of
    :documentation "the original string of the token")
   (identifier
    :initarg :identifier
    :accessor identifier-of
    :documentation "the identifier of the token. it should be one of
*token-identifiers*")))

(defmethod print-object ((object token) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "token: ~A [~A]"
            (identifier-of object) (string-of object))))

(defmethod make-token ((string string))
  "make a token from string"
  (make-instance 'token :string string))

(defmethod make-token ((stream stream))
  "make a token from string output stream"
  (make-token (get-output-stream-string stream)))
