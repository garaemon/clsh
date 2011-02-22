(in-package :clsh)

(defvar *token-identifiers*
  '(:newline :io_here :io_number :token :word :name :assignment))

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

(defun skip-comment (stream)
  
  )

(defun read-token (stream)
  "return a token instance.

this is an implementation of below:
http://pubs.opengroup.org/onlinepubs/007908799/xcu/chap2.html#tag_001_003"
  (let ((ch nil))
    (while t
      (setq ch (read-char stream nil :eof))
      (cond
        ((eq ch :eof)
         )
        ((standard-char-p ch)           ;ok?
         (case ch
           )
         )
        (t                              ;might be error
         )))))

