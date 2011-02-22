(in-package :clsh)

;;utility
(defun chars= (ch &rest chars)
  (if (null chars)
      t
      (and (char= ch (car chars))
           (chars= ch (cdr chars)))))

(defclass token-parser ()
  ((token-stream :initform (make-string-output-stream)
                 :accessor token-stream-of
                 :documentation "a stream to store the characters for token")
   (initialized :initform nil
                :accessor initialized-of
                :documentation "INITIALIZED will be T if token-parser begins
to parse string.")
   (quoted :initform nil
           :accessor quoted-of
           :documentation "QUATED will be T if token-parser begins to parse
quoted string.")
   (quoting-char :initform nil
                 :accessor quoting-char-of
                 :documentation "QUOTING-CHAR is a place holder of the
character beginning quate")
   (operator-candidates :initform nil
                        :accessor operator-candidates-of
                        :documentation "OPERATOR-CANDIDATES is used when
parsing the operators. OPERATOR-CANDIDATES helds the candidates of the
operators to be.")
   (count :initform 0
          :accessor count-of
          :documentation "COUNT is a counter of the number of the characters
of the token.")
   (immediate-return-p :initform nil
                       :accessor immediate-return-p-of
                       :documentation "IMMEDIATE-RETURN-P may be T in ordder to
 convert token-parser to token."))
  (:documentation
   "this is a helper class for tokenization. token-parser is created to
parse a token and never be reused."))

(defmethod has-operator-candidates-p ((parser token-parser))
  (operator-candidates-of parser))

(defmethod quotedp ((parser token-parser))
  (quoted-of parser))

(defmethod initializedp ((parser token-parser))
  (initialized-of parser))

(defmethod immediate-return-p ((parser token-parser))
  (immediate-return-p-of parser))

(defmethod token-parser->token ((parser token-parser))
  (if (initializedp parser)
      (make-token (get-output-stream-string (token-stream-of parser)))
      nil))

;; NB:
(defmethod eof-process ((parser token-parser))
  (debug-format :verbose "eof is detected")
  (finish-parsing parser))

(defmethod finish-parsing ((parser token-parser))
  (debug-format :verbose "finish parsing")
  (setf (immediate-return-p-of parser) t))

(defmethod push-char ((parser token-parser) ch)
  (write-char ch (token-stream-of parser))
  (incf (count-of parser))
  (setf (initialized-of parser) t)
  ch)

(defmethod operator-process ((parser token-parser) ch stream)
  ;; 2, 3
  (debug-format :verbose "processing operator")
  (with-slots (operator-candidates count) parser
    (let ((next-operators (operator-successor ch operator-candidates count)))
      (if next-operators
          (progn                        ;2
            (debug-format :verbose "operator successing: ~A -> ~A"
                          operator-candidates next-operators)
            (setf operator-candidates next-operators)
            (push-char parser ch))
          (progn                        ;3
            (unread-char ch stream)
            (finish-parsing parser))))))

(defmethod quote-process ((parser token-parser) ch)
  (debug-format :verbose "begins quoting")
  (setf (quoted-of parser) t)
  (setf (quoting-char-of parser) ch))

(defmethod beginning-operator-process ((parser token-parser) ch stream)
  (debug-format :verbose "beigins operator")
  (if (initializedp parser)
      (progn
        (unread-char ch stream)
        (finish-parsing parser))
      (progn
        (setf (operator-candidates-of parser) (operator-startswith ch))
        (debug-format :verbose "ch: ~A" ch)
        (debug-format :verbose "candidates: ~A" (operator-candidates-of parser))
        (push-char parser ch))))

(defmethod newline-process ((parser token-parser))
  (debug-format :verbose "unquoted newline")
  (if (initializedp parser)
      (finish-parsing parser))
  ;; not initialized, dispose the character read and skip
  t)

(defmethod whitespace-process ((parser token-parser))
  (debug-format :verbose "unquoted whitespace")
  (if (initializedp parser)
      (finish-parsing parser))
  ;; not initialized, dispose the character read and skip
  t)

(defmethod comment-process ((parser token-parser) ch stream)
  (debug-format :verbose "comment syntax is detected.")
  (if (initializedp parser)
      (progn
        (unread-char ch stream)
        (finish-parsing parser))
      (loop for ch2 = (read-char stream nil :eof)
           until (or (eq ch2 :eof)
                     (char= ch2 #\NewLine))
           finally (unread-char ch2 stream))))

(defmethod expansion-process ((parser token-parser) ch stream)
  )

(defmacro while (test &rest args)
  `(loop
      while ,test
      do (progn ,@args)))

(defun read-token (stream)
  "return a token instance.

this is an implementation of below:
http://pubs.opengroup.org/onlinepubs/007908799/xcu/chap2.html#tag_001_003"
  (debug-format :verbose "read-token is called")
  (let ((ch nil)
        (parser (make-instance 'token-parser)))
    (while t
      (setq ch (read-char stream nil :eof))
      (debug-format :verbose "ch: ~A" ch)
      (cond ((eq ch :eof)               ;1, eof
             (eof-process parser))
            ((has-operator-candidates-p parser) ;2, 3, operator
             (operator-process parser ch stream))
            ((and (not (quotedp parser)) (chars= ch #\\ #\' #\")) ;4, quoting
             (quote-process parser ch))
            ((and (not (quotedp parser)) (chars= ch #\$ #\`)) ;5, expansion
             (expansion-process parser ch stream))
            ((and (not (quotedp parser)) ;6, the beginning of operators
                  (operator-startswith ch))
             (beginning-operator-process parser ch stream))
            ((and (not (quotedp parser)) (char= ch #\NewLine)) ;7, newline
             (newline-process parser))
            ((and (not (quotedp parser)) (clap:whitespacep ch)) ;8, space
             (whitespace-process parser))
            ((char= ch #\#)             ;10, comment
             (comment-process parser ch stream))
            ((initializedp parser)      ;9
             (debug-format :verbose "adding ~A into word" ch)
             (push-char parser ch))
            ((standard-char-p ch)       ;11, the beginning of word
             (debug-format :verbose "begining new word from ~A" ch)
             (push-char parser ch))
            (t (error 'clsh-bug)))
      (if (immediate-return-p parser)
          (return-from read-token (token-parser->token parser))))))

