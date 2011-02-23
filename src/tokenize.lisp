(in-package :clsh)

;;utility
(defun chars= (ch &rest chars)
  (if (null chars)
      nil
      (or (char= ch (car chars))
          (apply #'chars= ch (cdr chars)))))

(defclass tokenizer ()
  ((token-stream :initform (make-string-output-stream)
                 :accessor token-stream-of
                 :documentation "a stream to store the characters for token")
   (token-hint :initform nil
               :accessor token-hint-of
               :documentation "a hint of the kind of the token to be read")
   (initialized :initform nil
                :accessor initialized-of
                :documentation "INITIALIZED will be T if tokenizer begins
to parse string.")
   (quoted :initform nil
           :accessor quoted-of
           :documentation "QUATED will be T if tokenizer begins to parse
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
 convert tokenizer to token."))
  (:documentation
   "this is a helper class for tokenization. tokenizer is created to
parse a token and never be reused."))

(defmethod has-operator-candidates-p ((parser tokenizer))
  (operator-candidates-of parser))

(defmethod quotedp ((parser tokenizer))
  (quoted-of parser))

(defmethod initializedp ((parser tokenizer))
  (initialized-of parser))

(defmethod immediate-return-p ((parser tokenizer))
  (immediate-return-p-of parser))

(defmethod tokenizer->token ((parser tokenizer))
  (if (initializedp parser)
      (make-token (get-output-stream-string (token-stream-of parser))
                  (token-hint-of parser))
      nil))

(defmethod token-string ((parser tokenizer))
  (get-output-stream-string (token-stream-of parser)))

(defmethod eof-process ((parser tokenizer))
  (debug-format :verbose "eof is detected")
  (finish-parsing parser))

(defmethod finish-parsing ((parser tokenizer))
  (debug-format :verbose "finish parsing")
  (setf (immediate-return-p-of parser) t))

(defmethod push-char ((parser tokenizer) ch)
  (write-char ch (token-stream-of parser))
  (incf (count-of parser))
  (setf (initialized-of parser) t)
  ch)

(defmethod operator-process ((parser tokenizer) ch stream)
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

(defmethod unquote-parser ((parser tokenizer))
  (setf (quoted-of parser) nil)
  (setf (quoting-char-of parser) nil))

(defmethod backslash-quote-p ((parser tokenizer))
  (and (quotedp parser)
       (char= (quoting-char-of parser) #\\)))

(defmethod doublequote-quote-p ((parser tokenizer))
  (and (quotedp parser)
       (char= (quoting-char-of parser) #\")))

(defmethod singlequote-quote-p ((parser tokenizer))
  (and (quotedp parser)
       (char= (quoting-char-of parser) #\')))

(defmethod beginning-operator-process ((parser tokenizer) ch stream)
  (debug-format :verbose "beigins operator")
  (if (initializedp parser)
      (progn
        (unread-char ch stream)
        (finish-parsing parser)
        (if (and (chars= ch #\< #\>)
                 (clap:isdigit (token-string parser)))
            (setf (token-hint-of parser) :io_number)))
      (progn
        (setf (operator-candidates-of parser) (operator-startswith ch))
        (debug-format :verbose "ch: ~s" ch)
        (debug-format :verbose "candidates: ~A" (operator-candidates-of parser))
        (setf (token-hint-of parser) :operator)
        (push-char parser ch))))

(defmethod newline-process ((parser tokenizer) ch stream)
  (debug-format :verbose "unquoted newline")
  (if (initializedp parser)
      (progn
        (unread-char ch stream)
        (finish-parsing parser))
      (progn
        (push-char parser ch)
        (setf (token-hint-of parser) :newline)
        (finish-parsing parser))))

(defmethod whitespace-process ((parser tokenizer))
  (debug-format :verbose "unquoted whitespace")
  (if (initializedp parser)
      (finish-parsing parser))
  ;; not initialized, dispose the character read and skip
  t)

(defmethod comment-process ((parser tokenizer) ch stream)
  (debug-format :verbose "comment syntax is detected.")
  (if (initializedp parser)
      (progn
        (unread-char ch stream)
        (finish-parsing parser))
      (loop for ch2 = (read-char stream nil :eof)
           until (or (eq ch2 :eof)
                     (char= ch2 #\NewLine))
           finally (unread-char ch2 stream))))

(defmethod expansion-process ((parser tokenizer) ch stream)
  (case ch
    (#\$
     (expansion-doller-process parser ch stream))
    (#\`
     (expansion-backquote-process parser ch stream))
    (t
     (error 'clsh-bug))))

;; NB:
(defmethod quote-process ((parser tokenizer) ch stream)
  (setf (quoted-of parser) t)
  (setf (quoting-char-of parser) ch))

;; NB: nested syntax is not supported!
(defmethod read-until ((parser tokenizer) end-string stream)
  ;; return the result as string
  ;; its not an effcient implementation
  (let ((output (make-string-output-stream)))
    (loop
       for ch = (read-char stream nil :eof)
       if (eq ch :eof)
       do (error 'syntax-error :err-str (format nil "mismatch ~A" end-string))
       else 
       do (progn
            (debug-format :verbose "ch -> ~A" ch)
            (write-char ch output)
            (let ((string (get-output-stream-string output)))
              ;; it will clear OUPUT stream
              (if (clap:endswith string end-string)
                  (progn
                    (format (token-stream-of parser) string)
                    (return string))
                  (format output string)))))))

(defmethod expansion-doller-process ((parser tokenizer) ch stream)
  ;; ch = $
  (push-char parser ch)
  (let ((ch2 (read-char stream nil :eof)))
    (cond
      ((eq ch2 :eof)                    ; only $
       (finish-parsing parser))
      ((char= ch2 #\()                  ;$(
       (push-char parser ch2)
       (let ((ch3 (read-char stream nil :eof)))
         (cond ((char= ch3 #\()         ;$((
                ;; read until ))
                (push-char parser ch3)
                (read-until parser "))" stream))
               (t
                ;; read until )
                (unread-char ch2 stream)
                (read-until parser ")" stream)))))
      ((char= ch2 #\{)                  ;${
       ;; read until }
       (push-char parser ch2)
       (read-until parser "}" stream))
      (t                                ;$hoge...
       (push-char parser ch2))
      )))

(defmacro while (test &rest args)
  `(loop
      while ,test
      do (progn ,@args)))

(defun read-token (stream &optional (context nil))
  "return a token instance.

this is an implementation of below:
http://pubs.opengroup.org/onlinepubs/007908799/xcu/chap2.html#tag_001_003"
  (debug-format :verbose "read-token is called")
  (let ((ch nil)
        (parser (make-instance 'tokenizer)))
    (while t
      (setq ch (read-char stream nil :eof))
      (debug-format :verbose "ch: ~s" ch)
      (cond ((eq ch :eof)               ;1, eof
             (eof-process parser))
            ((has-operator-candidates-p parser) ;2, 3, operator
             (operator-process parser ch stream))
            ((and (not (quotedp parser)) (chars= ch #\\ #\' #\")) ;4, quoting
             (quote-process parser ch stream))
            ((backslash-quote-p parser) ;4, closing backslash quote
             (push-char parser ch)
             (unquote-parser parser))
            ((and (doublequote-quote-p parser) (char= ch #\")) ;4
             (push-char parser ch)
             (unquote-parser parser))
            ((and (singlequote-quote-p parser) (char= ch #\')) ;4
             (push-char parser ch)
             (unquote-parser parser))
            ((and (or (not (quotedp parser)) (doublequote-quote-p parser))
                  (chars= ch #\$ #\`))  ;5, expansion
             (expansion-process parser ch stream))
            ((and (not (quotedp parser)) ;6, the beginning of operators
                  (operator-startswith ch))
             (beginning-operator-process parser ch stream))
            ((and (not (quotedp parser)) (char= ch #\NewLine)) ;7, newline
             (newline-process parser ch stream))
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
          (return-from read-token (tokenizer->token parser))))))

(defun read-tokens (stream)
  (let ((tokens nil)
        (token nil)
        (context nil))
    (while (setq token (read-token stream context))
      (push token tokens)
      ;; NB: update token here
      (setf context (update-context token context)))
    (nreverse tokens)))

