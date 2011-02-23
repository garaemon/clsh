(in-package :clsh)

;; methods of environment class for expansion
(defmethod word-expansion ((env environment) (word token))
  ;; return a word or a list of the words
  (let ((tilda (tilde-expansion env word)))
    (let ((param (parameter-expansion env tilda)))
      (let ((com (command-substitusion env param)))
        (let ((arth (arithmetic-expansion env com)))
          ))))
  )

(defmethod tilda-expansion ((env environment) (word token))
  (let* ((token-string (string-of word))
         (token-stream (make-string-input-stream token-string)))
    ;; scanning string until unquoted tilda will be found
    (let ((output (make-string-output-stream)))
      (loop
         for ch = (read-char token-stream nil :eof)
         until (eq ch :eof)  
         if (char= ch #\\)
         do (let ((ch (read-char token-stream nil :eof)))
              (write-char ch output))
         elif (char= ch #\')            ;skip until '
         do nil
         elif (char= ch #\")            ;skip until "
         do nil
         elif (char= ch #\~)            ;tilda!
         do nil
         else
         do (write-char ch output))
      word)))

(defmethod parameter-expansion ((env environment) (word token))
  word)

(defmethod command-substitusion ((env environment) (word token))
  word)

(defmethod arithmetic-expansion ((env environment) (word token))
  word)

