(in-package :clsh)

;; methods of environment class for expansion
(defmethod word-expansion ((env environment) (word token))
  ;; return a word or a list of the words
  (let ((tilde (tilde-expansion env word)))
    (let ((param (parameter-expansion env tilde)))
      (let ((com (command-substitusion env param)))
        (let ((arth (arithmetic-expansion env com)))
          arth)))))

;; backslash-escape-p not supported
(defun copy-stream-until (input-stream output-stream ends-strings
                          &key (backslash-escape-p nil))
  (let ((output (make-string-output-stream)))
    (loop
       for ch = (read-char input-stream nil :eof)
       until (eq ch :eof)
       do (progn
            (debug-format :verbose "ch -> ~s" ch)
            (write-char ch output)
            (let ((string (get-output-stream-string output)))
              (if (find-if #'(lambda (x) (clap:endswith string x))
                           ends-strings)
                  (progn
                    (format output-stream string)
                    (return string))
                  (format output string)))))
    (format output-stream (get-output-stream-string output))))

(defun read-tilde-suffix (input)
  "read until unquoted / or :.

READ-TILDE-SUFFIX returns :quoted if it finds a quoting character."
  (let ((output (make-string-output-stream)))
    (loop
       for ch = (read-char input nil :eof)
       until (eq ch :eof)
       if (chars= ch #\' #\\ #\")
       do (return-from read-tilde-suffix :quoted)
       else if (chars= ch #\/ #\:)
       do (progn (debug-format :verbose "terminate tilde suffix: ~A" ch)
                 (unread-char ch input) (return))
       else 
       do (write-char ch output))
    (get-output-stream-string output)))

(defmethod resolve-homedir ((env environment) user)
  (debug-format :verbose "resolving homedir: ~A" user)
  (if (or (null user) (string= user ""))
      (getenv env "HOME")
      (let ((passwd (clap-pwd:getpwnam user)))
        (let ((ret (clap-pwd:pw-dir passwd)))
          (if ret ret
              (error 'unknown-user :user user))))))

(defmethod tilde-expansion ((env environment) (word token))
  (debug-format :verbose "tilda expansion for: ~A" word)
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
         else if (char= ch #\')            ;skip until '
         do (copy-stream-until token-stream output '("'"))
         else if (char= ch #\")            ;skip until "
         do (copy-stream-until token-stream output '("\"") :backslash-escape-p t)
         else if (char= ch #\~)            ;unquoted tilda!
         do (progn
              (debug-format :verbose "detect tilde")
              (let ((user (read-tilde-suffix token-stream)))
                (if (eq user :quoted)
                    (progn     ; detect quoting, abort tilde expansion
                      (debug-format :verbose "quoting is detected. abort tilde ~
 expansion")
                      ;; no tilda expansion, clear output and copy token-string
                      (get-output-stream-string output)
                      (return (write-string token-string output)))
                    (let ((replace (resolve-homedir env user)))
                      (write-string replace output))))) ; copy to output
         else
         do (write-char ch output))
      (let ((id (identifier-of word)))
        (make-instance 'token :identifier id
                       :string (get-output-stream-string output))))))

(defun read-longest-name (input)
  (with-output-to-string (output)
    (loop
       for ch = (read-char input nil :eof)
       until (eq ch :eof)
       if (chars= ch #\\ #\' #\" #\` #\$ #\  #\NewLine #\{ #\() ; NB: valid?
       do (progn (unread-char ch input)
                 (return))
       else do (write-char ch output))))

(defmethod parameter-expansion ((env environment) (words list))
  (mapcar #'(lambda (x) (parameter-expansion env x)) words))

(defmethod resolve-parameter ((env environment) var)
  (debug-format :verbose "lookup ~A" var)
  (getenv env var))                     ;NB: what will happen if lookup undefined variable?

(defmethod braced-parameter-expansion ((env environment) input)
  (error 'not-implemented))

(defmethod parameter-expansion ((env environment) (word token))
  (debug-format :verbose "parameter expansion for: ~A" word)
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
         else if (char= ch #\')            ;skip until '
         do (copy-stream-until token-stream output '("'"))
         else if (char= ch #\$)
         do (progn
              (debug-format :verbose "detect $")
              (let ((ch2 (read-char token-stream nil :eof)))
                (cond ((eq ch2 :eof)  ;$
                       (write-char ch output))
                      ((char= ch2 #\{) ;${HOGE}
                       (braced-parameter-expansion env token-stream))
                      (t              ;$HOGE
                       (unread-char ch2 token-stream)
                       ;; scan the possible longest name
                       (let ((name (read-longest-name token-stream)))
                         (debug-format :verbose "variable name: ~A" name)
                         (let ((replace (resolve-parameter env name)))
                           (write-string replace output)))))))
         else do (write-char ch output))
      (let ((id (identifier-of word)))
        (make-instance 'token :identifier id
                       :string (get-output-stream-string output))))))

(defmethod command-substitusion ((env environment) (words list))
  (mapcar #'(lambda (x) (command-substitusion env x)) words))

(defmethod command-substitusion ((env environment) (word token))
  word)

(defmethod arithmetic-expansion ((env environment) (words list))
  (mapcar #'(lambda (x) (arithmetic-expansion env x)) words))

(defmethod arithmetic-expansion ((env environment) (word token))
  word)

