(in-package :clsh)

;;utility
(defun chars= (ch &rest chars)
  (if (null chars)
      t
      (and (char= ch (car chars))
           (chars= ch (cdr chars)))))

(defun read-token (stream)
  "return a token instance.

this is an implementation of below:
http://pubs.opengroup.org/onlinepubs/007908799/xcu/chap2.html#tag_001_003"
  (debug-level-format :verbose "read-token is called")
  (let ((ch nil)
        (current-token-stream (make-string-output-stream))
        (initialized nil)
        (quoated nil)
        (quating-character nil))
    (labels ((push-char (ch)
               (write-char ch current-token-stream)
               (if (not initialized)
                   (setq initialized t)))
             (return-token ()
               (return-from read-token (make-token current-token-stream)))
             (quating (quate-char)
               (setq quated t)
               (setq quating-character quate-char)))
      (while t
        (setq ch (read-char stream nil :eof))
        (cond
          ((eq ch :eof)                 ;1
           ;; end of file
           (if initialized
               ;; no current token, so the end-of-input indicator is returned
               ;; end-of-input indicator = nil
               (return-from read-token nil)
               ;; the current token is, so token will be delimited
               (return-token)))
          ;; 2?
          ;; 3?
          ((and (not quoted)            ;4, quoting
                (chars= ch #\\ #\' #\"))
           (quating ch))
          ((and (not quoted)            ;5, expantion
                (chars= ch #\$ #\`))
           )
          ((and (not quoted)            ;6?
                ))
          ;; NB: what happend if bland line is?
          ((and (not quoted)            ;7, unquoted newline and the current 
                (char= ch #\NewLine))   ;   token will be delimited
           (return-token))
          
          (t                              ;might be error
           (error 'clsh-bug)))))))

