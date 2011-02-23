(in-package :clsh)


(defclass command ()
  ((tokens :initarg :tokens
           :accessor tokens-of
           :initform nil))
  (:documentation "a container of tokens"))

(defmethod print-object ((object command) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "[~A]"
            (tokens-of object))))

(defclass simple-command (command)
  ())

(defclass pipeline (command)
  ())

(defclass list-command (command)
  ())

(defclass compound-command (command)
  ())

(defclass function-definition (command)
  ())

(defclass redirection ()
  ())

(defclass word-assignment ()
  ())

(defmethod make-simple-command ((env environment)
                                tokens redirections assignments)
  ;; 1: variable assignments or rediections are saved for step 3 and 4
  ;; 2: the words that are not variable assignments or redirections will
  ;;    be expanded. first fieldwill be the command name and the rest will be
  ;;    the arguments
  ;; 3: redirection
  ;; 4: variable assignment will be expanded for ~, parameter, command,
  ;;    arithmetic
  ;; fist of all, detect the delimiter of command: ; or NewLine
  (make-instance 'simple-command :tokens tokens))

(yacc:define-parser *token-parser*
  (:start-symbol command)
  (:terminals (:word :assignment_word :newline :token :name :io_number
                     :semicolon :ampasand :lparen :rparen :pipe))
  (:precedence nil)
  (complete_command
   (list separator)
   list)
  (list
   (list separator_op and_or)
   and_or)
  (and_or
   pipeline
   (and_or :and_if linebreak pipeline)
   (and_or :or_if linebreak pipeline))
  (pipeline
   pipe_sequence
   (:bang pipe_sequence))
  (pipe_sequence
   command
   (pipe_sequence :pipe linebreak command))
  (command
   simple_command
   compound_command
   (compound_command redirect_list)
   function_definition)
  (compound_command
   brace_group
   subshell
   for_clause
   cause_clause
   if_clause
   while_clause
   until_clause)
  (subshell
   (:lparen compound_list :rparen))
  (compound_list
   term
   (newline_list term)
   (term separator)
   (newline_list term separator))
  (term
   (term separator and_or)
   and_or)
  (for_clause
   (:for name linebreak do_group)
   (:for name linebreak :in wordlist sequential_sep do_group))
  (wordlist
   (wordlist :word)
   :word)
  (simple_command
   (cmd_prefix cmd_word cmd_suffix)
   (cmd_prefix cmd_word)
   cmd_prefix
   (cmd_name cmd_suffix
             #'(lambda (x y)
                 (if (listp y)
                     (let ((redirections
                            (remove-if-not #'(lambda (x) (typep x 'redirection)) y))
                           (cmd-args
                            (remove-if #'(lambda (x) (typep x 'redirection)) y)))
                       (make-simple-command *env* (cons x cmd-args)
                                            redirections nil))
                     (make-simple-command *env* (list x y) nil nil))))
   (cmd_name #'(lambda (x)
                 (make-simple-command *env* (list x) nil nil))))
  (cmd_name :word)
  (cmd_word :word)
  (cmd_prefix
   io_redirect
   (cmd_prefix io_redirect)
   :assignment_word
   (cmd_prefix :assignment_word))
  (cmd_suffix
   io_redirect
   (cmd_suffix io_redirect)
   :word
   (cmd_suffix :word))
  (redirect_list
   io_redirect
   (redirect_list io_redirect))
  (io_redirect
   io_file
   (:io_number io_file)
   io_here
   (:io_number io_here))
  (separator_op
   :ampasand :semicolon)
  (separator
   (separator_op linebreak)
   newline_list)
  (sequential_sep
   (:semicolon linebreak)
   newline_list)
  )

(defun list-lexr (tokens)
  #'(lambda ()
      (let ((value (pop tokens)))
        (if (null value)
            (values nil nil)
            (let ((terminal (cond
                              ((reserved-word-p value)
                               (reserved-word-identifier-of value))
                              ((eq (identifier-of value) :word)
                               (let ((s (string-of value)))
                                 (cond
                                   ((string= s "&") :ampasand)
                                   ((string= s ";") :semicolon)
                                   ((string= s "(") :lparen)
                                   ((string= s ")") :rparen)
                                   ((string= s "|") :pipe)
                                   (t :word))))
                              (t
                               (identifier-of value)))))
              (values terminal value))))))
