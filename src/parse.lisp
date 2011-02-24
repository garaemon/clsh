(in-package :clsh)

#|
grammer
complete_command : list separator
                 | list
                 ;
list             : list separator_op and_or
                 |                   and_or
                 ;
and_or           :                         pipeline
                 | and_or AND_IF linebreak pipeline
                 | and_or OR_IF  linebreak pipeline
                 ;
pipeline         :      pipe_sequence
                 | Bang pipe_sequence
                 ;
pipe_sequence    :                             command
                 | pipe_sequence '|' linebreak command
                 ;
command          : simple_command
                 | compound_command
                 | compound_command redirect_list
                 | function_definition
                 ;
compound_command : brace_group
                 | subshell
                 | for_clause
                 | case_clause
                 | if_clause
                 | while_clause
                 | until_clause
                 ;
subshell         : '(' compound_list ')'
                 ;
compound_list    :              term
                 | newline_list term
                 |              term separator
                 | newline_list term separator
                 ;
term             : term separator and_or
                 |                and_or
                 ;
for_clause       : For name linebreak                            do_group
                 | For name linebreak in wordlist sequential_sep do_group
                 ;
name             : NAME                     /* Apply rule 5 */
                 ;
in               : In                       /* Apply rule 6 */
                 ;
wordlist         : wordlist WORD
                 |          WORD
                 ;
case_clause      : Case WORD linebreak in linebreak case_list Esac
                 | Case WORD linebreak in linebreak           Esac
                 ;
case_list        : case_list case_item
                 |              case_item
                 ;
case_item        :     pattern ')' linebreak     DSEMI linebreak
                 |     pattern ')' compound_list DSEMI linebreak
                 | '(' pattern ')' linebreak     DSEMI linebreak
                 | '(' pattern ')' compound_list DSEMI linebreak
                 ;
pattern          :             WORD         /* Apply rule 4 */
                 | pattern '|' WORD     /* Do not apply rule (4) */
                 ;
if_clause        : If compound_list Then compound_list else_part Fi
                 | If compound_list Then compound_list           Fi
                 ;
else_part        : Elif compound_list Then else_part
                 | Else compound_list
                 ;
while_clause     : While compound_list do_group
                 ;
until_clause     : Until compound_list do_group
                 ;
function_definition : fname '(' ')' linebreak function_body
                 ;
function_body    : compound_command                /* Apply rule 9 */
                 | compound_command redirect_list  /* Apply rule 9 */
                 ;
fname            : NAME                            /* Apply rule 8 */
                 ;
brace_group      : Lbrace compound_list Rbrace
                 ;
do_group         : Do compound_list Done
                 ;
simple_command   : cmd_prefix cmd_word cmd_suffix
                 | cmd_prefix cmd_word
                 | cmd_prefix
                 | cmd_name cmd_suffix
                 | cmd_name
                 ;
cmd_name         : WORD                   /* Apply rule 7a */
                 ;
cmd_word         : WORD                   /* Apply rule 7b */
                 ;
cmd_prefix       :            io_redirect
                 | cmd_prefix io_redirect
                 |            ASSIGNMENT_WORD
                 | cmd_prefix ASSIGNMENT_WORD
                 ;
cmd_suffix       :            io_redirect
                 | cmd_suffix io_redirect
                 |            WORD
                 | cmd_suffix WORD
                 ;
redirect_list    :               io_redirect
                 | redirect_list io_redirect
                 ;
io_redirect      :           io_file
                 | IO_NUMBER io_file
                 |           io_here
                 | IO_NUMBER io_here
                 ;
io_file          : '<'       filename
                 | LESSAND   filename
                 | '>'       filename
                 | GREATAND  filename
                 | DGREAT    filename
                 | LESSGREAT filename
                 | CLOBBER   filename
                 ;
filename         : WORD                      /* Apply rule 2 */
                 ;
io_here          :    DLESS     here_end
                 |    DLESSDASH here_end
                 ;
here_end         : WORD                      /* Apply rule 3 */
                 ;
newline_list     :              NEWLINE
                 | newline_list NEWLINE
                 ;
linebreak        : newline_list
                 | /* empty */
                 ;
separator_op     : '&'
                 | ';'
                 ;
separator        : separator_op linebreak
                 | newline_list
                 ;
sequential_sep   : ';' linebreak
                 | newline_list
                 ;
|#

(defmethod exec-simple-command ((env environment) tokens
                                redirections assignments)
  (let ((expanded-tokens
         (mapcar #'(lambda (x) (word-expansion env x)) tokens)))
    (debug-format :debug "before expanded tokens => ~A" tokens)
    (debug-format :debug "expanded-tokens => ~A" expanded-tokens)
    (execute-tokens env expanded-tokens)
    expanded-tokens))

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
                       (exec-simple-command *env* (cons x cmd-args)
                                            redirections nil))
                     (exec-simple-command *env* (list x y) nil nil))))
   (cmd_name #'(lambda (x)
                 (exec-simple-command *env* (list x) nil nil))))
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
