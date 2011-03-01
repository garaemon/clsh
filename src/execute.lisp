(in-package :clsh)

(defmethod execute-tokens ((env environment) tokens)
  (let ((command-token (car tokens))
        (command-args (cdr tokens)))
    (debug-format :verbose "command-token: ~A" command-token)
    (debug-format :verbose "command-args: ~A" command-args)
    (let ((command (resolve-command env command-token)))
      (debug-format :verbose "command: ~A" command)
      )
    ))

