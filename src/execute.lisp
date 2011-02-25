(in-package :clsh)

(defmethod execute-tokens ((env environment) tokens)
  (let ((command (car tokens))
        (command-args (cdr tokens)))
    (debug-format :verbose "command: ~A" command)
    (debug-format :verbose "command-args: ~A" command-args)
    ))
