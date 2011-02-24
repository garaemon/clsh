(in-package :clsh)

(defmethod execute-tokens ((env environment) tokens)
  (let ((command (car tokens))
        (command-args (cdr tokens)))
    ))
