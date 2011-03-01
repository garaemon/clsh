(defsystem clsh
    :version "0.0.0"
    :license "New BSD"
    :depends-on (clap yacc)
    :components
    ((:module "src" :components
              ((:file "clsh")
               (:file "condition" :depends-on ("clsh"))
               (:file "debug" :depends-on ("clsh"))
               (:file "util" :depends-on ("clsh" "debug"))
               (:file "token" :depends-on ("clsh" "debug" "util"))
               (:file "environment" :depends-on ("clsh" "debug"))
               (:file "operators" :depends-on ("token"))
               (:file "reserved-words" :depends-on ("token"))
               (:file "tokenize" :depends-on ("clsh" "debug" "token"))
               (:file "parse" :depends-on ("clsh" "tokenize" "environment"))
               (:file "expansion" :depends-on ("clsh" "tokenize" "environment"))
               (:file "command" :depends-on ("clsh" "environment"))
               (:file "builtin-command" :depends-on ("command"))
               ))))
