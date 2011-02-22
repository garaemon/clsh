(defsystem clsh
    :version "0.0.0"
    :license "New BSD"
    :depends-on (clap)
    :components
    ((:module "src" :components
              ((:file "clsh")
               (:file "condition" :depends-on ("clsh"))
               (:file "debug" :depends-on ("clsh"))
               (:file "tokenize" :depends-on ("clsh" "debug"))
               ))))
