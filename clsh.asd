(defsystem clsh
    :version "0.0.0"
    :license "New BSD"
    :depends-on (clap)
    :components
    ((:module "src" :components
              ((:file "clsh")
               (:file "condition" :depends-on ("clsh"))
               (:file "debug" :depends-on ("clsh"))
               (:file "token" :depends-on ("clsh" "debug"))
               (:file "operators" :depends-on ("token"))
               (:file "reserved-words" :depends-on ("token"))
               (:file "parse" :depends-on ("clsh" "debug"))
               ))))
