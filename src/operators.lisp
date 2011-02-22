(in-package :clsh)

(defvar *operators* nil)

;; should be a subclass of token? os have token?
(defclass operator (token)
  ((identifier :accessor identifier-of
               :initarg :identifier)
   (string :accerror string-of
           :initarg :string))
  )

(defmethod print-object ((object operator) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "operator: ~A [~A]"
            (identifier-of object) (string-of object))))

(defmacro define-operator (sym string)
  (let ((op (gensym)))
    `(let ((,op (make-instance 'operator
                               :identifier ,sym
                               :string ,string)))
       (if (not (member ,op *operators*
                        :test #'eq
                        :key #'identifier-of))
           (push ,op *operators*))
       ,op)))

(define-operator :and_if "&&")
(define-operator :or_if "||")
(define-operator :dsemi ";;")
(define-oeprator :dless "<<")
(define-operator :dgreat ">>")
(define-operator :lessand "<&")
(define-operator :greatand ">&")
(define-operator :lessgreat "<>")
(define-operator :dlessdash "<<-")

(define-operator :clobber ">|")

;; (define-operator :if "if")
;; (define-operator :then "then")
;; (define-operator :else "else")
;; (define-operator :elif "elif")
;; (define-operator :fi "fi")
;; (define-operator :do "do")
;; (define-operator :done "done")

;; (define-operator :case "case")
;; (define-operator :esac "esac")
;; (define-operator :while "while")
;; (define-operator :until "until")
;; (define-operator :for "for")

;; (define-operator :lbrace "{")
;; (define-operator :rbrace "}")
;; (define-operator :bang "!")

;; (define-operator :in "in")

