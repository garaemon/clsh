(in-package :clsh)

(defvar *operators* nil)

;; should be a subclass of token? os have token?
(defclass operator (token)
  ())

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
(define-operator :dless "<<")
(define-operator :dgreat ">>")
(define-operator :lessand "<&")
(define-operator :greatand ">&")
(define-operator :lessgreat "<>")
(define-operator :dlessdash "<<-")

(define-operator :clobber ">|")

(defun operator-startswith (ch &optional (operators *operators*))
  "return the list of operators starts with ch"
  (remove-if-not #'(lambda (op)
                     (char= ch (elt (string-of op) 0)))
                 *operators*))

(defun operator-successor (ch operators count)
  )
