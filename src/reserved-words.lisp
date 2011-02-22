(in-package :clsh)

(defvar *reserved-words* nil)

(defclass reserved-word (token)
  ((reserved-word-identifier
    :initarg :reserved-word-identifier
    :accessor reserved-word-identifier-of
    :documentation "the identifier for reserverd words")))

(defmacro define-reserved-word (sym string)
  (let ((wd (gensym)))
    `(let ((,wd (make-instance 'reserved-word
                               :identifier :word
                               :reserved-word-identifier ,sym
                               :string ,string)))
       (if (not (member ,wd *reserved-words*
                        :test #'eq
                        :key #'reserved-word-identifier-of))
           (push ,wd *reserved-words*))
       ,wd)))

(define-reserved-word :if "if")
(define-reserved-word :then "then")
(define-reserved-word :else "else")
(define-reserved-word :elif "elif")
(define-reserved-word :fi "fi")
(define-reserved-word :do "do")
(define-reserved-word :done "done")

(define-reserved-word :case "case")
(define-reserved-word :esac "esac")
(define-reserved-word :while "while")
(define-reserved-word :until "until")
(define-reserved-word :for "for")

(define-reserved-word :lbrace "{")
(define-reserved-word :rbrace "}")
(define-reserved-word :bang "!")

(define-reserved-word :in "in")

