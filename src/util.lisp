(in-package :clsh)

(defun valid-name-p (string)
  (debug-format :verbose "checking ~A is a valid name" string)
  (and (> (length string) 0)               ;at least, containing one character
       (not (digit-char-p (elt string 0)))    ;not starts with digit
       ;; contains only alphabetics, digits and underscores
       (loop
            for ch across string
            if (not (or (alphanumericp ch)
                        (digit-char-p ch)
                        (char= ch #\_)))
            return nil
            finally (return t))))

(defun list-concatenate (x y)
  (if (listp x)
      (append x (list y))
      (list x y)))
