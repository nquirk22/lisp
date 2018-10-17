;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Credit: Dr. Richard Wyatt

(defun odds (L)
  "Returns the odd-numbered members of a list"
  (cond ((endp L) nil)
	( t (cons (first L) (evens (rest L))))))

(defun evens (L)
  "Returns the even-numbered members of a list"
  (cond ((endp L) nil)
	( t (odds (rest L)))))
