;;; A simple example of mutual recursion

(defun odds (L)
  "Evaluates to the list of the odd numbered members of a list."
  (cond ((endp l) nil)
	( t (cons (first L)(evens(rest L))))))

(defun evens (l)
  "Evaluates to the list of the even numbered members of a list."
  (cond ((endp L) nil)
	( t (odds (rest L)))))


