(defun deepReverse (L)
  "Returns a deep reversal of a list"
  (cond ((endp L) nil)
	(t (append (myReverse (rest L))
		   (if (listp (first L)) (list (myReverse (first L))) (list (first L))))))) 
