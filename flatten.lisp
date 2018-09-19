(defun flatten (L)
  "Flattens a nested list structure."
  (cond ((endp L) nil)
	(t (append (if (listp (first L)) (flatten (first L))
		       (list (first L)))
		   (flatten (rest L))))))
