(defun compress (L)
  "Elminate any consecutive duplicate elements from a list."
  (cond ((endp L) nil)
	(t (if (eql (first L) (first (rest L))) (compress (rest L)) (cons (first L) (compress (rest L)))))))
