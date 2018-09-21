(defun compress (L)
  "Elminate any consecutive duplicate elements from a list."
  (labels ((compress-aux (subL)
	     (cond ((not (eql (first subL) (first (rest subL)))) (compress subL))
		   (t (compress-aux (rest subL)))))))
  (cond ((endp L) nil)
	(t (cons (first L) (compress-aux (rest L))))))
