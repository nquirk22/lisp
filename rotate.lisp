(defun rotate (L n)
  "Rotates a list L n times"
  (cond ((eql n 0) L)
	(t (append (first L) (rest (rotate (L (1- n))))))))
  
