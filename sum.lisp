(defun sum (n m)
  "Returns the sum of two numbers."
  (cond ((not (and (integerp n) (integerp m))) nil)
	((zerop n) m)
	((zerop m) n)
	((< n 0) (sum (1+ n) (1- m)))
	((< m 0) (sum (1- n) (1+ n)))
	(t (sum (1- n) (1+ m)))))
