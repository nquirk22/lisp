(defun myFactorial (n)
  "Returns the value of n!"
  (cond ((zerop n) 1)
	((= n 1) 1)
	( t (* n (myFactorial (1- n))))))
