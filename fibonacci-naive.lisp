(defun fibonacci (n)
  "Returns the nth value in the Fibonacci sequence"
  (cond	((<= n 1) n)
	(t (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
