(defun factorialTR (n)
  "Computes n! tail-recursively"
  (labels ((fact-aux (n acc)
	     (cond ((zerop n) acc)
		   ((= n 1) acc)
		   ( t (fact-aux (1- n) (* n acc))))))
    (fact-aux n 1)))
