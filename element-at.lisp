(defun element-at (L i)
  "Returns the element at the specified location in the list."
  (cond ((eql i 1) (first L))
	 (t (element-at (rest L) (1- i)))))
