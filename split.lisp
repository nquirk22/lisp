(defun mySplit (L n)
  "Splits a list into two parts, given the length of the first part"
  (labels ((split-aux (L1 L2)
	     (cond (eql n 0) (cons '(L1 L2))
		   (append (L1) (list (first (L2))))))))
  (split-aux (
