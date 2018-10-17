(defun myReverse (L)
  "Returns a shallow reversal of a list"
  (labels ((myReverse-aux (L acc)
	     (cond ((endp L) acc)
		   (t (myReverse-aux (rest L) (cons (first L) acc))))))
    (myReverse-aux L '())))
