(defun deepReverse (L)
  "Returns a deep reversal of a list"
  (labels ((deepReverse-aux (L acc)
	     (cond ((endp L) acc)
		   ((listp (first L)) (deepReverse-aux (rest L) (cons (deepReverse (first L)) acc)))
		   ( t (deepReverse-aux (rest L) (cons (first L) acc))))))
    (deepReverse-aux L '())))
