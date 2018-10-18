(defun set-p (L)
  "Returns true if given list is a set"
  (cond ((endp L) T)
	((member (first L) (rest L)) nil)
	( t (set-p (rest L)))))

(defun make-setTR (L)
  "Eliminates any duplicate items from a list, making it a set"
  (labels ((make-set-aux (L acc)
	     (cond ((endp L) acc)
		   ((member (first L) acc) (make-set-aux (rest L) acc))
		   ( t (make-set-aux (rest L) (append acc (list (first L))))))))
    (make-set-aux L nil)))
  
