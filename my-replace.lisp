(defun my-replace (e1 e2 L)
  "Replaces all occurences of element e1 with e2 at all levels of the list structure"
  (cond ((endp L) nil)
	((equal e1 (first L)) (cons e2 (my-replace e1 e2 (rest L))))
	((listp (first L)) (cons (my-replace e1 e2 (first L)) (my-replace e1 e2 (rest L))))
	(t (cons (first L) (my-replace e1 e2 (rest L))))))
