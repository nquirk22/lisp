(defun  my-replace (elt1 elt2 L)
  "Returns list L with all occurences of e1 replaced."
  (labels ((my-replace-aux (elt1 elt2 L)
              (cond ((endp L) nil)
                    ((equal (first L) elt1) (cons elt2 (rest L)))
                    ((listp (first L)) (my-replace-aux elt1 elt2 (first L)))
                    (t (my-replace-aux elt1 elt2 (rest L))))))
    
  (my-replace-aux elt1 elt2 L)))
