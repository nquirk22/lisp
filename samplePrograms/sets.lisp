

;;;=================================================================
;;;    NAME: setp
;;;  ARG(S): any list
;;; RETURNS: Returns whether the list is a set

(defun setp (L)
  "Test an expression to see if it is a set"
  (and (listp L)
       (cond ((endp L) t)
	     ((member (first L) (rest L)) nil)
	     ( t (setp (rest L)))))
  )

;;;=================================================================
;;;    NAME: make-set
;;;  ARG(S): any list
;;; RETURNS: the set corresponding to the list

(defun make-set (L)
  "Returns a set from a list. Duplicate elements are removed."
  (cond ((endp L) nil)
	((member (first L) (rest L)) (make-set (rest L)))
	( t (cons (first L)(make-set (rest L))))))

;;;=================================================================
;;;    NAME: my-union
;;;  ARG(S): two sets
;;; RETURNS: returns the union of two sets by calling union-aux

(defun my-union (S1 S2)
  "Returns the union of two sets."
  (cond ((endp S1) S2)
	((member (first S1) S2) (my-union (rest S1) S2))
	( t (my-union (rest S1) (cons (first S1) S2)))))

;;;=================================================================
;;;    NAME: my-intersection
;;;  ARG(S): two sets
;;; RETURNS: returns the intersection of two sets

(defun my-intersection (S1 S2)
  "Returns the intersection of two sets  by calling intersection-aux"
  (cond ((endp S1) nil)
	((member (first S1) S2) (cons (first S1) 
				      (my-intersection (rest S1) S2)))
	( t (my-intersection (rest S1) S2))))

;;;=================================================================
;;;    NAME: power-set
;;; "ARG(S): any set
;;; RETURNS:  the powerset 


(defun power-set (S) 
  "Returns  of the powerset of the arguments, which must be a set"
  (cond ((endp S)(list S))
        ( t (append (cons-to-all (first S) (power-set (rest S)))
		    (power-set (rest S))))))
	
(defun cons-to-all (e L)
  (cond ((endp L) nil)
	( t (cons (cons e (first L))
		  (cons-to-all e (rest L))))))
  

;;; ANOTHER VERSION AVOIDING THE DOUBLE RECURSION

(defun power-set2 (S) 
  (cond ((endp S)(list S))
        ( t (let ((power-rest (power-set2 (rest S))))
	      (append (cons-to-all (first S) power-rest)
		      power-rest)))))


;;; ANOTHER VERSION USING MAPCAR AND A LAMBDA FUNCTION

(defun power-set3 (S) 
  (if (endp S)
      (list S)
    (append (mapcar #'(lambda (L)(cons (first S) L))
		    (power-set3 (rest S)))
	    (power-set3 (rest S)))))

;;; YET ANOTHER VERSION: USING AN ACCUMULATOR AND A CLOSURE
(defun power-set4 (L)
  (power-set4-aux L (list nil)))

(defun power-set4-aux (L A)
  (cond ((endp L) A)
	( t (power-set4-aux (rest L) 
			    (append A (mapcar (lambda (x)(cons (first L) x)) 
					      A))))))

;;;===============================================================
;;;    NAME: subset
;;;  ARG(S): two sets
;;; RETURNS: T if first set is a subset of the second set

(defun subset  (S1 S2) 
  "Returns T if all elements of first set are elements of second set"
  (cond ((endp S1) t)
	((member (first S1) S2) (subset (rest S1) S2))
	( t nil )))

;;;===============================================================
;;;    NAME: set-equal 
;;;  ARG(S): two sets
;;; RETURNS: T if the two sets have the very same elements

(defun set-equal (S1 S2)
  "Returns T if sets S1 and S2 have the very same elements"
  (and (subset S1 S2)
       (subset S2 S1)))


;;; another version
(defun set-equal (S1 S2)
  "Returns T if sets S1 and S2 have the very same elements"
  (cond((endp S1)(null s2))
       ((not(member(first S1) S2)) nil)
       ( t (set-equal(rest S1)
		     (remove (first S1) S2)))))

;;;================================================================
;;;  end of file
