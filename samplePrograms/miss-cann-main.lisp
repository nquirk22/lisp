;;; runs under CMUCL - Sept 2015 -- RWW
;;;=====================================================================
;;; CROSS THE RIVER WITHOUT BEING EATEN
;;;
;;;  There are three missionaries and three cannibals on one side of a
;;;  river; suppose that they are on the left hand side.  They wants
;;;  to get across to the other side.  There is a small boat that is
;;;  able to carry only two people at the one time.  Only a missionary
;;;  can pilot the boat.  If it ever happens that there are more
;;;  cannibals than missionaries on one side of the river, then the
;;;  missionaries will be eaten unless the boat is docked at that side
;;;  of the river. (Cannibals do not try to eat anyone who is able to
;;;  escape.)
;;;


;;;===================================================================
;;;     CROSS: top level function - convenient starting function
;;;     INPUT: ---
;;;    OUTPUT: prints the final list of states of the river
;;; CALLED BY: ---
;;;     CALLS: cross-river

(defun cross ()
  (cross-river (list start-state)))
       
;;;======================================================================
;;; STARTING AND ENDING STATES

(defconstant start-state (make-state '() '(b m m m c c c))) 
(defconstant end-state   (make-state '(b m m m c c c) '()))        
         
;;;======================================================================
;;;        NAME: CROSS-RIVER: MAIN TOP LEVEL FUNCTION
;;;       INPUT: ---
;;;      OUTPUT: 
;;; DESCRIPTION:
;;;   CALLED BY: ---
;;;       CALLS: 

(defun cross-river (states)
  (let* ((state (first states))
	 (next-states 
	  (remove-visited(remove-banned(generate-nexts '((b m)(b m m)(b m c)) state))
			 states))
	 )
    (cond((state-equal state end-state) (print states))
	 (t (cross-river-many states next-states)))))


;;;======================================================================
;;; CROSS-RIVER-MANY:  CROSS THE RIVER BEGINING FROM ANY STATE, state.
;;;     INPUT: state
;;;    OUTPUT: prints the list of states so far and the next state
;;; CALLED BY: cross, cross-from
;;;     CALLS: generate, prune, select, is-end-state, cross-from

(defun cross-river-many (states next-states)
  (cond((endp next-states) nil)
       (t (cross-river (cons(first next-states) states))
	  (cross-river-many states (rest next-states)))))


;;;======================================================================
;;; GENERATE-NEXTS:  generate the list of all possible next states from a given state
;;;     INPUT: state
;;;    OUTPUT: list of states
;;; CALLED BY: cross-from
;;;     CALLS: remove-nill, switch

(defun generate-nexts (L state)
  (cond ((endp L) nil)
	( t (cons (switch (first L) state)
		  (generate-nexts (rest L) state)))))

;;;======================================================================
;;; SWITCH: SWITCH EVERY MEMBER OF list TO THE SIDE OF state IT IS NOT ON
;;;     INPUT: state
;;;    OUTPUT: state
;;; CALLED BY: generate
;;;     CALLS: has-boat, memb-all, remove-all 
;;;	      left-side, right-side, make-state

(defun switch (list state)
    (cond ((and (has-boat(left-side state))
                (memb-all list (left-side state)))
           (make-state (remove-all list (left-side state))
                       (append list (right-side state)) ) )
          ((and (has-boat(right-side state))  
                (memb-all list (right-side state)))        
           (make-state (append list (left-side state))
                       (remove-all list (right-side state)) ) ) ) )
                       
;;;======================================================================
;;; HAS-BOAT:  DETERMINE WHETHER THE SIDE OF A STATE, side, HAS THE BOAT
;;;     INPUT: state
;;;    OUTPUT: boolean
;;; CALLED BY: switch
;;;     CALLS: ---

(defun has-boat (side)
   (member 'b side) )
                                
;;;======================================================================
;;; MEMB-ALL -- DETERMINES WHETHER ALL 
;;;             MEMBERS OF list1 ARE MEMBERS OF list2
;;;     INPUT: list, list
;;;    OUTPUT: boolean
;;; CALLED BY: memb-all, switch
;;;     CALLS: memb-all, remove-one

(defun memb-all (list1 list2)
    (cond ((endp list1) t )
          ((member (first list1) list2) 
              (memb-all (rest list1) (remove-one (first list1) list2)) )
          ( t  nil ) ) )

;;;======================================================================   
;;; REMOVE-ONE:  REMOVE JUST ONE OCCURENCE OF element FROM list
;;;     INPUT: element, list
;;;    OUTPUT: list
;;; CALLED BY: remove-one, memb-all, left-equiv
;;;     CALLS: remove-one

(defun remove-one (element list)
    (cond ((null list) list)
          ((equal element (first list)) (rest list))  
          (  t  (cons (first list)(remove-one element (rest list)))) ) )
                  
;;;======================================================================
;;; REMOVE-ALL -- REMOVE EXACTLY list1 FROM list2.  
;;;           eg  (remove-all '(m c m) '(m c m c m c)) returns  '(c m c)
;;;     INPUT: list
;;;    OUTPUT: list
;;; CALLED BY: remove-all, switch, more-c-than-m
;;;     CALLS: remove-all, remove-one

(defun remove-all (list1 list2)
    (cond ((endp list1) list2 )
          (    t     (remove-all (rest list1) 
                                 (remove-one (first list1) list2))) ) )

;;;======================================================================
;;; REMOVE-BANNED: PRUNE THE LIST OF ALL POSSIBLE NEXT STATES FROM A list-of-states
;;;        BY REMOVING ALL STATES VIOLATING THE PROBLEM CONDITIONS.
;;;     INPUT: list
;;;    OUTPUT: list
;;; CALLED BY: prune, cross-from
;;;     CALLS: banned, prune, left-side. right-side

(defun remove-banned (next-states)
   (cond ((endp next-states) nil)
         ((banned (left-side(first next-states))) 
          (remove-banned (rest next-states)))
         ((banned (right-side(first next-states))) 
          (remove-banned (rest next-states)))
         ( t (cons(first next-states)(remove-banned(rest next-states))))))
                 
;;;======================================================================
;;; REMOVE-VISITED: PRUNE THE LIST OF ALL POSSIBLE NEXT STATES FROM A list-of-states
;;;        BY REMOVING ALL STATES VIOLATING THE PROBLEM CONDITIONS.
;;;     INPUT: list
;;;    OUTPUT: list
;;; CALLED BY: prune, cross-from
;;;     CALLS: banned, prune, left-side. right-side

(defun remove-visited (next-states states)
   (cond ((endp next-states) nil)
         ((member (first next-states) states :test #'state-equal)
	   (remove-visited (rest next-states) states))
	 (t (cons (first next-states)
		  (remove-visited (rest next-states) states)))))
                 
;;;======================================================================
;;; BANNED: DETERMINES WHETHER A side OF A STATE IS BANNED BY 
;;;         THE PROBLEM CONDTIONS
;;;     INPUT: side of state
;;;    OUTPUT: boolean
;;; CALLED BY: prune 
;;;     CALLS: more-m-than-c

(defun banned (side)
  (and (not(has-boat side))
       (has-missionary side)
       (has-more-c-than-m side) ) )

(defun has-missionary (side)
  (member 'm side))

;;;=================================================================       
;;; HAS-MORE-C-THAN-M: DETERMINES WHETHER A side OF A STATE HAS MORE
;;;                CANNIBALS (C) THAN MISSIONARIES (M)
;;;     INPUT: side of state
;;;    OUTPUT: boolean
;;; CALLED BY: more-c-than-m, remove-all, banned
;;;     CALLS: more-c-then-m, remove-all

(defun has-more-c-than-m (side)
  (< (missionary-count side)
     (cannibal-count side)))

(defun missionary-count (side)
(cond ((endp side) 0)
      ((eql 'm (first side)) (1+ (missionary-count (rest side))))
      ( t  (missionary-count (rest side)))))

(defun cannibal-count (side)
  (cond ((endp side) 0)
	((eql 'c (first side)) (1+ (cannibal-count (rest side))))
	( t  (cannibal-count (rest side)))))

;;;===================================================================
(defun state-equal (s1 s2)
  (side-equal (left-side s1)
	      (left-side s2)))

(defun side-equal (s1 s2)
  (cond((endp s1) (null s2))
       ((not(member(first s1) s2)) nil)
       (t (side-equal (rest s1)(remove-one(first s1) s2)))))

;;;======================================================================
;;; end-of-file
