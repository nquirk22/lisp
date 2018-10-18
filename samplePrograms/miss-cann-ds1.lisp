;;; MISSIONARIES AND CANNIBALS
;;; DATA STRUCTURE 1 


;;; A state of the river is represented as, eg,: ((b m c) (m m c c))
;;;
;;;   AUTOLOAD THE MAIN FILE AT THE END OF THIS FILE

;;; to run, eval (cross)
;;; CL-USER> (cross)
;;; ;; Loading #P"/home/wyatt/Classes/CSC345/Lisp/miss-cann-main.lisp".

;;; (((B M C C M M C) NIL) ((C M M C) (B M C)) ((B M C M M C) (C))
;;;  ((M M C) (B M C C)) ((B M M M C) (C C)) ((M C) (B M C M C))
;;;  ((B M M C C) (M C)) ((C C) (B M M M C)) ((B M C C) (M M C))
;;;  ((C) (B M M M C C)) ((B M C) (M M C C)) (NIL (B M M M C C C))) 
;;; NIL
;;; CL-USER> 


;;;======================================================================
;;; LEFT-SIDE:  find the left side of a state
;;;     INPUT: state
;;;    OUTPUT: a side of a state
;;; CALLED BY: switch, prune, is-in, is-end-state
;;;     CALLS: ---

(defun left-side (state)
   (first state) )  

;;;======================================================================
;;; RIGHT-SIDE:  find the right side of a state
;;;     INPUT: state
;;;    OUTPUT: side of a state
;;; CALLED BY: switch, prune.
;;;     CALLS: ---

(defun right-side (state)
   (second state) )

;;;======================================================================
;;;  MAKE-STATE:  makes a state, given the left and right sides
;;;     INPUT: two sides of a state
;;;    OUTPUT: state
;;; CALLED BY: start-state, end-state, switch
;;;     CALLS: ---

(defun make-state (left-side  right-side)
      (list left-side right-side) )


;;;======================================================================
;;;   AUTOLOAD THE MAIN FILE

(load 
 "/home/wyatt/Classes/CSC345/Lisp/Sample-programs/miss-cann-main.lisp")


;;;**********************************************************************
