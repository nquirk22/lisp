;;; MISSIONARIES AND CANNIBALS
;;; DATA STRUCTURE 2  

;;; A state of the river is represented as, eg,: (b m c r m m c c)
;;;
;;;   AUTOLOAD THE MAIN FILE AT THE END OF THIS FILE
;;;
;;; to run, eval (cross)
;;; CL-USER> (cross)
;;; 
;;; ((B M C M C M C R) (C M C M R B M C) (B M C M M C R C) (C M M R B M C C)
;;;  (B M M M C R C C) (C M R B M C M C) (B M M C C R M C) (C C R B M M M C)
;;;  (B M C C R M M C) (C R B M M M C C) (B M C R M M C C) (R B M M M C C C)) 
;;; NIL
;;; CL-USER> 


;;;======================================================================
;;; LEFT-SIDE:  find the left side of a node
;;;     INPUT: state
;;;    OUTPUT: a side of a state
;;; CALLED BY: switch, prune, is-in, is-end-state
;;;     CALLS: ---

(defun left-side (state)
   (rest (member 'r (reverse state))) )

;;;======================================================================
;;; RIGHT-SIDE:  find the left side of a node
;;;     INPUT: state
;;;    OUTPUT: a side of a state
;;; CALLED BY: switch, prune.
;;;     CALLS: ---

(defun right-side (state)
   (rest (member 'r state) ) )

;;;======================================================================
;;;  MAKE-STATE:  makes a state, given the left and right sides
;;;     INPUT: two sides of a state
;;;    OUTPUT: state
;;; CALLED BY: start-state, end-state, switch
;;;     CALLS: ---

(defun make-state (left-side  right-side)
      (append left-side '(r) right-side) )

;;;======================================================================
;;;   LOAD THE MAIN FILE

(load 
 "/home/wyatt/Classes/CSC345/Lisp/Sample-programs/miss-cann-main.lisp")

;;;======================================================================
