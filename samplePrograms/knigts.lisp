;;; SOLVE THE KNIGHTS TOUR PROBLEM
;;;
;;; Runs under CMUL -  Sept 10, 2012 - RWW
;;; minor fixes - Sept 10, 2012 - RWW
;;;=================================================================

(defvar count)


;;; THE MAIN TOP-LEVEL FUNCTION
(defun knt (n) 
  "Solve solve the knights tour problem on an N by N board"
  (setf count 1)
  (knights '((1 1)) n)) ;; start at square (1 1)

;;;--------------------------------------------------------

(defun 2+ (n) (+ n 2))
(defun 2- (n) (- n 2))

(defun make-location (x y) 
  (list x y))


(defun next-8-moves (location) 
  (let ((x (first location))
	(y (second location)))
    (list (list (1+ x)(- y 2))
	  (list (+ x 2)(1- y))
	  (list (+ x 2)(1+ y))
	  (list (1+ x)(+ y 2))
	  (list (1- x)(+ y 2))
	  (list (- x 2)(1+ y))
	  (list (- x 2)(1- y))
	  (list (1- x)(- y 2)))))
	  
(defun remove-impossibles (locs n) 
  (cond ((endp locs) nil)
	((impossible (first locs) n) (remove-impossibles (rest locs) n))
	( t (cons (first locs)(remove-impossibles (rest locs) n)))))

(defun impossible (loc n) 
  (or (< (x-coord loc) 1)
      (< (y-coord loc) 1)
      (> (x-coord loc) n)
      (> (y-coord loc) n)))

(defun x-coord (loc) (first loc))
(defun y-coord (loc) (second loc))

(defun next-moves (loc n)
 (remove-impossibles (next-8-moves loc) n))

(defun knights (board n) 
  (cond ((board-full board n) (print-board (reverse board) n))
	((null(remove-visited(next-moves(current-loc board) n)
			     board))
	 nil)
	( t (many-knights (remove-visited(next-moves(current-loc
						     board) n)
					 board)
			  board n))))

(defun current-loc (board)
  (first board))

(defun remove-visited (nexts board) 
  (cond ((endp nexts) nil)
	((member (first nexts) board :test (function equal)) 
	 (remove-visited (rest nexts) board))
	( t (cons (first nexts) (remove-visited (rest nexts) board))) ) )

(defun many-knights (next-moves board n) 
  (cond ((endp next-moves) nil)
	( t (knights (cons (first next-moves) board) n)
	    (many-knights (rest next-moves) board n))))

(defun board-full (board n) 
  (= (length board) (* n n)))

;;;*****************************************************************
;;; PRINTING A BOARD -- its as much work as finding the solution

(defun number-moves (board number) 
  (cond ((endp board) nil)
	( t (cons (number-a-move(first board) number)
		  (number-moves (rest board) (1+ number))))))

;;;===================================================================
(defun number-a-move (square number)
  (append square (list number)))

;;;===================================================================
;;; sort the positions with the added move numbers

(defun sort-board (board)
  (sort board #'square-less))

;;;===================================================================
;;; determine whether square sq1 occurs earlier than square sq2

(defun square-less (sq1 sq2) 
  (cond ((< (first sq1)(first sq2)) t)
	(( and (= (first sq1)(first sq2))(< (second sq1)(second sq2))) t)
	( t nil )))

;;;===================================================================

(defun top-of-board (n)    
  (format t "+")
  (n-dashes (1- (* 3 n)))
  (format t "+~%"))

(defun bottom-of-board (n)
  (top-of-board n))

(defun vertical-stroke() 
  "Draw a vertical stroke: | "
  (format t "|"))

(defun n-dashes (n)
  "Draw a line of n dahes: --------- "
  (cond ((= n 0) nil)
	(t (princ "-")
	   (n-dashes (1- n)))))

;;;*****************************************************************
(defun print-board (board n)
  (format t "   SOLUTION  ~a~%" count)
  (setf count (1+ count))
  (print-top n)
  (print-lines (sort-board (number-moves board 1)) n 1)
  (print-bottom n)
  (newline)
  )

(defun print-top (n)    
  " Draw a line like: +----------------+ "
  (format t "+")
  (n-dashes (1- (* 3 n)))
  (format t "+~%"))

(defun print-bottom (n)
  (print-top n))

(defun print-lines (board n numb-printed)
  (cond ((endp board) (vertical-stroke)   ; end of board
		      (newline))
	((> numb-printed n)  (vertical-stroke)
			     (newline)
			     (print-lines board n 1))
	( t  (print-move-number (first board))
	     (print-lines (rest board) n (1+ numb-printed)))))
	  

(defun print-move-number (m)
  (vertical-stroke)
  (if (< (third m) 10)(format t " "))
  (format t "~a" (third m)))


(defun newline ()
  (format t "~%"))

;;; END OF FILE
;;;--------------------------------------------------------------
