(defun myLast (L)
  "Returns the last item in a list"
  (if (eql (rest L) nil) (first L) (myLast (rest L))))
