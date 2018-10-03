(defun fibonacci-TR (n)
  "Returns the nth term in the Fibonacci sequence, computed with tail-recursion"
  (labels ((fib-aux (n f1 f2)
                    (if (zerop n) f1 (fib-aux (1- n) f2 (+ f1 f2)))))
          (fib-aux n 0 1)))
