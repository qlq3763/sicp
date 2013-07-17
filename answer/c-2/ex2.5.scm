(define (my-cons a b)
  (* (fast-expt-iter 2 a)
     (fast-expt-iter 3 b)))

(define (my-car z)
  (if (= (remainder z 2) 0)
      (+ 1 (my-car (/ z 2)))
      0))

(define (my-cdr z)
  (if (= (remainder z 3) 0)
      (+ 1 (my-cdr (/ z 3)))
      0))

(define z (my-cons 14 4))

(newline)
(display (my-car z))

(newline)
(display (my-cdr z))