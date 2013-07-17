(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (sub-vec) (dot-product sub-vec v))
       m))

(define (transpose mat)
  (accumulate-n cons 
		'()
		mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (sub-vec) (matrix-*-vector cols sub-vec)) 
	 m)))

(define v-1 '(1 2 3))
(define v-2 '(4 5 6))
(define v-3 '(6 7 8))
(define m-1 (list v-1 v-2 v-3))
(define m-2 (list v-2 v-3 v-1))

(define (print v)
  (newline)
  (display v))

(print v-1)
(print m-1)

(print (dot-product v-1 v-3))
(print (matrix-*-vector m-1 v-1))
(print (transpose m-1))
(print (matrix-*-matrix m-1 m-2))