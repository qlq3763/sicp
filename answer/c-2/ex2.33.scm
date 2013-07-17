(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
	      '()
	      sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y)
		(+ y 1))
	      0 
	      sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define l-1 (list 4 3 2 1))
(define l-2 (list 1 2 3 4))
(define l-3 (list l-1 l-2))

(newline)
(display (my-map square l-1))

(newline)
(display (my-append l-1 l-2))

(newline)
(display (my-length l-1))

(newline)
(display (my-length l-3))