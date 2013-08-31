(define (my-reverse l)
  (if (null? l)
      l
      (append (my-reverse (cdr l))
	      (list (car l)))))

(define (my-reverse-v2 l)
  (define (iter l rst)
	(if (null? l)
		rst
		(iter (cdr l) (cons (car l) rst))))
  (iter l '()))

(newline)
(display l-1)
(newline)
(display (my-reverse l-1))

(newline)
(display l-1)
(newline)
(display (my-reverse-v2 l-1))