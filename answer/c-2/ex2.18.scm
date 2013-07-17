g(define (my-reverse l)
  (if (null? l)
      l
      (append (my-reverse (cdr l))
	      (list (car l)))))

(newline)
(display l-1)

(newline)
(display (my-reverse l-1))