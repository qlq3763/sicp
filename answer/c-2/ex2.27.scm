(define (my-reverse l)
  (if (null? l)
      l
      (append (my-reverse (cdr l))
	      (list (car l)))))

(define (my-deep-reverse l)
    (cond ((null? l)
	 l)
	((not (pair? (car l)))
	 (append (my-deep-reverse (cdr l))
		 (list (car l))))
	(else (append (my-deep-reverse (cdr l))
		      (list (my-deep-reverse (car l)))))))

(define (my-deep-reverse-2 l)
  (if (not (pair? l))
      l
      (append (my-deep-reverse-2 (cdr l))
	      (list (my-deep-reverse-2 (car l))))))

(define x (list (list 1 2) (list 3 4)))

(newline)
(display x)

(newline)
(display (my-reverse x))

(newline)
(display (my-deep-reverse x))

(newline)
(display (my-deep-reverse-2 x))