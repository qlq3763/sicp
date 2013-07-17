(define (last-pair l)
  (define (last-element e l)
    (if (null? l)
	e
	(last-element (car l) (cdr l))))
  (if (null? l)
      (display "empty list")
      (last-element (car l) (cdr l))))

(define (last-pair-2 l)
  (define (one-element-list? l)
    (null? (cdr l)))
  (if (one-element-list? l)
      (car l)
      (last-pair-2 (cdr l))))

(define l-1 (list 1 2 3 4 14))
(define l-2 (list 14 4 3 2 -4))

(newline)
(display (last-pair l-1))

(newline)
(display (last-pair l-2))

(newline)
(display (last-pair-2 l-1))

(newline)
(display (last-pair-2 l-2))