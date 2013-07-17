(define (same-parity . l)
  (define (test-rem-append rem element l)
    (if (= (remainder element 2) rem)
	(append l (list element))
	l))

  (define (parity-even-odd-list rem l rst)
    (if (null? l)
	rst
	(parity-even-odd-list rem 
			      (cdr l) 
			      (test-rem-append rem (car l) rst))))

  (if (null? l)
      l
      (parity-even-odd-list (remainder (car l) 2)
			    l
			    '())))

(define (same-parity-2 x . l)
  (define (inner x l)
    (let ((rem (remainder x 2)))
      (cond ((null? l)
	     l)
	    ((= (remainder (car l) 2) rem)
	     (cons (car l) (inner x (cdr l))))
	    (else (inner x (cdr l))))))
  (cons x (inner x l)))

(newline)
(display (same-parity 1 2 3 4 5 6 7))

(newline)
(display (same-parity 2 3 4 5 6 7))

(newline)
(display (same-parity-2 1 2 3 4 5 6 7))

(newline)
(display (same-parity-2 2 3 4 5 6 7))