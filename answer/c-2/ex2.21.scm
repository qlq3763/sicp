(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (square-list (cdr items)))))

(define (square-list-2 items)
  (map square items))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
	    (map proc (cdr items)))))


(define l-3 (list 1 2 3 4 14))
(define l-4 (list -1 1 4 14 -4))

(assert '(list=? (square-list l-3)
				 '(1 4 9 16 196)
				 =))
(assert '(list=? (square-list l-4)
				 '(1 1 16 196 16)
				 =))

(assert '(list=? (square-list-2 l-3)
				 '(1 4 9 16 196)
				 =))
(assert '(list=? (square-list-2 l-4)
				 '(1 1 16 196 16)
				 =))