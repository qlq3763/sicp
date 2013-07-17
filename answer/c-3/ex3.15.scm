(define x (list 'a 'b))
(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(print z1)
(set-to-wow! z1)
(print z1)

(print z2)
(set-to-wow! z2)
(print z2)

(print x)

(print (eq? (car z1) (cdr z1)))
(print (eq? (car z2) (cdr z2)))