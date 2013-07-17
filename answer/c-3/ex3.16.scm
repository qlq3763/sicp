(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; also see pic
(define n-3 (list 1 2 3))
(print n-3)
(print (count-pairs n-3))

(print "***********")

(define x (list 1))
(define n-4 (list x x))
(print n-4)
(print (count-pairs n-4))

(print "***********")

(define x1 (list 1))
(define x2 (cons x1 x1))
(define n-7 (cons x2 x2))
(print n-7)
(print (count-pairs n-7))

(print "***********")
;; use make circle from ex3.13

(define n-never (make-cycle (list 1 2 3)))
;; (print n-never)
;; (print (count-pair n-never))

(print "***********")