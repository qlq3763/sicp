;; UNORDERED AND DUPLICATE

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
      (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))

(define set1 '(1 2 3))
(define set2 '(2 3 4))
(define set3 (adjoin-set 4 set2))
(newline)
(print set1)
(print set2)
(print set3)
(print (union-set set1 set2))
(print (intersection-set set1 set2))