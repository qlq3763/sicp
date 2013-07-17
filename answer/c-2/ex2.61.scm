;; ORDERED

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; Exercise 2.61
(define (adjoin-set x set)
  (let ((smallest (car set)))
    (cond ((= x smallest)
	   set)
	  ((< x smallest)
	   (cons x set))
	  (else (cons smallest (adjoin-set x (cdr set)))))))

;; Exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1)
	 set2)
	((null? set2)
	 set1)
	(else (let ((x1 (car set1))
		    (x2 (car set2)))
		(cond ((= x1 x2)
		       (cons x1 (union-set (cdr set1) (cdr set2))))
		      ((< x1 x2)
		       (cons x1 (union-set (cdr set1) set2)))
		      (else cons x2 (union-set set1 (cdr set2))))))))
	
(define set1 '(1 2 3))
(define set2 '(2 4 5))
(define set3 (adjoin-set 3 set2))

(print set1)
(print set2)
(print set3)

(print (union-set set1 set2))
(print (union-set set1 set3))
(print (union-set set2 set3))