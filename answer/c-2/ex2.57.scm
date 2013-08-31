(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; representing algebraic expressions

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; simplify number elements by computing one result element
;; instead of multiple
(define (simplify-nums op initial elem-list)
  (let ((nums (filter number? elem-list))
	(non-nums (filter (lambda (x) (not (number? x))) elem-list)))
    (let ((acc (accumulate op initial nums)))
      (if (not (= acc initial))
	  (cons acc non-nums)
	  non-nums))))

(define (one-elem? elem-list)
  (= (length elem-list) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sum
(define (make-sum . a)
  (let ((elems (simplify-nums + 0 a)))
    (cond ((null? elems)
	   0) ;; (make-sum)=>0
	  ((one-elem? elems)
	   (car elems))
	  (else (cons '+ elems)))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) 
  (nry s '+))

(assert '(equal? (make-sum 1 2 3 4) 10))
(assert '(equal? (make-sum) 0))
(assert '(equal? (make-sum 3) 3))
(assert '(equal? (make-sum 'x) 'x))
(assert '(equal? (make-sum 1 'x 2 3 'y) '(+ 6 x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; product
(define (make-product . a)
  (let ((elems (simplify-nums * 1 a)))
    (cond ((null? elems)
	   1) ;; (make-product)=>1
	  ((one-elem? elems)
	   (car elems))
	  (else (cons '* elems)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplicand p)
  (nry p '*))

(define (multiplier p) (cadr p))  

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(assert '(equal? (make-product) 1))
(assert '(equal? (make-product 'x) 'x))
(assert '(equal? (make-product 1 2 3 4) 24))
(assert '(equal? (make-product 1 2 3 'x 4 'y) '(* 24 x y)))

;; if seq is of the form (op, arg1, arg2), then return arg2.
;; if seq is of the from (op, arg1, arg2, ...argn), return (op arg2, ...argn).
(define (nry seq op)
  (let ((rest (cddr seq)))
    (if (= (length rest) 1)
	(car rest)
	(cons op rest))))

(assert '(equal? (deriv '(+ x x x x) 'x) 4))
(newline)
(print (deriv '(* x x x x) 'x))

;; Actually, I think the first parameter to deriv should be constructed
;; (only) from make-sum, make-product. '(+ x x x) actually is making 
;; assumption about how sum is represented. It assumes there is list,
;; but how about sum being represented by (cons + (cons ...))?
;; Another point: this is white box test. Still I don't think directly
;; giving list as arguemnt is a good idea. Suppose the representation
;; of sum has changed, then the tests need to change too.
(assert '(equal? (deriv (make-sum) 'x) 0))
(assert '(equal? (deriv (make-sum 'x) 'x) 1))
;; (assert '(equal? (deriv '(+ x) 'x) 1)) ;; augend will fail