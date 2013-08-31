(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp) ; must come before product?
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

;; a.
;; (define (make-sum a1 a2)
;;   (cond ((=number? a1 0) a2)
;;         ((=number? a2 0) a1)
;;         ((and (number? a1) (number? a2)) (+ a1 a2))
;;         (else (list a1 '+ a2))))

;; (define (=number? exp num)
;;   (and (number? exp) (= exp num)))

;; (define (make-product m1 m2)
;;   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;;         ((=number? m1 1) m2)
;;         ((=number? m2 1) m1)
;;         ((and (number? m1) (number? m2)) (* m1 m2))
;;         (else (list m1 '* m2))))

;; (define (sum? x)
;;   (and (pair? x) (> (length x) 1) (eq? (cadr x) '+)))

;; (define (addend s) (car s))

;; (define (augend s) (caddr s))

;; (define (product? x)
;;   (and (pair? x) (> (length x) 1) (eq? (cadr x) '*)))

;; (define (multiplier p) (car p))

;; (define (multiplicand p) (caddr p))

;; ;; (x + (3 * (x + (y + 2))))
;; (define exp (make-sum 'x
;; 					  (make-product 3
;; 									(make-sum 'x
;; 											  (make-sum 'y 2)))))
;; (assert '(= (deriv exp 'x) 4))
;; (assert '(= (deriv exp 'y) 3))

;; b

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

;; insert op between any two elements
(define (insert-op op elem-list)
  (define (iter lst rst)
	(if (= (length lst) 1)
		(append rst lst)
		(iter (cdr lst)
			  (append rst (cons (car lst) (list op))))))
  (iter elem-list '()))

;; sum
(define (make-sum . a)
  (let ((elems (simplify-nums + 0 a)))
    (cond ((null? elems)
		   0) ;; (make-sum)=>0
		  ((one-elem? elems)
		   (car elems))
		  (else (insert-op '+ elems)))))

(define (sum? x)
  (and (pair? x) (list? (memq '+ x))))

(define (addend s) 
  (define (collect-until-plus seq result)
    (if (and (symbol? (car seq)) (eq? (car seq) '+))
		result
		(collect-until-plus (cdr seq) 
							(append result (list (car seq))))))

  (let ((seq-before-plus (collect-until-plus s '())))
    (if (= (length seq-before-plus) 1) ;; single element
		(car seq-before-plus)
		seq-before-plus)))

(define (augend s) 
  (let ((seq-after-plus (cdr (memq '+ s))))
    (if (= (length seq-after-plus) 1) ;; single element
	(car seq-after-plus)
	seq-after-plus)))

;; simple tests
(define s1 (make-sum 1 2 3 'x 4))
(define s2 (make-sum 1 'x 'y 3))

(assert '(equal? (make-sum 1 2 3 4) 10))
(assert '(equal? (make-sum 'x) 'x))
(assert '(equal?  s1 '(10 + x)))
(assert '(equal?  s2 '(4 + x + y)))
(assert '(equal? (make-sum 0 'x 0 0 'z) '(x + z)))

(assert '(equal? (addend s1) 10))
(assert '(equal? (augend s1) 'x))
(assert '(equal? (addend s2) 4))
(assert '(equal? (augend s2) '(x + y)))

;; product
;; should also make changes to ex2.57
;; just leave as it is, as a remainder: test cases should cover
;; at least every possible condition
(define (make-product . a)
  (let ((elems (simplify-nums * 1 a)))
    (cond ((null? elems)
		   1) ;; (make-product)=>1
		  ((not (false? (memq 0 elems))) ;; add 
		   0)
		  ((one-elem? elems)
		   (car elems))
		  (else (insert-op '* elems)))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) 
  (define (collect lst rst)
	(cond ((null? lst)
		   rst)
		  ((and (symbol? (car lst)) (eq? (car lst) '*))
		   (collect (cddr lst)
					(append rst (list (cadr lst)))))
		  (else rst)))
  (let ((rest (insert-op '* (collect (cdr p) '()))))
	(if (= (length rest) 1)
		(car rest)
		rest)))
		  

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; simple tests
(define p1 (make-product 1 2 3 'x 4))
(define p2 (make-product 1 'x 'y 3))

(assert '(equal? (make-product 1 2 3 4) 24))
(assert '(equal? (make-product 'x) 'x))
(assert '(equal? (make-product 'x 1 1 1 'y) '(x * y)))
(assert '(equal?  p1 '(24 * x)))
(assert '(equal?  p2 '(3 * x * y)))

(assert '(equal? (multiplier p1) 24))
(assert '(equal? (multiplicand p1) 'x))
(assert '(equal? (multiplier p2) 3))
(assert '(equal? (multiplicand p2) '(x * y)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(assert '(equal? (deriv '(x + x + (x + y + x)) 'x) 4))
(assert '(equal? (deriv '(x + (3 * (x + (y + 2)))) 'x) 4))
(assert '(equal? (deriv '(x + (3 * (x + (y + 2)))) 'y) 3))
(assert '(equal? (deriv '(x + 3 * (x + y + 2)) 'x) 4))
(assert '(equal? (deriv '(x * y * z) 'x) '(y * z)))
(assert '(equal? (deriv '(3 * x + 2 * x) 'x) 5))
(assert '(equal? (deriv '(x + (3 * x + y) * 2) 'x) 7))

(newline)
(print (deriv '(3 * x * z * m + x) 'x))
(print (deriv '((3 * x + y) * x + y * x) 'x))

;; Another problem:
;; for ex2.57, ex2.58.
;; Does make-sum, make-product really need to be changed to accept
;; more than two arguments?
;; Two sides: 
;; advantage of change: consistent with the list representation, conceptual consistance

;; disadvantage of change: need more work.

;; Actually, the list representation of expression is just a language that
;; we need to interpret, we don't need to create expression that way. As 
;; we know, as far as deriv is corcened, make-sum(make-product) just take
;; two arguments. 

;; As for this form: (deriv (make-sum 1 'x 'y 'x)), we don't care what 
;; representation make-sum used to keep all this information. The only
;; condition is that addend and augend can retrieve this information
;; properly. Then problem for ex2.57 an ex2.58 becomes: we just need 
;; a two argument make-sum and make-product, which are simple to implement. Plus these, what we need is a more complex way to interpret another
;; list representation.

;; Or we could think it another way: those list representaions are produced by make-sum and make-product, then we need to implement arbitrary-argument make-sum, make-product. As I said in ex2.57, argument passed
;; to deriv should be constructed from make-sum and make-product

;; Above is some thoughts about those two exercises, I will still
;; implement the arbitrary-argument version.
