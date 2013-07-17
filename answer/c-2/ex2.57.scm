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

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))


;; (print (deriv '(+ x 3) 'x))
;; (print (deriv '(* x y) 'x))
;; (print (deriv '(* (* x y) (+ x 3)) 'x))


;; With simplification

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-sum a1 . a2)
  (cond ((< (length a2) 1)
	 (print 'wrong))
	((= (length a2) 1)
	 (list '+ a1 (car a2)))
	(else (list '+ a1 (make-sum a2)))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


;; (print (deriv '(+ x 3) 'x))
;; (print (deriv '(* x y) 'x))
;; (print (deriv '(* (* x y) (+ x 3)) 'x))

(define (make-exponentiation u n)
  (cond ((= n 0)
	 1)
	((= n 1)
	 u)
	(else (list '** u n))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
	 ;(print exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
	((exponentiation? exp) ;; add here
	 (make-product (make-product (exponent exp) 
				     (make-exponentiation (base exp) 
							  (- (exponent exp) 1)))
		       (deriv (base exp) var))) ;; end add here
        (else
         (error "unknown expression type -- DERIV" exp))))

;; (print (deriv '(* 4 (** x 0)) 'x))
;; (print (deriv '(+ 4 (* 4 (** x 1))) 'x))
;; (print (deriv '(+ (* 4 x) (** x 4)) 'x))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

;; (define (make-sum a1 . a2)
;;   (define (nest-sum addend augend)
;;     ;(print augend)
;;     (cond ((< (length augend) 1)
;; 	   (print 'wrong))
;; 	  ((= (length augend) 1)
;; 	   (list '+ addend (car augend)))
;; 	  (else (list '+ addend (nest-sum (car augend) (cdr augend))))))
;;   (nest-sum a1 a2))

(define (addend s) (cadr s))


(define (augend s) 
  (nry s '+))

(define (multiplicand p)
  (nry p '*))
  
  ;(caddr p))

;; if seq is of the form (op, arg1, arg2), then return arg2.
;; if seq is of the from (op, arg1, arg2, ...argn), return (op arg2, ...argn).
(define (nry seq op)
  (let ((rest (cddr seq)))
    (if (= (length rest) 1)
	(car rest)
	(cons op rest))))

(print (deriv '(+ x x x x) 'x))
(print (deriv '(* x x x x) 'x))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

;; (define add_four_elem (make-sum 1 2 3 4))

;; (print add_four_elem)
;; (print (addend add_four_elem))
;; (print (augend add_four_elem))
