
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


;: (deriv '(+ x 3) 'x)
;: (deriv '(* x y) 'x)
;: (deriv '(* (* x y) (+ x 3)) 'x)


;; With simplification

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


;: (deriv '(+ x 3) 'x)
;: (deriv '(* x y) 'x)
;: (deriv '(* (* x y) (+ x 3)) 'x)

;; a
;; (define (make-sum a1 a2) (list '+ a1 a2))

;; (define (make-product m1 m2) (list '* m1 m2))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

;; (print (deriv '(x + (3 * (x + (y + 2)))) 'x))
;; (print (deriv '(x + (3 * (x + (y + 2)))) 'y))

;; b
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum? x)
  (and (pair? x) (list? (memq '+ x))))

(define (addend s) 
  (define (collect-until-plus seq result)
    (if (and (symbol? (car seq)) (eq? (car seq) '+))
	result
	(collect-until-plus (cdr seq) 
			    (append result (list (car seq))))))

  (let ((seq-before-plus (collect-until-plus s '())))
    (if (= (length seq-before-plus) 1)
	(car seq-before-plus)
	seq-before-plus)))

(define (augend s) 
  (let ((seq-after-plus (cdr (memq '+ s))))
    (if (= (length seq-after-plus) 1)
	(car seq-after-plus)
	seq-after-plus)))

;; (print (sum? '(3 * x + 2 * x)))
;; (print (addend '(3 * x + 2 * x)))
;; (print (addend '((3 * 4) + x)))
;; (print (addend '(x + 3 + 6 + 9)))
;; (print (augend '(3 * x + 2 * x)))
;; (print (augend '((3 * 4) + x)))
;; (print (augend '(x + 3 + 6 + 9)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

;; (print (deriv '(x + x + (x + y + x)) 'x))
;; (print (deriv '(x + (3 * (x + (y + 2)))) 'x))
;; (print (deriv '(x + (3 * (x + (y + 2)))) 'y))
(print (deriv '(x + (3 * (x + y + 2))) 'x))
(print (deriv '(3 * x * z * m + x) 'x))
(print (deriv '(3 * x + 2 * x) 'x))
(print (deriv '(x + (3 * x + y) * 2) 'x))
(print (deriv '((3 * x + y) * x + y * x) 'x))