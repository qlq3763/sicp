;; 1.
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; (a)
(define (harmonic n)
  (define (reciprocal n) (/ 1 n))
  (define (inc n) (+ n 1))
  (sum reciprocal 1 inc n))

;; (b)
(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (+ result (term a)))))
    (iter a 0))

(define (harmonic-i n)
  (define (reciprocal n) (/ 1 n))
  (define (inc n) (+ n 1))
  (sum-i reciprocal 1 inc n))

;; (c)
(display "output from problem 1:\n")
(display (harmonic 1))
(display "\n")
(display (harmonic 50))
(display "\n")
(display (harmonic 100))
(display "\n")
(display "\n")

(display (harmonic-i 1))
(display "\n")
(display (harmonic-i 50))
(display "\n")
(display (harmonic-i 100))
(display "\n")


;;;;;;;;;;;;;;;;;; 2.
;; (a)
(define (product term a next b)
  (if (> a b) 
      1
      (* (term a) 
         (product term (next a) next b))))

;; (b)
(define (product-i term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

;; (c)
(define (pi-approx n)
  (define (fourth-pi n)
    (define (next k) (+ k 1))
    (define (term k)
      (if (odd? k)
          (/ (+ k 1) (+ k 2)) 
          (/ (+ k 2) (+ k 1))))
    (let ((p1 (product term 1 next n))
          (p2 (product-i term 1 next n)))
      (if (= p1 p2) ;; verification
          p1
          non-exist))) ;; generate error when try to execute this expression  
  (* 4.0 (fourth-pi n))) ;; 4.0 instead of 4 to get float format result

;; (d)
(display "\n\noutput from problem 2:\n")
(display (pi-approx 1))
(display "\n")
(display (pi-approx 100))
(display "\n")
(display (pi-approx 1000))
(display "\n")

;; These tests demonstate that the approximation gets more accurate as n gets larger.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 3.
;; (a)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner 
			    null-value 
			    term 
			    (next a)
			    next
			    b))))

(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a)
	      (combiner (term a) result))))
  (iter a null-value))

;; (b)
(define (sum term a next b)
  ;; (display "new sum\n") ;; yep, new version does be called
  (let ((s1 (accumulate + 0 term a next b))
        (s2 (accumulate-i + 0 term a next b)))
    (if (= s1 s2) ;; verification
        s1
        non-exist)))

(define (product term a next b)
  (let ((p1 (accumulate * 1 term a next b))
        (p2 (accumulate-i * 1 term a next b)))
    (if (= p1 p2) 
        p1
        non-exist)))

;; (c)
(define (harmonic n)
  (define (reciprocal n) (/ 1 n))
  (define (inc n) (+ n 1))
  (sum reciprocal 1 inc n))

(define (pi-approx n)
  (define (fourth-pi n)
    (define (next k) (+ k 1))
    (define (term k)
      (if (odd? k)
          (/ (+ k 1) (+ k 2)) 
          (/ (+ k 2) (+ k 1))))
    (product term 1 next n))
  (* 4.0 (fourth-pi n)))

(display "output from problem 3:\n")
(display (harmonic 1))
(display "\n")
(display (harmonic 50))
(display "\n")
(display (harmonic 100))
(display "\n")
(display "\n")

(display (pi-approx 1))
(display "\n")
(display (pi-approx 100))
(display "\n")
(display (pi-approx 1000))
(display "\n")

;;;;;;;;;;;;;;;;;;;;;;;; 4.
;; (a)
(define (der f h)
  (lambda (x)
    (/ (- (f (+ x h)) (f x)) 
       h)))

;; (b)
(define pi 3.141592653589793)
(define h .5)
(display "\n\noutput from problem 4(b):\n")
(display (- ((der sin h) 0) (cos 0)))
(display "\n")

(define x (* pi 1/2))
(display (- ((der sin h) x) (cos x)))
(display "\n")

(define x (* pi 3/4))
(display (- ((der sin h) x) (cos x)))
(display "\n")

(display (- ((der sin h) pi) (cos pi)))
(display "\n")

;; (c)
(define (fun x)
  (+ (- (* 3 x x)
        (* 2 x))
     7))

(define (der-fun x)
  (- (* 6 x)
     2))
;; (define h 0.001)
(display "\n\noutput from problem 4(c):\n")
(display (- ((der fun h) 0) (der-fun 0)))
(display "\n")
(display (- ((der fun h) 4) (der-fun 4)))
(display "\n")
(display (- ((der fun h) 10) (der-fun 10)))
(display "\n")
(display (- ((der fun h) 100) (der-fun 100)))
(display "\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 5.
(define h 0.0001)
(define (newtons-method f x0 n)
  (if (<= n 0)
      x0
      (newtons-method f 
                      (- x0 (/ (f x0)
                               ((der f h) x0)))
                      (- n 1))))

;; (a)
(define (f x)
  (+ (* 2 x)
     1))
(display "\n\noutput from problem 5(a):\n")
(display (- (f (newtons-method f 4 10)) 0))
(display "\n")

;; (b)
(define (g x)
  (- (- (* x x)
        x)
     1))

(display "\n\noutput from problem 5(b):\n")
(display (- (g (newtons-method g 2 10)) 0))
(display "\n")

;; (c)
(define (sqrt-newt n)
  (define (f x)
    (- (* x x)
       n))
  (newtons-method f 1 40))

(display "\n\noutput from problem 5(c):\n")
(display (- (sqrt-newt 0) 0))
(display "\n")
(display (- (sqrt-newt 4) 2))
(display "\n")
(display (- (sqrt-newt 16) 4))
(display "\n")
(display (- (sqrt-newt 100) 10))
(display "\n")