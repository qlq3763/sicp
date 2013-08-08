;; display the content following by a new line
(define (print content)
  (display content)
  (display "\n")
  true) ;; just don't like "unspecified return value"

;; simple stupid implemetation of assert
(define (assert exp)
  (if (not (eval exp user-initial-environment))
	  (begin (print "\n assertion failure:")
			 (print exp)
			 (abort))
	  true))

;; should use this one to test equality of float numbers
(define (float=? f1 f2)
  (< (abs (- f1 f2)) 0.00001))

;; small but frequently used procedures
(define (identity x) x)

(define (cube x) (* x x x))

(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (average x y) (/ (+ x y) 2))

;; accumulator
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

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (filtered-accumulate filter combiner null-value
			     term a next b)
  (define (iter a result)
    (cond ((> a b) result)
	  ((if (filter a)
	       (iter (next a) (combiner (term a) result))
	       (iter (next a) result)))))
  (iter a null-value))

;; prime?
(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))

  (define (divides? a b)
    (= (remainder b a) 0))

  (and (> n 1)
       (= n (smallest-divisor n))))

;; fixed-point
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
