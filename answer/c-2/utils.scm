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

;; use the hint: ab^n as an invariant
(define (fast-expt-iter b n)
  (define (iter b n a)
    (cond ((= n 0) a)
	  ((even? n) (iter (square b) 
			   (/ n 2) 
			   a))
	  (else (iter b 
		      (- n 1)
		      (* a b)))))
  (iter b n 1))

;; 
(define nil '())

;;
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;; 

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
