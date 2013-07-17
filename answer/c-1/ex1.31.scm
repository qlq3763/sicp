(define (product term a next b)
  (if (> a b) 1
      (* (term a) 
	 (product term (next a) next b))))

(define (factorial n)
  (define (term a) a)
  (define (next a) (+ a 1))
  (product term 1 next n))

;; (define (pi-div-4 n)
;;   (define (next k) (+ k 1))
;;   (define (dividend)
;;     (define (term k)
;;       (if (odd? k) (+ k 1)
;; 	  (+ k 2)))
;;     (product term 1 next n))
;;   (define (divisor)
;;     (define (term k)
;;       (if (odd? k) (+ k 2)
;; 	  (+ k 1)))
;;     (product term 1 next n))
;;   (/ (dividend) (divisor)))

(define (pi-div-4 n)
  (define (next k) (+ k 1))
  (define (term k)
    (if (odd? k)
	(/ (+ k 1) (+ k 2)) ;; redundant
	(/ (+ k 2) (+ k 1))))
  (product term 1 next n))

(define (pi n)
  (* 4.0 (pi-div-4 n)))


(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))