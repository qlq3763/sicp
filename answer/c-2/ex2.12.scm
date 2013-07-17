(define (make-center-percent c p)
  (make-center-width c (/ (* c p) 100.0)))

(define (percent i)
  (/ (* (width i) 100.0) (center i)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define i-4 (make-center-percent 4 5))
(newline)
(print-interval i-4)

(newline)
(display (center i-4))

(newline)
(display (percent i-4))

(newline)
(display (percent (mul-interval i-4 i-4))) ; used to test proof in ex2.13

(newline)
(display (percent (improved-mul-interval i-4 i-4))) ; ditto

(newline)
(print-interval (div-interval i-4 i-4))