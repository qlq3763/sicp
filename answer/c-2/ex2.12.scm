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
(print-interval (div-interval i-4 i-4)) ; used for ex2.14

;; the following is used for ex2.15
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define par-i-1 (par1 i-4 i-4))
(display "\n use par1")
(print-interval par-i-1)
(newline)
(display (percent par-i-1))

(define par-i-2 (par2 i-4 i-4))
(display "\n use par2")
(print-interval par-i-2)
(newline)
(display (percent par-i-2))

;; The following is fro ex2.16
(display "\n\nex2.16***************")
(print-interval (sub-interval (add-interval i-4 i-4) i-4))
(newline)

(print-interval (sub-interval i-4 i-4))
(newline)

(print-interval (div-interval i-4 i-4))
(newline)

;; (define i (make-interval 4 14)) ;; defined in ex2.7.scm
;; (define i-2 (make-interval 0 4)) ;; defined in ex2.8.scm
(assert '(interval=? (add-interval i i-4)
					 (add-interval i-4 i)))

(assert '(interval=? (mul-interval i i-4)
					 (mul-interval i-4 i)))

(assert '(interval=? (add-interval i (add-interval i-2 i-4))
					 (add-interval i-2 (add-interval i-4 i))))

(assert '(interval=? (mul-interval i (mul-interval i-2 i-4))
					 (mul-interval i-2 (mul-interval i-4 i))))

(assert '(interval=? (mul-interval i (add-interval i-2 i-4))
					 (add-interval (mul-interval i i-2)
								   (mul-interval i i-4))))

