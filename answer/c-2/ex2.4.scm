(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

(define (my-cdr z)
  (z (lambda (p q) q)))

(define z (my-cons 4 14))
(assert '(= (my-car z) 4))
(assert '(= (my-cdr z) 14))

;; proof, substitution model
;; (my-car z) =>
;; (my-car (my-cons 4 14)) =>
;; (my-car (lambda (m) (m 4 14))) =>
;; ((lambda (m) (m 4 14)) (lambda (p q) p)) =>
;; (lambda (4 14) 4) =>
;; 4