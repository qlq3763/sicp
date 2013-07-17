(define (make-rat n d)
  (if (> d 0)
      (cons n d)
      (cons (- n) (- d))))

(define (numer x) (car x))

(define (denom x) (cdr x))

;;footnote -- alternative definitions
;; (define make-rat cons)
;; (define numer car)
;; (define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))