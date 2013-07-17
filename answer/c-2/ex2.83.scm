;; I will use scheme-numbe as Integer and ignore Real
;; So, the hierarchy is: scheme-number -> rational number -> complex number

;; add to scheme-number-package
;; (define (integer->rational n)
;;   (make-rational n 1))

;; (put 'raise '(integer) integer->rational)

;; add to rational-package
;; (define (rational->complex r)
;;   (make-complex-from-real-imag (/ (numer r) (denom r)) 0))

;; (put 'raise '(rational) rational->complex)

(define (raise x)
  (apply-generic 'raise x))

(define s-1 (make-scheme-number 0))
(print (raise s-1))
(print (raise 14))

(define r-1 (make-rational 1 4))
(print (raise r-1))

(define z-1 (make-complex-from-real-imag 0 0))
;; (print (raise z-1))