(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add m n)
  (lambda (f) 
    (lambda (x) ((m f) ((n f) x)))))

(define (inc-2 n)
  (+ n 2))

(define test-one (add-1 zero))
(define test-two (add-1 test-one))
(define test-three (add-1 test-two))
(define test-four (add-1 test-three))

(newline)
(display ((zero inc-2) 4))

(newline)
(display ((test-one inc-2) 4))

(newline)
(display ((test-two inc-2) 4))

(newline)
(display ((test-three inc-2) 4))

(newline)
(display ((test-four inc-2) 4))

(newline)
(display "test over, real code begin")

(newline)
(display ((one inc-2) 4))

(newline)
(display ((two inc-2) 4))

(newline)
(display (((add test-four two) inc-2) 4))

