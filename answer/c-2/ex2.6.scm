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

;; here one means call the give function once, two twiece, ...
(assert '(= ((one inc-2) 4) 6)) ;; +2
(assert '(= ((two inc-2) 4) 8)) ;; +2 twice
(assert '(= (((add one two) inc-2) 4) 10)) ;; three times

