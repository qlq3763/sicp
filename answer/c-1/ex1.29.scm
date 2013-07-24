(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(define (s-intg f a b n)
  (define h (/ (- b a) n))
  (define (term k) 
    (define f-kh (f (+ a (* k h))))
    (cond ((or (= k 0) (= k n)) f-kh)
	  ((odd? k) (* 4 f-kh))
	  (else (* 2 f-kh))))
  (define (next k) (+ k 1))
  ( * (/ h 3.0)
      (sum term 0 next n)))

(print "\n")
(print (integral cube 0 1 0.01))
(print (s-intg cube 0 1 100))

(print "\n\n")
(print (integral cube 0 1 0.001))
(print (s-intg cube 0 1 1000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "\n")
(print (integral cube 1 2 0.01))
(print (s-intg cube 1 2 100))

(print "\n\n")
(print (integral cube 1 2 0.001))
(print (s-intg cube 1 2 1000))
