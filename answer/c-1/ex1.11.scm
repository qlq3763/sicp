(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
	 (* 2 (f-rec (- n 2)))
	 (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (iter f-n-1 f-n-2 f-n-3 counter)
    (if (<= counter 0)
	f-n-1
	(iter (+ f-n-1 (* 2 f-n-2) (* 3 f-n-3))
	      f-n-1
	      f-n-2
	      (- counter 1))))
  (if (< n 3)
      n
      (iter 2 1 0 (- n 2))))


(define (f-iter-2 n)
  (define (fn a b c) (+ a (* 2 b) (* 3 c)))

  (define (get-init n base)
    (+ (- n (floor n))
       base))

  (define (iter f-n-1 f-n-2 f-n-3 counter)   
    (if (< counter 1) ;; a little subtle here
	f-n-1
	(iter (fn f-n-1 f-n-2 f-n-3)
	      f-n-1
	      f-n-2
	      (- counter 1))))
  
  (cond ((< n 3)
	 n)
	((integer? n)
	 (iter 2 1 0 (- n 2)))
	(else (iter (get-init n 2)
		    (get-init n 1)
		    (get-init n 0)
		    (- n 2)))))

(assert '(float=? (f-rec 0.5) 0.5))
(assert '(float=? (f-rec 3.5) 7.0))
(assert '(float=? (f-rec 4.5) 16.5))

(assert '(= (f-rec 3) 4))
(assert '(= (f-rec 4) 11))

(assert '(float=? (f-rec 0.5) (f-iter-2 0.5)))
(assert '(float=? (f-rec 3.5) (f-iter-2 3.5)))
;; (assert '(float=? (f-rec 3.5) (f-iter 3.5)))
(assert '(float=? (f-rec 4.5) (f-iter-2 4.5)))

(assert '(= (f-rec 3) (f-iter 3)))
(assert '(= (f-rec 3) (f-iter-2 3)))
(assert '(= (f-rec 4) (f-iter 4)))
(assert '(= (f-rec 4) (f-iter-2 4)))
