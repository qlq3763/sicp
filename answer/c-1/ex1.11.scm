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
  (define (iter f-n-1 f-n-2 f-n-3 counter)
    (define (fn a b c) (+ a (* 2 b) (* 3 c)))
    (if (< counter 1) ;; a little subtle here
	f-n-1
	(iter (fn f-n-1 f-n-2 f-n-3)
	      f-n-1
	      f-n-2
	      (- counter 1))))
  (define (get-init n diff)
    (+ (- n (floor n))
       diff))
  (cond ((< n 3)
	 n)
	((integer? n)
	 (iter 2 1 0 (- n 2)))
	(else (iter (get-init n 2)
		    (get-init n 1)
		    (get-init n 0)
		    (- n 2)))))