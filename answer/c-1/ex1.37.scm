(define (cont-frac n d k)
  (define (iter index result)
    (cond ((= index 0) result)
	  (else (iter (- index 1)
		      (/ (n index) (+ (d index) result))))))
  (iter k 0))


(define (rep-fi k)
  (cont-frac (lambda (i) 1.0)
	     (lambda (i) 1.0)
	     k))

(define (rep-fi-4-decimal)
  (define def 0.001)
  (define (smallest-k k)
    (let ((cur (rep-fi k))
	  (next (rep-fi (+ k 1)))) ;; redudant 
      (if (< (abs (- cur next)) def)
	  (+ k 1)
	  (smallest-k (+ k 1)))))
  (smallest-k 1))

(define (cont-frac n d k)
  (define (rec index)
    (if (> index k) 0
	(/ (n index) (+ (d index) (rec (+ index 1))))))
  (rec 1))