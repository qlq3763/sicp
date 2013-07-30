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
  (define diff 0.001)
  (define (smallest-k k)
    (let ((cur (rep-fi k))
	  (next (rep-fi (+ k 1)))) ;; redudant 
      (if (< (abs (- cur next)) diff)
	  (+ k 1)
	  (smallest-k (+ k 1)))))
  (smallest-k 1))

(define (cont-frac-rec n d k)
  (define (rec index)
    (if (> index k) 0
		(/ (n index) (+ (d index) (rec (+ index 1))))))
  (rec 1))

(print "")
(print (rep-fi 100))
(print (rep-fi-4-decimal))
(print (cont-frac-rec (lambda (i) 1.0)
					  (lambda (i) 1.0)
					  100))
(print (/ 1.0 (rep-fi 100)))