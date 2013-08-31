;; x:(x1 x2)
;; y:(y1 y2)
;; 1 # x1 >= 0, x2 >= 0, y1 >= 0, y2 >= 0 -> (x1y1, x2y2)
;; 2 # x1 >= 0, x2 >= 0, y1 >= 0, y2 <= 0 -> Impossible
;; 3 # x1 >= 0, x2 >= 0, y1 <= 0, y2 >= 0 -> (x2y1, x2y2)
;; 4 # x1 >= 0, x2 >= 0, y1 <= 0, y2 <= 0 -> (x2y1, x1y2)
;; 5 # x1 >= 0, x2 <= 0, y1 >= 0, y2 >= 0 -> Impossible
;; 6 # x1 >= 0, x2 <= 0, y1 >= 0, y2 <= 0 -> Impossible
;; 7 # x1 >= 0, x2 <= 0, y1 <= 0, y2 >= 0 -> Impossible
;; 8 # x1 >= 0, x2 <= 0, y1 <= 0, y2 <= 0 -> Impossible
;; 9 # x1 <= 0, x2 >= 0, y1 >= 0, y2 >= 0 -> (x1y2, x2y2)
;; 10# x1 <= 0, x2 >= 0, y1 <= 0, y2 >= 0 -> Impossible
;; 11# x1 <= 0, x2 >= 0, y1 <= 0, y2 >= 0 -> (min(x1y2, x2y1), max(x1y1, x2y2))
;; 12# x1 <= 0, x2 >= 0, y1 <= 0, y2 <= 0 -> (x2y1, x1y1)
;; 13# x1 <= 0, x2 <= 0, y1 >= 0, y2 >= 0 -> (x1y2, x2y1)
;; 14# x1 <= 0, x2 <= 0, y1 >= 0, y2 <= 0 -> Impossible
;; 15# x1 <= 0, x2 <= 0, y1 <= 0, y2 >= 0 -> (x1y2, x1y1)
;; 16# x1 <= 0, x2 <= 0, y1 <= 0, y2 <= 0 -> (x2y2, x1y1)

(define (improved-mul-interval x y)
  (let ((x1 (lower-bound x))
	 (x2 (upper-bound x))
	 (y1 (lower-bound y))
	 (y2 (upper-bound y)))
     (define (cond-p op-1 op-2 op-3 op-4)
       (and (op-1 x1 0) (op-2 x2 0) (op-3 y1 0) (op-4 y2 0)))
     (define (make-i a b c d)
       (make-interval (* a b) (* c d)))
     (cond ((cond-p >= >= >= >=) ;1 #
	    (make-i x1 y1 x2 y2))
	   ((cond-p >= >= <= >=) ;3 #
	    (make-i x2 y1 x2 y2))
	   ((cond-p >= >= <= <=) ;4 #
	    (make-i x2 y1 x1 y2))
	   ((cond-p <= >= >= >=) ;9 #
	    (make-i x1 y2 x2 y2))
	   ((cond-p <= >= <= >=) ;11#
	    (make-interval (min (* x1 y2) (* x2 y1))
			   (max (* x1 y1) (* x2 y2))))
	   ((cond-p <= >= <= <=) ;12#
	    (make-i x2 y1 x1 y1))
	   ((cond-p <= <= >= >=) ;13#
	    (make-i x1 y2 x2 y1))
	   ((cond-p <= <= <= >=) ;15#
	    (make-i x1 y2 x1 y1))
	   ((cond-p <= <= <= <=) ;16#
	    (make-i x2 y2 x1 y1))
	   (else (display "illegal interval")))))

(define (interval=? i-1 i-2)
  (and (= (lower-bound i-1) (lower-bound i-2))
	   (= (upper-bound i-1) (upper-bound i-2))))
		 
(print-interval i)
(print-interval i-2)
(print-interval i-3)


(print-interval (improved-mul-interval i i-2))
(print-interval (improved-mul-interval i i-3))
(print-interval (improved-mul-interval i-2 i-3))

(assert '(interval=? (mul-interval i i-2)
					 (improved-mul-interval i i-2)))

(assert '(interval=? (mul-interval i i-3)
					 (improved-mul-interval i i-3)))

(assert '(interval=? (mul-interval i-2 i-3)
					 (improved-mul-interval i-2 i-3)))