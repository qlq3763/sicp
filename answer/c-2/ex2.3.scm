;; p-m .--------. p-s
;;     |
;;     |
;;     |
;; p-e .
(define (make-rectangle p-s p-m p-e)
  (cons p-s (cons p-m p-e)))

(define (p-s r)
  (car r))

(define (p-m r)
  (cadr r))

(define (p-e r)
  (cddr r))

(define (height-rectangle r)
  (let ((ps (p-s r))
		(pm (p-m r)))
    (sqrt (+ (square (- (x-point ps) (x-point pm)))
			 (square (- (y-point ps) (y-point pm)))))))

(define (width-rectangle r)
  (let ((pm (p-m r))
	(pe (p-e r)))
    (sqrt (+ (square (- (x-point pm) (x-point pe)))
	     (square (- (y-point pm) (y-point pe)))))))

;;    ------------. c1
;;    |
;;    |
;;    |
;; c2 .
;; (define (make-rectangle c1 c2)
;;   (cons c1 c2))

;; (define (p-s r)
;;   (car r))

;; (define (p-e r)
;;   (cdr r))

;; (define (p-m r)
;;   (make-point (x-point (p-e r))
;; 	      (y-point (p-s r))))


(define (perimeter-rectangle r)
  (* 2 (+ (width-rectangle r) (height-rectangle r))))

(define (area-rectangle r)
  (* (width-rectangle r) (height-rectangle r)))

(define ps (make-point 0 2))
(define pm (make-point 0 0))
(define pe (make-point 4 0))

;; To test both versions, you have to comment out one version
;; at a time, then another version.

;; (define r (make-rectangle ps pe))
;; (newline)
;; (display (perimeter-rectangle r))
;; (newline)
;; (display (area-rectangle r))

(define r2 (make-rectangle ps pm pe))
(newline)
(display (perimeter-rectangle r2))
(newline)
(display (area-rectangle r2))