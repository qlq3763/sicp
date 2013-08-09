;; p-m .--------. p-s
;;     |
;;     |
;;     |
;; p-e .
;; (define (make-rectangle p-s p-m p-e)
;;   (cons p-s (cons p-m p-e)))

;; (define (p-s r)
;;   (car r))

;; (define (p-m r)
;;   (cadr r))

;; (define (p-e r)
;;   (cddr r))

;; The interface(abstraction barrier) is consisted of procedures 
;; height-rectangle and width-rectangle. As far as this exercise is
;; concerned, this is true.
(define (height-rectangle r)
  (let ((pe (p-e r))
	(pm (p-m r)))
    (sqrt (+ (square (- (x-point pe) (x-point pm)))
	     (square (- (y-point pe) (y-point pm)))))))

(define (width-rectangle r)
  (let ((pm (p-m r))
	(ps (p-s r)))
    (sqrt (+ (square (- (x-point pm) (x-point ps)))
	     (square (- (y-point pm) (y-point ps)))))))

;;    ------------. c1
;;    |
;;    |
;;    |
;; c2 .
(define (make-rectangle c1 c2)
  (cons c1 c2))

(define (p-s r)
  (car r))

(define (p-e r)
  (cdr r))

(define (p-m r)
  (make-point (x-point (p-e r))
	      (y-point (p-s r))))


(define (perimeter-rectangle r)
  (* 2 (+ (width-rectangle r) (height-rectangle r))))

(define (area-rectangle r)
  (* (width-rectangle r) (height-rectangle r)))

(define ps (make-point 0 2))
(define pm (make-point 0 0))
(define pe (make-point 4 0))

;; To test both versions, you have to comment out one version
;; at a time, then another version.

(define r (make-rectangle ps pe))
(newline)
(display (perimeter-rectangle r))
(newline)
(display (area-rectangle r))

;; (define r2 (make-rectangle ps pm pe))
;; (newline)
;; (display (perimeter-rectangle r2))
;; (newline)
;; (display (area-rectangle r2))
