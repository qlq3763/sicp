;; can not understand why this version works this way: 
;; given as parameter '((1 2 3 4)), will return '((1 2 3 4)) 
;; instead of '((1 1 1 1)). But one really need attention of
;; myself is that: map does not change the struture of a list.
(define (count-leaves-2 t)
  (accumulate + 
	      0
	      (map (lambda (sub-tree)
		     (cond ((null? sub-tree)
			    0)
			   ((not (pair? sub-tree))
			    1)
			   (else count-leaves sub-tree)))
		   t)))

;; improved and really stupid mistakes
(define (count-leaves-2 t)
  (accumulate + 
	      0
	      (map (lambda (sub-tree)
		     (cond ((null? sub-tree)
			    0)
			   ((not (pair? sub-tree))
			    1)
			   (else (count-leaves sub-tree)))) ; add ( here
		   t)))
;; still can not get things right this time
;; (define (count-leaves t)
;;   ;; (accumulate (lambda (x y) (+ x y))
;;   ;; 	      0
;; 	      (map (lambda (sub-tree)
;; 		     (cond ((null? sub-tree)
;; 			    0)
;; 			   ((not (pair? sub-tree))
;; 			    1)
			   
;; 		   t))

(define (count-leaves t)
  (accumulate +
	      0
	      (map (lambda (sub-tree)
		     (if (not (pair? sub-tree))
			 1
			 (count-leaves sub-tree)))
		   t)))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))


(define (count-leaves-t x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves-t (car x))
                 (count-leaves-t (cdr x))))))

;; (count-leaves l-3)
(define ll-1 '(() (1) 1 (1 2 3 4) (((1)))))
(print (count-leaves ll-1))
(print (count-leaves-2 ll-1))
(print (count-leaves-t ll-1))