;; a.
(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cadr b))

;; b.
(define (total-weight m)
  (cond ((null? m)
	 0)
	((not (pair? m))
	 m)
	(else (+ (total-weight (branch-structure (left-branch m)))
		 (total-weight (branch-structure (right-branch m)))))))

;; c. how to define local variables depending on local variables????
(define (mobile-balanced? m)
  (define (balanced? l r)
    (let ((l-l (branch-length l))
	  (l-w (total-weight (branch-structure l)))
	  (r-l (branch-length r))
	  (r-w (total-weight (branch-structure r))))
      (= (* l-l l-w) (* r-l r-w))))

  (cond ((not (pair? m))
	 true)
	((balanced? (left-branch m) (right-branch m))
	 (and (mobile-balanced? (branch-structure (left-branch m)))
	      (mobile-balanced? (branch-structure (right-branch m)))))
	(else false)))
	 
;; d.
;; The selectors need to change.
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (right-branch m)
  (cdr m))

(define (branch-structure b)
  (cdr b))

;; (define (make-mobile left right)
;;   (list left right))

;; (define (make-branch length structure)
;;   (list length structure))

(define b-1 (make-branch 4 1))
(define b-2 (make-branch 4 2))
(define b-3 (make-branch 4 3))
(define b-4 (make-branch 4 4))

(define m-1 (make-mobile b-1 b-2))
(define m-2 (make-mobile b-3 b-4))

(define b-5 (make-branch 4 m-1))
(define b-6 (make-branch 4 m-2))

(define m-3 (make-mobile b-5 b-6))
(define m-4 (make-mobile b-1 b-1))
(define b-7 (make-branch 4 m-4))
(define m-5 (make-mobile b-7 b-2))
(define b-8 (make-branch 4 m-5))
(define m-6 (make-mobile b-8 b-4))

(newline)
(display (total-weight m-3))

(newline)
(display (mobile-balanced? m-3))

(newline)
(display (mobile-balanced? m-4))

(newline)
(display (mobile-balanced? m-5))


(newline)
(display (mobile-balanced? m-6))
;; part d
;; (define (make-mobile left right)
;;   (cons left right))

;; (define (make-branch length structure)
;;   (cons length structure))

