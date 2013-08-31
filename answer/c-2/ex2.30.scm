(define (square-tree t)
  (cond ((null? t)
		 t)
		((not (pair? t))
		 (square t))
		(else (cons (square-tree (car t))
					(square-tree (cdr t))))))

;; get used to this inner call outter recursion
(define (square-tree-2 t)
  (map (lambda (sub-tree)
		 (if (pair? sub-tree)
			 (square-tree-2 sub-tree)
			 (square sub-tree)))
       t))

(newline)
(display (square-tree
		  (list 1
				(list 2 (list 3 4) 5)
				(list 6 7))))

(newline)
(display (square-tree-2
		  (list 1
				(list 2 (list 3 4) 5)
				(list 6 7))))