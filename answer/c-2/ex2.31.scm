;; use map, which takes care of the cons or append, 
;; or whatever.
(define (tree-map proc tree)
  (map (lambda (sub-tree) 
	 (if (pair? sub-tree)
	     (tree-map proc sub-tree)
	     (proc sub-tree)))
       tree))

(define (square-tree-3 tree)
  (tree-map square tree))

(define t-1 '(1 (4 (9 16) 25) (36 49)))

(newline)
(display (square-tree-3 t-1))