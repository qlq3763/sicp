(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


;; EXERCISE 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; (define tree-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
;; (define tree-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
;; (define tree-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

;; (print tree-1)
;; (print (tree->list-1 tree-1))
;; (print (tree->list-2 tree-1))

;; (print tree-2)
;; (print (tree->list-1 tree-2))
;; (print (tree->list-2 tree-2))

;; (print tree-3)
;; (print (tree->list-1 tree-3))
;; (print (tree->list-2 tree-3))

;; EXERCISE 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; (print (list->tree '(1 3 5 7 9 11)))
;; INFORMATION RETRIEVAL

;; Exercise 2.65

;; from section--sets as ordered lists
(define (intersection-set-ordered-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-ordered-list (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set-ordered-list (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-ordered-list set1 (cdr set2)))))))

;; from Exercise 2.62
(define (union-set-ordered-list set1 set2)
  (cond ((null? set1)
	 set2)
	((null? set2)
	 set1)
	(else (let ((x1 (car set1))
		    (x2 (car set2)))
		(cond ((= x1 x2)
		       (cons x1 (union-set-ordered-list (cdr set1) (cdr set2))))
		      ((< x1 x2)
		       (cons x1 (union-set-ordered-list (cdr set1) set2)))
		      (else cons x2 (union-set-ordered-list set1 (cdr set2))))))))

(define (union-set set1 set2)
  (list->tree (union-set-ordered-list (tree->list-2 set1) (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-set-ordered-list (tree->list-2 set1) (tree->list-2 set2))))

(define tree-4 (list->tree '(1 2 3 4 5)))
(define tree-5 (list->tree '(1 3 5 7 9)))
;; (print tree-4)
;; (print tree-5)
;; (print (union-set tree-4 tree-5))
;; (print (intersection-set tree-4 tree-5))

;; Exercise 2.66
;; (define (lookup given-key set-of-records)
;;   (cond ((null? set-of-records) false)
;;         ((equal? given-key (key (car set-of-records)))
;;          (car set-of-records))
;;         (else (lookup given-key (cdr set-of-records)))))

;; Assumption: numerical valuses as keys
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((cur-record (entry set-of-records)))
	(let ((cur-key (key cur-record)))
	  (cond ((= given-key cur-key)
		 cur-record)
		((< given-key cur-key)
		 (lookup given-key (left-branch set-of-records)))
		(else (lookup given-key (right-branch set-of-records))))))))

(define (key record)
  record)
(print (lookup 4 tree-4))
(print (lookup 4 tree-5))