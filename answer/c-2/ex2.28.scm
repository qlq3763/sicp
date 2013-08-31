(define (fringe l)
  (cond ((null? l)
	 l)
	((not (pair? l))
	 (list l))
	(else (append (fringe (car l))
		(fringe (cdr l))))))

;; use x from ex2.27
(fringe (list x x))

;; compare this implementation with the one from
;; ex2.27--my-deep-reverse-2, think about a question: where does the
;; operator "list" should be put? I ex2.27, the "list" is put at the last
;; part, however, here, "list" is put at the "not pair?" part. The
;; reason, I think, is that here I just want to construct a plain list,
;; while in ex2.27 what I want to construct is a list of list(maybe). 

;; The other thing need to mention is that only the car can
;; get reduce a list to a non-list element, cdr can not achieve this, it can
;; only get nil.