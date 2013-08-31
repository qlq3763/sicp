#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
;; Exercise 2.49
;;(define origin (make-vect 0 0))
;;(define edge1 (make-vect 4 0))
;;(define edge2 (make-vect 0 4))
;;(define frame (make-frame origin edge1 edge2))
;(define one 0.99)
;(define origin (make-vect 0 0))
;(define x-cor (make-vect one 0))
;(define y-cor (make-vect 0 one))
;(define diagonal (make-vect one one))
;
;;;a
;(define (frame-outline frame)
;    ((segments->painter (list (make-segment origin x-cor)
;                           (make-segment origin y-cor)
;                           (make-segment x-cor diagonal)
;                           (make-segment y-cor diagonal)))
;     frame))
;
;;(paint frame-outline)
;
;;; b
;(define (x-painter frame)
;  ((segments->painter (list (make-segment origin diagonal)
;                            (make-segment x-cor y-cor)))
;   frame))
;;(paint x-painter)
;
;;; c
;(define half 0.5)
;(define x-b-half (make-vect half 0))
;(define x-t-half (make-vect half 1))
;(define y-l-half (make-vect 0 half))
;(define y-r-half (make-vect 1 half))
;(define (diamond-painter frame)
;  ((segments->painter (list (make-segment x-b-half y-l-half)
;                            (make-segment x-b-half y-r-half)
;                            (make-segment x-t-half y-l-half)
;                            (make-segment x-t-half y-r-half)))
;   frame))
;
;;(paint diamond-painter)
;
;;; d
;(define (wave frame)
;  ((segments->painter (list 
;                       ;; high part
;                       (make-segment (make-vect 0 0.8) (make-vect 0.1 0.65))
;                       (make-segment (make-vect 0.1 0.65) (make-vect 0.3 0.7))
;                       (make-segment (make-vect 0.3 0.7) (make-vect 0.25 0.8))
;                       (make-segment (make-vect 0.25 0.8) (make-vect 0.35 0.9))
;                       (make-segment (make-vect 0.5 0.9) (make-vect 0.55 0.8))
;                       (make-segment (make-vect 0.55 0.8) (make-vect 0.5 0.7))
;                       (make-segment (make-vect 0.5 0.7) (make-vect 0.65 0.7))
;                       (make-segment (make-vect 0.65 0.7) (make-vect 0.9 0.3))
;                       ;; low part
;                       (make-segment (make-vect 0 0.65) (make-vect 0.1 0.55))  
;                       (make-segment (make-vect 0.1 0.55) (make-vect 0.2 0.64))
;                       (make-segment (make-vect 0.2 0.64) (make-vect 0.25 0.5))
;                       (make-segment (make-vect 0.2 0) (make-vect 0.25 0.5))
;                       (make-segment (make-vect 0.3 0) (make-vect 0.4 0.4))
;                       (make-segment (make-vect 0.4 0.4) (make-vect 0.5 0))
;                       (make-segment (make-vect 0.6 0) (make-vect 0.5 0.55))
;                       (make-segment (make-vect 0.5 0.55) (make-vect 0.8 0.1))
;                       )) 
;   frame))
;
;;(paint wave)
;;(paint (beside wave (flip-vert wave)))
;
;; Exercise 2.50
;(define (my-flip-horiz painter)
;  ((transform-painter 
;                     (make-vect one 0)
;                     (make-vect 0 0)
;                     (make-vect 1 1))painter))
;
;(define (my-rotate180 painter)
;  ((transform-painter (make-vect one one)
;                     (make-vect 0 one)
;                     (make-vect one 0))
;   painter))
;
;(define (my-rotate270 painter)
;  ((transform-painter (make-vect 0 one)
;                      (make-vect 0 0 )
;                      (make-vect one one))
;   painter))
;
;;(paint einstein)
;;(paint (my-flip-horiz einstein))
;;(paint (rotate90 einstein))
;;(paint (my-rotate180 einstein))
;;(paint (my-rotate270 einstein))
;
;; Exercise 2.51
;(define (my-below painter1 painter2)
;  (let ((split-point (make-vect 0.0 0.5)))
;    (let ((paint-below
;           ((transform-painter 
;                              (make-vect 0.0 0.0)
;                              (make-vect one 0.0)
;                              split-point)
;            painter1))
;          (paint-top
;           ((transform-painter 
;                              split-point
;                              (make-vect one 0.5)
;                              (make-vect 0.0 one))
;            painter2)))
;      (lambda (frame)
;        (paint-below frame)
;        (paint-top frame)))))
;
;(define (my-below-2 painter1 painter2)
;  (rotate270 (beside (rotate90 painter2) (rotate90 painter1))))
;  
;;(paint (my-below wave einstein))
;;(paint (my-below-2 wave einstein))
;
;;; Exercise 2.52
;
;;; a
;; I kind of feel sick trying to make change to this procedure. I should 
;; add more comment here or split this procedure apart. Keep this in mind,
;; don't repeat this pattern again.
;(define (wave-2 frame)
;  ((segments->painter (list 
;                       ;; high part
;                       (make-segment (make-vect 0 0.8) (make-vect 0.1 0.65))
;                       (make-segment (make-vect 0.1 0.65) (make-vect 0.3 0.7))
;                       (make-segment (make-vect 0.3 0.7) (make-vect 0.25 0.8))
;                       (make-segment (make-vect 0.25 0.8) (make-vect 0.35 0.9))
;                       (make-segment (make-vect 0.5 0.9) (make-vect 0.55 0.8))
;                       (make-segment (make-vect 0.55 0.8) (make-vect 0.5 0.7))
;                       (make-segment (make-vect 0.5 0.7) (make-vect 0.65 0.7))
;                       (make-segment (make-vect 0.65 0.7) (make-vect 0.9 0.3))
;                       ;; low part
;                       (make-segment (make-vect 0 0.65) (make-vect 0.1 0.55))  
;                       (make-segment (make-vect 0.1 0.55) (make-vect 0.2 0.64))
;                       (make-segment (make-vect 0.2 0.64) (make-vect 0.25 0.5))
;                       (make-segment (make-vect 0.2 0) (make-vect 0.25 0.5))
;                       (make-segment (make-vect 0.3 0) (make-vect 0.4 0.4))
;                       (make-segment (make-vect 0.2 0) (make-vect 0.3 0)) ;; add foot here
;                       (make-segment (make-vect 0.4 0.4) (make-vect 0.5 0))
;                       (make-segment (make-vect 0.6 0) (make-vect 0.5 0.55))
;                       (make-segment (make-vect 0.5 0) (make-vect 0.6 0)) ;; add foot here
;                       (make-segment (make-vect 0.5 0.55) (make-vect 0.8 0.1))
;                       )) 
;   frame))
;
;;(paint wave-2)
;
;;; b
;(define (right-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (right-split painter (- n 1))))
;        (beside painter (below smaller smaller)))))

(define (split first-op second-op)
  (define (n-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (n-split painter (- n 1))))
          (first-op painter (second-op smaller smaller)))))
  n-split)
(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;
;(define (corner-split-2 painter n)
;  (if (= n 0)
;      painter
;      (let ((up (up-split painter (- n 1)))
;            (right (right-split painter (- n 1))))
;        (let ((top-left up)
;              (bottom-right right)
;              (corner (corner-split painter (- n 1))))
;          (beside (below painter top-left)
;                  (below bottom-right corner))))))
;
;;(paint (corner-split einstein 4))
;;(paint (corner-split-2 einstein 4))


;; c
(define rogers (load-painter "fovnder.gif"))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity ; should not make change here, as I do in the first time
                                  rotate180 flip-vert)))
    (combine4 (corner-split (flip-horiz painter) n))))

(paint (square-limit rogers 1))
(paint rogers)
(paint (flip-horiz rogers))