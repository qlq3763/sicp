#lang scheme
(require htdp/testing)
(require racket/gui)

;; first vector, exercise 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; simple tests
(define v1 (make-vect 0 4))
(define v2 (make-vect 4 0))
(check-expect (add-vect v1 v2) (make-vect 4 4))
(check-expect (sub-vect v1 v2) (make-vect -4 4))
(check-expect (scale-vect 2 v1) (make-vect 0 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; now the frame, ex2.47

;; version one
;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))
;
;(define (origin-frame f)
;  (car f))
;
;(define (edge1-frame f)
;  (cadr f))
;
;(define (edge2-frame f)
;  (caddr f))

;; version two
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (cddr f))
;; simple tests 
(define v3 (make-vect 4 4))
(define f (make-frame v3 v2 v1))

(check-expect (origin-frame f) v3)
(check-expect (edge1-frame f) v2)
(check-expect (edge2-frame f) v1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the segment, ex2.48
(define (make-segment s e)
  (cons s e))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

;; simple test
(define s (make-segment v1 v2))
(check-expect (start-segment s) v1)
(check-expect (end-segment s) v2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coordinate map
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; transform-painter
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(generate-report)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; segments->painter
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
        segment-list)))


;; wave painter and draw-line, copyed from PictureLanguage_1_0 
(define wave 
  (segments->painter 
   (list (make-segment (make-vect 0.25 0.00) (make-vect 0.37 0.37)) ;1
         (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.25)) ;2
         (make-segment (make-vect 0.50 0.25) (make-vect 0.62 0.00)) ;3
         (make-segment (make-vect 0.75 0.00) (make-vect 0.70 0.50)) ;4
         (make-segment (make-vect 0.70 0.50) (make-vect 1.00 0.30)) ;5
         (make-segment (make-vect 1.00 0.50) (make-vect 0.75 0.62)) ;6
         (make-segment (make-vect 0.75 0.62) (make-vect 0.62 0.62)) ;7
         (make-segment (make-vect 0.62 0.62) (make-vect 0.75 0.75)) ;8
         (make-segment (make-vect 0.75 0.75) (make-vect 0.62 1.00)) ;9
         (make-segment (make-vect 0.40 1.00) (make-vect 0.30 0.75)) ;10
         (make-segment (make-vect 0.30 0.75) (make-vect 0.40 0.62)) ;11
         (make-segment (make-vect 0.40 0.62) (make-vect 0.25 0.62)) ;12
         (make-segment (make-vect 0.25 0.62) (make-vect 0.20 0.50)) ;13
         (make-segment (make-vect 0.20 0.50) (make-vect 0.00 0.70)) ;14
         (make-segment (make-vect 0.37 0.37) (make-vect 0.30 0.50)) ;15
         (make-segment (make-vect 0.30 0.50) (make-vect 0.12 0.37)) ;16
         (make-segment (make-vect 0.12 0.37) (make-vect 0.00 0.50)) ;17
         (make-segment (make-vect 0.50 0.70) (make-vect 0.35 0.75)) ;smile 1
         (make-segment (make-vect 0.50 0.70) (make-vect 0.65 0.75)) ;smile 2
         )))

;; This implementation assumes a default frame, with lengths of edge1
;; and edge2 equal to 250, 250, respectively.
;; I tried to not make this assumption, but for now I could not find a 
;; solution. For draw-line knows nothing about the frame to draw in, but
;; to draw a line, we need a screen(bitmap) to draw in(especially we need
;; to know the size of the screen, which can not be decided).

;; A possible solution may be: a global variable representing the screen, 
;; every time a frame is given to draw in, set! the global screen according 
;; to the new frame.
;; But I don't like global variable, I don't like using set! that way, so I
;; will stick to this simple version(this version can not handle arbitrary frame,
;; for example, a frame whose area is larger than the default one).

;; Setting up the drawing infrastructure specific to PLT Racket.
(define size-x 250)
(define size-y 250)

;; (+ 10) (+ 35), to avoid hiding the outline of frame
(define picture (make-object bitmap% (+ size-x 10) (+ size-y 35)))
(define bm-dc (make-object bitmap-dc% picture))
(send bm-dc clear)

(define frame-gui (new frame% 
                       [label "Picture Language"]
                       [width (+ size-x 10)]
                       [height (+ size-y 35)]))

(define canvas (new canvas%
                    [parent frame-gui]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc draw-bitmap picture 0 0))]))

(define (draw-line v1 v2)
  (send bm-dc 
        draw-line 
        (* size-x (xcor-vect v1)) 
        (- size-y (* size-y (ycor-vect v1))) 
        (* size-x (xcor-vect v2))
        (- size-y (* size-y (ycor-vect v2)))))

(send frame-gui show #t)

;; test painter
(define f1 (make-frame (make-vect 0 0)
                       (make-vect 1 0)
                       (make-vect 0 1)))
;;(wave f1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercises
;; To make test easy, I firstly implement the lower level primitives,
;; then higher level ones, which rely on the lower level ones.
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)    ; new origin
                     (make-vect 1.0 1.0)    ; new end of edge1
                     (make-vect 0.0 0.0)))  ; new end of edge2

;; ((flip-vert wave) f1)

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;; ((rotate90 wave) f1)

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;; ((beside wave wave) f1)

;; ex2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))
;;((flip-horiz wave) f1) 

;; rotate180, rotate270, could use rotate90 to implement
;; I like to use the vector version to test my understanding
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

;;((rotate180 wave) f1)
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))
;; ((rotate270 wave) f1)

;; ex2.51
;(define (below painter1 painter2)
;  (let ((split-point (make-vect 0 0.5)))
;    (let ((paint-bottom
;           (transform-painter painter1
;                              (make-vect 0 0)
;                              (make-vect 1 0)
;                              split-point))
;          (paint-top 
;           (transform-painter painter2
;                              split-point
;                              (make-vect 1 0.5)
;                              (make-vect 0 1))))
;      (lambda (frame)
;        (paint-bottom frame)
;        (paint-top frame)))))

;; version two
(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))
;; ((below wave wave) f1)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


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


(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;; ex2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;;((up-split wave 4) f1)

;; ex2.45
(define (split identity-op smaller-op)
  (define (def-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (def-split painter (- n 1))))
          (identity-op painter (smaller-op smaller smaller)))))
  def-split)

(define right-split-v2 (split beside below))
(define up-split-v2 (split below beside))

;; ((right-split-v2 wave 4) f1)
;; ((up-split-v2 wave 4) f1)

;; ex2.49
(define left-bottom (make-vect 0 0))
(define left-top (make-vect 0 0.99))
(define right-bottom (make-vect 0.99 0))
(define right-top (make-vect 0.99 0.99))
;; a.
(define outline-painter (segments->painter 
                         (list (make-segment left-bottom right-bottom)
                               (make-segment right-bottom right-top)
                               (make-segment right-top left-top)
                               (make-segment left-top left-bottom))))
;;(outline-painter f1)

;; b.
(define x-painter (segments->painter 
                   (list (make-segment left-bottom right-top)
                         (make-segment left-top right-bottom))))
;; (x-painter f1)

;; c.
(define diamond-painter (let ((p1 (make-vect 0.5 0))
                              (p2 (make-vect 1 0.5))
                              (p3 (make-vect 0.5 1))
                              (p4 (make-vect 0 0.5)))
                          (segments->painter
                           (list (make-segment p1 p2)
                                 (make-segment p2 p3)
                                 (make-segment p3 p4)
                                 (make-segment p4 p1)))))
;; (diamond-painter f1)

;; d.
;; see above definition

;; ex2.52
;;((square-limit wave 1) f1)
;; a.
;; already has smile
;; just change the definition of wave, add some segments

;; b.
;; just to chagne the internal implementation of corner-split
;; to avoid duplicate definition, I changed the name of corner-split-b
;; so I also have to change square-limit.
;; If I just need one version of corner-split, I just need to change its 
;; internal definition
(define (corner-split-b painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))


(define (square-limit-b painter n)
  (let ((quarter (corner-split-b painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
;; ((square-limit-b wave 1) f1)

;; c.
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


(define (square-limit-c1 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;;((square-limit-c1 wave 1) f1)

(define (square-limit-c2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity ;; should not make change here
                                  rotate180 flip-vert)))
    (combine4 (corner-split (flip-horiz painter) n)))) ;; yeah, should make change here

((square-limit-c2 wave 1) f1)