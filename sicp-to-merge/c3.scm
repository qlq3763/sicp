;; (define (stream-ref s n)
;;   (if (= n 0)
;;       (stream-car s)
;;       (stream-ref (stream-cdr s) (- n 1))))

;; (define (stream-map proc s)
;;   (if (stream-null? s)
;;       the-empty-stream
;;       (cons-stream (proc (stream-car s))
;;                    (stream-map proc (stream-cdr s)))))

;; (define (stream-for-each proc s)
;;   (if (stream-null? s)
;;       'done
;;       (begin (proc (stream-car s))
;;              (stream-for-each proc (stream-cdr s)))))

;; (define (display-stream s)
;;   (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (show-stream s n)
  (if (= n 0)
	  (display-line "work finished")
	  (begin (display-line (stream-car s))
			 (show-stream (stream-cdr s) (- n 1)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define ones (cons-stream 1 ones))

(define integers (integers-starting-from 1))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (cons-stream a b)
;;   (cons a (my-delay b)))

;; (define (stream-car stream) (car stream))

;; (define (stream-cdr stream) (my-force (cdr stream)))

;; (define (my-force delayed-object)
;;   (delayed-object))

;; (define (my-delay exp)
;;   (lambda () exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (stream-enumerate-interval low high)
;;   (if (> low high)
;;       the-empty-stream
;;       (cons-stream
;;        low
;;        (stream-enumerate-interval (+ low 1) high))))

;; (define (stream-filter pred stream)
;;   (cond ((stream-null? stream) the-empty-stream)
;;         ((pred (stream-car stream))
;;          (cons-stream (stream-car stream)
;;                       (stream-filter pred
;;                                      (stream-cdr stream))))
;;         (else (stream-filter pred (stream-cdr stream)))))
