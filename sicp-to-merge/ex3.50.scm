(define (my-stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply my-stream-map
              (cons proc (map stream-cdr argstreams))))))

(define s1 (stream-enumerate-interval 0 4))
(define s2 (stream-enumerate-interval 4 8))
(define s3 (my-stream-map + s1 s2))

(display-stream s1)
(display-line "#############")
(display-stream s2)
(display-line "#############")
(display-stream s3)

(define (test-args . args)
  (display-line args))
