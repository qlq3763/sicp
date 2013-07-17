(define (my-stream-ref s n)
  (if (= n 0)
      (my-stream-car s)
      (my-stream-ref (my-stream-cdr s) (- n 1))))

(define (my-cons-stream a b)
  (cons a (my-delay b)))

(define (my-stream-car stream) (car stream))

(define (my-stream-cdr stream) (my-force (cdr stream)))

(define (my-force delayed-object)
  (delayed-object))

(define (my-delay exp)
  (lambda () exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (my-stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (my-cons-stream
       low
       (my-stream-enumerate-interval (+ low 1) high))))

(define (my-stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons
       (apply proc (map my-stream-car argstreams))
       (lambda () (apply my-stream-map
			 (cons proc (map my-stream-cdr argstreams)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (show x)
  (display-line x)
  x)

(define (display-line x)
  (newline)
  (display x))

(define ss4 (my-stream-enumerate-interval 0 10))
(define x (my-stream-map show ss4))
;; 1. (stream-enumerate-interval 0 10) gives ss1:(cons 0 (delay stream-enumerate-interval (+ 0 1) 10))
;; 2. (stream-map show ss1) will print:
;; 0

;; (stream-ref x 5)
;; will print 
;; 1
;; 2
;; 3
;; 4
;; 5
;; ...done

;; (stream-ref x 7)
;; will print
;; 6
;; 7


Summary: the above version uses my own implementation of "delay", "force" and "map". 
I think the implementation of my-delay is "wrong", because my-delay is a normal function,
and that means it will try to evaluate its arguments before they are passed to "the lambda 
expression", so this means delay must be a special form, which will not try to evaluate its
argument. This means cons-stream must also be a special form, which will not try to evaluate
its second argument.

I should have pay attention to these two sentenses from the text book:
1. Our implementation of streams will be based on a special form called delay.
2. Cons-stream is a special form defined so that...

Unfortunately, I didn't pay enough attention to these and learned the facts the hard way. 

By the way, this unofficial version has different print results from the official one:
(my-stream-ref 5) will always print 1 2 3 4 5
(my-stream-ref 7) will always print 1 2 3 4 5 6 7
The reason is: there is no memo here.

Now, I know that "is equivalent to" is not equal to "is".
