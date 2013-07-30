 (define (count-change-iter amount)
   (cc-fifties amount 0))


 (define (cc-fifties amount acc)
   (cond ((= amount 0) (+ 1 acc))
         ((< amount 0) acc)
         (else (cc-fifties (- amount 50)
                           (cc-quarters amount acc)))))


 (define (cc-quarters amount acc)
   (cond ((= amount 0) (+ 1 acc))
         ((< amount 0) acc)
         (else (cc-quarters (- amount 25)
                         (cc-dimes amount acc)))))


 (define (cc-dimes amount acc)
   (cond ((= amount 0) (+ 1 acc))
         ((< amount 0) acc)
         (else (cc-dimes (- amount 10)
                         (cc-nickels amount acc)))))


 (define (cc-nickels amount acc)
   (cond ((= amount 0) (+ 1 acc))
         ((< amount 0) acc)
         (else (cc-nickels (- amount 5)
                           (cc-pennies amount acc)))))


 (define (cc-pennies amount acc)
   (cond ((= amount 0) (+ 1 acc))
         ((< amount 0) acc)
         (else (cc-pennies (- amount 1)
                           (cc-nothing amount acc)))))


 (define (cc-nothing amount acc)
   acc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define (count-change-iter-v2 amount)
   (cc-fifties amount 0))


 (define (cc-fifties amount acc)
   (cond ((= amount 0) (+ 1 acc))
         ((< amount 0) acc)
         (else (cc-fifties (- amount 50)
                           (cc-quarters amount acc)))))


 (define (cc-quarters amount acc)
   (cond ((= amount 0) (+ 1 acc))
         ((< amount 0) acc)
         (else (cc-quarters (- amount 25)
                         (cc-dimes amount acc)))))


 (define (cc-dimes amount acc)
   (cond ((= amount 0) (+ 1 acc))
         ((< amount 0) acc)
         (else (cc-dimes (- amount 10)
                         (cc-nickels amount acc)))))


 (define (cc-nickels-v2 amount acc)
   (cond ((= amount 0) (+ 1 acc))
         ((< amount 0) acc)
         (else (cc-nickels (- amount 5)
                           (cc-pennies-v2 amount acc)))))


 (define (cc-pennies-v2 amount acc)
   (cond ((< amount 0) acc)
		 (else (+ 1 acc))))


(define (divide? d n)
  (= (remainder n d) 0))
 ;; (define (cc-nothing amount acc)
 ;;   acc)