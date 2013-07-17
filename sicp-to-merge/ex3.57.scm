;; For n = 0, 1 the number of addition needed is 0;
;; For n >= 2, the number of addition needed is n - 1.

;; Without memo-proc:
;; fib(n) = fib(n-1) + fib(n-2)
;;        = fib(n-2) + fib(n-3) + fib(n-2)
;;        = 2fib(n-2) + fib(n-3)
;; if we (re)compute fib for each n without memo, from the above
;; formula we know that the time needed is exponentially to n.

;; For this problem, without memo-proc, every time we access fibs, we
;; have to recompute it from start. So, from above we know that the time
;; needed is exponentially to n.

;; See also: http://eli.thegreenplace.net/2007/11/05/sicp-sections-351-352/
