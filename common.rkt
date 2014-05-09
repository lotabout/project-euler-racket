#lang racket

(define (sieve n)
    (define primes (make-vector (add1 n) #t))
    (for* ([i (in-range 3 (add1 n) 2)]
	   #:when (vector-ref primes i)
	   [j (in-range (* i i) (add1 n) i)])
      (vector-set! primes j #f))
    (cons 2 (for/list ([n (in-range 3 (add1 n) 2)]
		       #:when (vector-ref primes n))
	      n)))
(provide sieve)
