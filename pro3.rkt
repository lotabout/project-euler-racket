#lang racket

; get prime numbers
(define (wheel-2 n)
  (define primes (make-vector (add1 n) #t))
  (for* ([i (in-range 3 (add1 n) 2)]
         #:when (vector-ref primes i)
         [j (in-range (* i i) (add1 n) i)])
    (vector-set! primes j #f))
  (cons 2 (for/list ([n (in-range 3 (add1 n) 2)]
	      #:when (vector-ref primes n))
     n)))


(define (pro3 num)
  (define limit (integer-sqrt num))
  (let loop ([primes (reverse (wheel-2 limit))])
    (if (= (modulo num (car primes)) 0)
	(car primes)
	(loop (cdr primes)))))

(pro3 600851475143)
