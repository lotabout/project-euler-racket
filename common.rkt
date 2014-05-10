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

;;; 1234 => (1 2 3 4)
(define (num->digits num)
  (let rec ([num num] [digits '()])
    (if (= num 0)
	digits
	(let-values ([(q r) (quotient/remainder num 10)])
	  (rec q (cons r digits))))))

(provide num->digits)
