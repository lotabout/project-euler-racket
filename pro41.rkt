#lang racket

(require (only-in "common.rkt"
		  sieve
		  num->digits))

(define (pandigital? num)
  (let ([digits (num->digits num)])
    (equal? (sort digits <)
	    (range 1 (add1 (length digits))))))

(define (pro41 upper)
  (define primes (reverse (sieve upper)))
  (let loop ([prime primes])
    (if (pandigital? (car prime))
	(car prime)
	(loop (cdr prime)))))

;;; I actually guess for this upper bound again and again Note that
;;; After you finish this, there is a very useful hint for guessing
;;; this bound in the thread.
;;; Will not exceed 7 numbers.
(pro41 10000000)
