#lang racket

(require profile)
(require (only-in "common.rkt"
		  num->digits
		  sieve
		  prime?))

(define primes (sieve 1000000))

(define (num-of-digits n)
  (let loop ([ret 0] [n n])
    (if (= n 0)
	ret
	(loop (add1 ret) (quotient n 10)))))

(define (get-all-circular n)
  (define len (num-of-digits n))
  (define power (expt 10 (sub1 len)))
  (let rec ([cur n] [i len] [ret '()])
    (let-values ([(q r) (quotient/remainder cur 10)])
      (define next (+ q (* r power)))
      (if (<= i 1)
	  ret
	  (rec next (sub1 i) (cons next ret))))))

;;; n must be prime itself
(define (circular-prime? n)
  (or (< n 10)
      (andmap prime? (get-all-circular n))))

(define (odd-digits? n)
  (not (memq 2 (num->digits n))))

(define (pro35)
  (+ 1 (length
    (filter (lambda (x) (and (odd-digits? x) (circular-prime? x)))
	    primes))))

(pro35)
