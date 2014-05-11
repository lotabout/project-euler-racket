#lang racket

(require  (only-in "common.rkt" sieve))

;;; (1 1 1 2 3 3) => ((1 1 1) (2) (3 3))
(define (group lst)
  (let loop ([l lst] [ret '()])
    (if (null? l)
	(reverse ret)
	(let-values ([(first rest) (partition (lambda (x) (= x (car l))) l)])
	  (loop rest (cons first ret))))))

;;; guess: the prime factors will not exceed to 50000
(define primes (sieve 50000))
(define (prime-factor n)
  (define (rec n primes ret )
    (if (= n 1) 
	(reverse ret)
	(let*-values ([(p) (car primes)]
		      [(q r) (quotient/remainder n p)])
	  (if (zero? r)
	      (rec q primes (cons p ret))
	      (rec n (cdr primes) ret)
	      ))))
  (rec n primes '()))

(define (num-of-factors n)
  (apply * (map (lambda (x) (add1 (length x)))
		(group (prime-factor n)))))

(define (pro12 (upper 500))
  (let loop ([i 2] [n 1])
    (if (> (num-of-factors n) upper)
	n
	(loop (add1 i) (+ n i)))))


(pro12)
