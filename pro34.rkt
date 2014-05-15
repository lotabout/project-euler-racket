#lang racket

(require (only-in "common.rkt"
		  num->digits))

;;; This problem is similar to problem 30.
;;; The only difference is the function applied to digits.

;;; Estimation of the upper bound
;;; suppose number have n digits, then 10^(n+1) <= n * 9!
;;; => for n >= 6, this do not hold, thus, the upper bound is 6*9! ~= 2400,000

(define (fact n)
  (apply * (range 1 (add1 n))))

(define facts (map fact '(0 1 2 3 4 5 6 7 8 9)))

(define (sum-of-digit-fact n)
  (apply + (map (lambda (x) (list-ref facts x))
		(num->digits n))))

(define (pro34)
  (apply + (for/list ([i (in-range 11 2400000)]
	      #:when (= i (sum-of-digit-fact i)))
     i)))

(pro34)
