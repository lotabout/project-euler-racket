#lang racket

;;; We first estimate the upper bound of the n-th prime, and then use
;;; sieve to actually get it

(require "common.rkt")

;;; note that (n >= 4)
(define (nth-prime n)
  (define upper (exact-ceiling (+ (* n (log n)) (* n (log (log n))))))
  (list-ref (sieve upper) (sub1 n)))

(nth-prime 10001)
