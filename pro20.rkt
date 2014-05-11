#lang racket

(require "common.rkt")

(define (fact n)
  (apply * (range 1 (add1 n))))

(define (pro20 (n 100))
  (apply + (num->digits (fact n))))

(pro20)
