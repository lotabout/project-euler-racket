#lang racket

(require "common.rkt")

(define (pro10 (upper 2000000))
  (apply + (sieve upper)))

(pro10)
