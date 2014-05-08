#lang racket

(define (pro5 upper)
  (apply lcm (range 1 20)))

(pro5 20)
