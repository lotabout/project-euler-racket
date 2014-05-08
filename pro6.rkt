#lang racket

(define (square-sum upper)
  (apply + (map (lambda (x) (* x x)) (range 1 (add1 upper)))))

(define (sum-square n)
  (define sum/2 (/ (* n (add1 n)) 2))
  (* sum/2 sum/2))

(define (pro6 n)
  (- (sum-square n) (square-sum n)))

(pro6 100)
