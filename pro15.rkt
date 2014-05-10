#lang racket

;;; math, in a mxn grid, the number of all possible routes is C(m+n)n

(define (nCr n r)
  (let ([r (max r (- n r))])
    (/ (apply * (for/list ([i (in-range (add1 (- n r)) (add1 n))]) i))
       (apply * (for/list ([i (in-range 1 (add1 r))]) i)))))


(define (pro15 (m 20) (n 20))
  (nCr (+ m n) m))

(pro15)
