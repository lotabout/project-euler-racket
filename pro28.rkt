#lang racket

;;; 1^2 + 2^2 + ... + n^2
(define (sum-square n)
  (/ (* n (+ n 1) (+ (* n 2) 1))
     6))

;;; 1^2 + 3^3 + ... + n^2
(define (sum-odd-square n)
  (- (sum-square n)
     (* 4 (sum-square (/ (- n 1) 2)))))

;;; An = An-2 + 4*n^2 - 6*n + 6
;;; result = 4*(3^2+5^2+...+n^2) - 6*(3+5+...+n) + 3*(n-1) + 1  for n >= 3
(define (pro26 n)
  (+ (* 4 (- (sum-odd-square n) 1))
     (- (* 3/2 (+ n 3) (- n 1)))
     (* 3 (- n 1))
     1))

(pro26 1001)
