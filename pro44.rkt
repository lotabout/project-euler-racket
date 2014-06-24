#lang racket

(define (get-n-from-p p)
  (quotient (add1 (integer-sqrt (add1 (* 24 p)))) 6))

(define (get-p-from-n n)
  (/ (* n (- (* n 3) 1)) 2))

(define (pentagonal? p)
  (= (get-p-from-n (get-n-from-p p)) p))

(define (pro44)
  (let loop ([i 1])
    (define m (get-p-from-n i))
    (let inner ([j (sub1 i)])
      (define n (get-p-from-n j))
      (cond
       [(<= j 0) (loop (add1 i))]
       [(and (pentagonal? (- m n))
             (pentagonal? (+ m n)))
        (- m n)]
       [else
        (inner (sub1 j))]))))

(pro44)
