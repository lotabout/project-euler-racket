#lang racket

(define (fibo (max 4000000))
  (define (fibo-rec i j acc)
    (if (> i max)
	acc
	(fibo-rec j (+ i j) (cons i acc))))
  (reverse (fibo-rec 1 2 '())))

(define (pro2 (max 4000000))
  (apply + (filter even? (fibo max))))

(pro2)
