#lang racket

(require (only-in "common.rkt"
		  get-prime-factors
		  group))

;;; (+ 1 x x^2 x^3 ... x^q)
(define (sum-up x q)
  (/ (sub1 (expt x (add1 q)))
     (sub1 x)))

(define (sum-of-factors n)
  ;; 28 -> ((2 2) (7 1))
  (define prime-factors
    (map (lambda (x) (list (car x) (length x)))
	 (group (get-prime-factors n))))
  ;; (2^0 + 2^1 + 2^2) * (7^0)
  (- (apply *
	  (map (lambda (x) (sum-up (car x) (cadr x)))
	       prime-factors))
     n))


(define (pro21 (upper 10000))
  (define amicable-nums
    (let loop ([i 2] [ret '()])
      (if (>= i upper)
	  ret
	  (let ([tmp (sum-of-factors i)])
	    (if (and (< i tmp) (= (sum-of-factors tmp) i))
		(loop (add1 i) (cons tmp (cons i ret)))
		(loop (add1 i) ret))))))
  (apply + (filter (lambda (x) (< x upper)) amicable-nums)))

(pro21 10000)
