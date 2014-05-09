#lang racket

;;; This problem can be solved by mathmatic analyze, If you finished
;;; this, check Pier's solution on thread of Problem 9.

;;; brute force
(define (pro9)
  (apply * (car (for*/list ([a (in-range 1 1000)]
			[b (in-range (add1 a) (- 1000 a))]
			#:when (let ([c (- 1000 a b)])
				 (and (< b c) (= (+ (* a a) (* b b)) (* c c)))))
	      (list a b (- 1000 a b))))))

(pro9)
