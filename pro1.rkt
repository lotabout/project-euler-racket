#lang racket

(define (pro1 (limit 1000))
  (apply +
	 (for/list ([x (in-range 1000)]
		    #:when (or (= (modulo x 3) 0)
			       (= (modulo x 5) 0)))
	   x)))
(pro1)
