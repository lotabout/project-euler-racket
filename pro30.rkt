#lang racket

(require (only-in "common.rkt"
		  num->digits))

(define powers (map (lambda (x) (expt x 5)) '(0 1 2 3 4 5 6 7 8 9)))


;;; Note that the number should less than 6*9^5 < 360000

(define (sum-of-digit-power n)
  (apply + (map (lambda (x) (list-ref powers x))
		(num->digits n))))

(define (pro30)
  (apply + (for/list ([i (in-range 1000 360000)]
	      #:when (= i (sum-of-digit-power i)))
     i)))

(pro30)
