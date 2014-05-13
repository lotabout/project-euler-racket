#lang racket

(require (only-in "common.rkt"
		  num->digits))

(define (num-of-digits n)
  (length (num->digits n)))

(define (pro25 n)
  (let loop ([cur 1] [prev 0] [idx 1])
    (if (>= (num-of-digits cur) n)
	idx
	(loop (+ cur prev) cur (add1 idx)))))

(pro25 1000)
