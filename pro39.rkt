#lang racket

(require (only-in "common.rkt"
		  find-max))

(define (get-b a p)
  (quotient/remainder
   (- (* p p) (* 2 a p))
   (* 2 (- p a))))

(define (possible-num p)
  (length (for/list ([a (in-range 1 (quotient p 3))]
	      #:when (let-values ([(b r) (get-b a p)])
		       (and (zero? r)
			    (< a b))))
     a)))

(define (pro39 upper)
  (define (max-index lst)
    (find-max lst <))
  (define all-nums (map possible-num (range 1 (add1 upper))))
  (let-values ([(max indx) (max-index all-nums)])
     (add1 indx)))

(pro39 1000)
