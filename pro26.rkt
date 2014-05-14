#lang racket

(define (num-cycle n)
  (define vec (make-vector n 0))
  (let loop ([rem 1] [indx 1])
    (let-values ([(q r) (quotient/remainder (* rem 10) n)])
      (cond
       [(zero? r) 0]
       [(> (vector-ref vec rem) 0) (- indx (vector-ref vec rem))]
       [else (vector-set! vec rem indx)
	     (loop r (add1 indx))]))))


(define (pro26 upper)
  (let loop ([cur 1] [max 0] [max-indx 1])
    (if (>= cur upper)
	max-indx
	(let ([cycles (num-cycle cur)])
	  (if (> cycles max)
	      (loop (add1 cur) cycles cur)
	      (loop (add1 cur) max max-indx))))))


(pro26 1000)
