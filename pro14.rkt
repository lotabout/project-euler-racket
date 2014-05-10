#lang racket

;;; simulate the process.

(define (next-collatz n)
  (let-values ([(q r) (quotient/remainder n 2)])
    (if (zero? r)
	q
	(+ (* 3 n) 1))))

(define (pro14 (num 1000000))
  (define collatz (make-vector (add1 num) 0))
  (for ([i (in-range 1 (add1 num))])
    (let loop ([cur (next-collatz i)] [cnt 1])
      (cond
       [(= cur 1) (vector-set! collatz i cnt)]
       [(<= cur num)
	(let ([tmp-count (vector-ref collatz cur)])
	  (if (not (zero? tmp-count))
	      (vector-set! collatz i (+ cnt tmp-count))
	      (loop (next-collatz cur) (add1 cnt))))]
       [else (loop (next-collatz cur) (add1 cnt))])))
  (let loop ([i 1] [max 1] [idx 1])
    (cond
     [(> i num) idx]
     [(> (vector-ref collatz i) max)
      (loop (add1 i) (vector-ref collatz i) i)]
     [else (loop (add1 i) max idx)])))

(pro14)
