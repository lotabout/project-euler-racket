#lang racket

(require (only-in "common.rkt"
		  num->digits))

;;; return the k-th digits of a number
;;; (get-digit 123 0) => 1
;;; (get-digit 123 1) => 2
(define (get-digit num k)
  (list-ref (num->digits num) k))

;;; get the k-th digit starting from 10^n
(define (get-kth-digit-10^n k n)
  (let-values ([(q r) (quotient/remainder (- k 1) n)])
    (get-digit (+ (expt 10 (sub1 n)) q) r)))

;;; total digits below 10^n
(define (total-digits-below-10^n n)
  (+ (* (- n 1/9) (expt 10 n))
     1/9))


(define (kth-digit k)
  (let loop ([n 1])
    (if (< (total-digits-below-10^n n) k)
	(loop (add1 n))
	(get-kth-digit-10^n (- k (total-digits-below-10^n (sub1 n)))
			    n))))


(define (pro35 power)
  (apply * (map kth-digit
		(map
		 (lambda (x) (expt 10 x))
		 (range 0 (add1 power))))))

(pro35 6)
