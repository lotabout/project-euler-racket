#lang racket

(require (only-in "common.rkt"
		  sieve
		  num->digits
		  digits->num))

;;; A-B
(define (set-diff as bs)
  (let loop ([a as] [b bs])
    (if (null? b)
	a
	(loop (remove (car b) a) (cdr b)))))

;;; prev = d0 d1... dx
;;; return all valid d0 d1 ... dx, dy
(define (next-digit prev prime)
  (define candidates (set-diff '(0 1 2 3 4 5 6 7 8 9) prev))
  (for/list ([dy candidates]
	     #:when (= (remainder
			(digits->num (append (take-right prev 2) (list dy)))
			prime)
		       0))
    (append prev (list dy))))

(define (get-permutation lst num)
  (if (zero? num)
      '(())
      (append-map (lambda (i)
		    (map (lambda (x) (cons i x))
			 (get-permutation (remove i lst) (sub1 num))))
		  lst)))

(define (pro43)
  (define start (filter (lambda (x) (not (zero? (car x))))
			(get-permutation '(0 1 2 3 4 5 6 7 8 9) 3)))
  (define primes '(2 3 5 7 11 13 17))
  (apply +
	 (map digits->num
	      (let loop ([start start] [primes primes])
		(if (null? primes)
		    start
		    (loop (append-map (lambda (x) (next-digit x (car primes)))
				      start)
			  (cdr primes)))))))

(pro43)
