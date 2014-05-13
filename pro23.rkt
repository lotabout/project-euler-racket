#lang racket

(require (only-in "common.rkt"
		  sieve
		  group))

;;; import `sum-of-factors` from problem 21.

;;; do not compute primes again and again
(define primes (sieve 28123))

(define (get-prime-factors primes n)
  (define (rec n primes ret )
    (if (= n 1) 
	(reverse ret)
	(let*-values ([(p) (car primes)]
		      [(q r) (quotient/remainder n p)])
	  (if (zero? r)
	      (rec q primes (cons p ret))
	      (rec n (cdr primes) ret)
	      ))))
  (rec n primes '()))

;;; (+ 1 x x^2 x^3 ... x^q)
(define (sum-up x q)
  (/ (sub1 (expt x (add1 q)))
     (sub1 x)))

(define (sum-of-factors n)
  ;; 28 -> ((2 2) (7 1))
  (define prime-factors
    (map (lambda (x) (list (car x) (length x)))
	 (group (get-prime-factors primes n))))
  ;; (2^0 + 2^1 + 2^2) * (7^0)
  (- (apply *
	  (map (lambda (x) (sum-up (car x) (cadr x)))
	       prime-factors))
     n))


(define (abundant? n)
  (> (sum-of-factors n) n))

(define (get-abundant-num upper)
  (define vec (make-vector upper #f))
  (for ([i (in-range 1 upper)])
    (when (abundant? i)
      (vector-set! vec i #t)))
  (for/list ([i (in-range 1 upper)]
	     #:when (vector-ref vec i))
    i))

(define (get-non-abundant-sums upper)
  (define vec (make-vector upper #t))
  (define abunds (get-abundant-num upper))
  (let loop ([xs abunds] [ys abunds])
    (cond
     [(null? xs) #t]
     [(null? ys) (loop (cdr xs) abunds)]
     [else
      (let* ([x (car xs)] [y (car ys)] [sum (+ x y)])
	(if (>= sum upper)
	    (loop (cdr xs) abunds)
	    (begin
	      (vector-set! vec sum #f)
	      (loop xs (cdr ys)))))]))
  (for/list ([i (in-range 1 upper)]
	     #:when (vector-ref vec i))
    i))

(define (pro23)
  (apply + (get-non-abundant-sums 28123)))

(time (pro23))
