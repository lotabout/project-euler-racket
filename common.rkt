#lang racket

(define (sieve n)
    (define primes (make-vector (add1 n) #t))
    (for* ([i (in-range 3 (add1 n) 2)]
	   #:when (vector-ref primes i)
	   [j (in-range (* i i) (add1 n) i)])
      (vector-set! primes j #f))
    (cons 2 (for/list ([n (in-range 3 (add1 n) 2)]
		       #:when (vector-ref primes n))
	      n)))
(provide sieve)

;;; 1234 => (1 2 3 4)
(define (num->digits num)
  (let rec ([num num] [digits '()])
    (if (= num 0)
	digits
	(let-values ([(q r) (quotient/remainder num 10)])
	  (rec q (cons r digits))))))

(provide num->digits)

;;; (1 1 1 2 3 3) => ((1 1 1) (2) (3 3))
(define (group lst)
  (let loop ([l lst] [ret '()])
    (if (null? l)
	(reverse ret)
	(let-values ([(first rest) (partition (lambda (x) (= x (car l))) l)])
	  (loop rest (cons first ret))))))
(provide group)

(define (get-prime-factors n)
  (define primes (sieve n))
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

(provide get-prime-factors)
