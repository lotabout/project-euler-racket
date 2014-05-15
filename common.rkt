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

(define (digits->num digits)
  (let rec ([cur 0] [digits digits])
    (if (null? digits)
	cur
	(rec (+ (* cur 10) (car digits)) (cdr digits)))))
(provide digits->num)

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

;;; test if a number is prime
(define (prime? n)
  (define limit (integer-sqrt n))
  (if (< n 2)
      #f
      (let loop ([i 2])
	(cond
	 [(> i limit) #t]
	 [(zero? (remainder n i)) #f]
	 [else (loop (add1 i))]))))

(provide prime?)


;;; find the max item and return (max max-index)
(define (find-max list less)
  (if (null? list) (values '() #f)
      (let loop ([cur (cdr list)]
		 [indx 1]
		 [max (car list)]
		 [max-indx 0])
	(cond
	 [(null? cur) (values max max-indx)]
	 [(less max (car cur)) (loop (cdr cur) (add1 indx) (car cur) indx)]
	 [else (loop (cdr cur) (add1 indx) max max-indx)]))))
(provide find-max)
