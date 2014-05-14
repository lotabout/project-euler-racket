#lang racket

(require (only-in "common.rkt"
		  sieve))

(define (prime? n)
  (define limit (integer-sqrt n))
  (let loop ([i 2])
    (cond
     [(> i limit) #t]
     [(zero? (remainder n i)) #f]
     [else (loop (add1 i))])))

(define (max-length a b)
  (let loop ([cur 1])
    (let ([num (+ (* cur cur) (* cur a) b)])
      (if (and (> num 0) (prime? num))
	  (loop (add1 cur))
	  cur))))

(define (find-max lst less)
  (let loop ([xs (cdr lst)] [max-item (car lst)])
    (cond
     [(null? xs) max-item]
     [(not (less (car xs) max-item)) (loop (cdr xs) (car xs))]
     [else (loop (cdr xs) max-item)])))

(define (max-item lst key-func)
  (find-max lst (lambda (a b) (< (key-func a) (key-func b)))))

(define (pro27)
  (define max-pair
    (max-item
     (for*/list ([b (sieve 1000)]
		 [a (in-range (- 1 b) 1000)])
       (let ([length (max-length a b)])
	 (list a b length)))
     caddr))
  (* (car max-pair) (cadr max-pair)))


(pro27)

