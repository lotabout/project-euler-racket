#lang racket

(define (count-shift func ls)
  (let loop ([cur 0] [ls ls] [ret '()])
    (if (null? ls)
	(reverse ret)
	(let ([next (func cur (car ls))])
	  (loop next (cdr ls) (cons next ret))))))

(define norm-shift (count-shift (lambda (x y) (remainder (+ x y) 7))
				'(0 31 28 31 30 31 30 31 31 30 31 30 31)))
(define leap-shift (count-shift (lambda (x y) (remainder (+ x y) 7))
				'(0 31 29 31 30 31 30 31 31 30 31 30 31)))


(define (weekday y m d)
  (define is-leap (and (zero? (modulo y 4)) (not (zero? (modulo y 400)))))
  (define shift
    (+ (+ (- y 1990)
	  (quotient (add1 (- y 1990)) 4))
       (list-ref (if is-leap leap-shift norm-shift) (- m 1))
       d))
  (modulo shift 7))


(define (pro19)
  (length
   (for*/list ([y (in-range 1901 2001)]
	       [m (in-range 1 13)]
	       #:when (= (weekday y m 1) 0))
     (list y m 1))))

(pro19)
