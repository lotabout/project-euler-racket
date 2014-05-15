#lang racket

(require "common.rkt")

(define (left-trunc? n)
  (define digits (num->digits n))
  (let loop ([cur (cdr digits)])
    (cond
     [(null? cur) #t]
     [(prime? (digits->num cur)) (loop (cdr cur))]
     [else #f])))

(define (right-trunc? n)
  (define digits (num->digits n))
  (let loop ([cur (drop-right digits 1)])
    (cond
     [(null? cur) #t]
     [(prime? (digits->num cur)) (loop (drop-right cur 1))]
     [else #f])))

(define (pro37 upper)
  (drop (filter (lambda (x)
		  (and (left-trunc? x)
		       (right-trunc? x)))
		(sieve upper))
	4))

(apply + (pro37 1000000))
