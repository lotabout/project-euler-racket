#lang racket

(require srfi/13)

(define (palindromic? n)
  (let* ([s (number->string n)]
	 [r (string-reverse s)])
    (string=? s r)))


(define (pro4 upper)
  (apply max
	 (for*/list ([a (in-range upper 0 -1)]
		     [b (in-range a 0 -1)]
		     #:when (palindromic? (* a b)))
	   (* a b))))

(pro4 999)
