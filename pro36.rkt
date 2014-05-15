#lang racket

(require srfi/13)

(define (palindromic? n)
  (let* ([s (number->string n)]
	 [r (string-reverse s)])
    (string=? s r)))

(define (palindromic-2? n)
  (let* ([s (number->string n 2)]
	 [r (string-reverse s)])
    (string=? s r)))

(define (pro36 upper)
  (apply +
	 (filter (lambda (x)
		   (and (palindromic? x)
			(palindromic-2? x)))
		 (range 1 upper 2))))

(pro36 1000000)
