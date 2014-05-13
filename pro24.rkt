#lang racket

(define (fact n)
  (apply * (range 1 (add1 n))))

;;; 1. encode
(define (encode nth num)
  (let loop ([cur nth] [n (sub1 num)] [ret '()])
    (if (< n 0)
	(reverse ret)
	(let-values ([(q r) (quotient/remainder cur (fact n))])
	  (loop r (sub1 n) (cons q ret))))))


(define (decode code lst)
  (define (ref-and-delete lst i)
    (let loop ([lst lst] [i i] [head '()])
      (cond
       [(= i 0) (values (car lst) (append (reverse head) (cdr lst)))]
       [else (loop (cdr lst) (sub1 i) (cons (car lst) head))])))
  (let loop ([code code] [lst lst] [ret '()])
    (if (null? code)
	(reverse ret)
	(let-values ([(cur next-lst)
		      (ref-and-delete lst (car code))])
	  (loop (cdr code) next-lst (cons cur ret))))))

(define (pro24 n)
  (decode (encode (sub1 n) 10) '(0 1 2 3 4 5 6 7 8 9)))

(pro24 1000000)
