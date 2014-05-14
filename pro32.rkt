#lang racket

(require (only-in "common.rkt"
		  num->digits))

;;; After analysis, the production can only be pattern
;;; (number of digits x number of digits): 1x4 or 2x3
;;; So, get all permutation of 5 number chosen from 1-9
;;; then check if a if any pair fits.

(define (get-permutation lst num)
  (if (zero? num)
      '(())
      (append-map (lambda (i)
		    (map (lambda (x) (cons i x))
			 (get-permutation (remove i lst) (sub1 num))))
		  lst)))


(define (digits->num lst)
  (let loop ([lst lst] [cur 0])
    (if (null? lst)
	cur
	(loop (cdr lst) (+ (* cur 10) (car lst))))))

;;; if it is pandigital pair, return their multiplication
(define (pandigital? xs ys)
  (let ([result (* (digits->num xs)
		   (digits->num ys))])
    (let ([union (sort (append xs ys (num->digits result)) <)])
      (let loop ([union union] [indx 1])
	(cond
	 [(and (null? union) (= indx 10)) result]
	 [(and (not (null? union))
	       (= (car union) indx))
	  (loop (cdr union) (add1 indx))]
	 [else #f])))))

;;; check if a permutation fits
(define (pro32)
  (apply +
	 (let loop ([lst (get-permutation '(1 2 3 4 5 6 7 8 9) 5)]
		    [ret '()])
	   (if (null? lst)
	       (remove-duplicates ret)
	       (let ([perm (car lst)])
		 (define p14 (pandigital? (list (car perm)) (cdr perm)))
		 (define p23 (pandigital? (take perm 2) (drop perm 2)))
		 (cond
		  [(and p14 p23) (loop (cdr lst) (cons p14 (cons p23 ret)))]
		  [p14 (loop (cdr lst) (cons p14 ret))]
		  [p23 (loop (cdr lst) (cons p23 ret))]
		  [else (loop (cdr lst) ret)]))))))


(pro32)
