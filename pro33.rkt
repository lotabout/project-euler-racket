#lang racket

(require (only-in "common.rkt"
		  num->digits))

(define (find-common x y)
  (define xs (sort x <))
  (define ys (sort y <))
  (let loop ([xs xs] [ys ys] [ret '()])
    (cond
     [(or (null? xs) (null? ys)) ret]
     [(< (car xs) (car ys)) (loop (cdr xs) ys ret)]
     [(< (car ys) (car xs)) (loop xs (cdr ys) ret)]
     [(= (car xs) (car ys)) (loop (cdr xs) (cdr ys) (cons (car xs) ret))])))

(define (remove-from xs items)
  (let loop ([ret xs] [i items])
    (if (null? i)
	ret
	(loop (remove (car i) xs) (cdr i)))))

(define (remove-common xs ys comm)
  (values (remove-from xs comm) (remove-from ys comm)))

(define (valid? a b)
  (let* ([as (num->digits a)]
	 [bs (num->digits b)]
	 [comm (find-common as bs)])
    (if (null? comm)
	#f
	(let-values ([(a-lst b-lst)
		      (remove-common as bs comm)])
	  (and (not (null? a-lst))
	       (not (null? b-lst))
	       (= (/ a b) (/ (car a-lst) (car b-lst))))))))

(define (pro33)
  (define fractions
    (for*/list ([a (in-range 10 100)]
		[b (in-range (add1 a) 100)]
		#:when (and (not (zero? (remainder a 10)))
			    (not (zero? (remainder b 10)))
			    (valid? a b)))
      (list a b)))
  (denominator
   (/ (apply * (map car fractions))
      (apply * (map cadr fractions)))))


(pro33)
