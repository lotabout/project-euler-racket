#lang racket

;;; given `num` money, and all possible choice `lst`
(define (pro31 num lst)
  (if (or (= num 0) (<= (length lst) 1))
      1
      (let ([cur (car lst)])
	(apply +
	       (for/list ([i (in-range 0 (add1 num) cur)])
		 (pro31 (- num i) (cdr lst)))))))

(pro31 200 '(200 100 50 20 10 5 2 1))
