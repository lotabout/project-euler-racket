#lang racket

;;; Brute force without any optimization
;;; the key is `remove-duplicates` provided by racket
(define (pro29-force a-limit b-limit)
  (length (remove-duplicates
	   (for*/list ([a (in-range 2 (add1 a-limit))]
		       [b (in-range 2 (add1 b-limit))])
	     (expt a b)))))

(pro29-force 100 100)
