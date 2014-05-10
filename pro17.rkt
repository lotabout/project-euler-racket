#lang racket

(define ones '("one" "two" "three" "four" "five" "six" "seven" "eight"
"nine" "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen"
"sixteen" "seventeen" "eighteen" "nineteen"))

(define tens '("not-used" "twenty" "thirty" "forty" "fifty" "sixty" "seventy"
"eighty" "ninety"))
(define hundred '("hundred"))

(define ones-cnt (map string-length ones))
(define tens-cnt (map string-length tens))
(define hundred-cnt (map string-length hundred))

;;; the length of number printted string
(define (num-str-len n)
  (cond
   [(<= n 19) (list-ref ones-cnt (sub1 n))]
   [(<= n 99) (let-values ([(q r) (quotient/remainder n 10)])
		(+ (list-ref tens-cnt (sub1 q))
		   (if (zero? r) 0 (num-str-len r))))]
   [(<= n 999) (let-values ([(q r) (quotient/remainder n 100)])
		 (+ (list-ref ones-cnt q) (list-ref hundred-cnt 0)
		    (if (zero? r) 0
			(+ 3 (num-str-len r)))))]
   [(= n 1000) (string-length "onethousand")]))

(define (pro17 (n 1000))
  (apply + (for/list ([i (in-range 1 (add1 n))]) (num-str-len i))))


(pro17)
