#lang racket

(define triangle
  (map (lambda (x)
	 (map string->number (string-split x))) (file->lines "pro67.txt")))

(define (max-path bot up)
  (map max
       (map + up (drop bot 1))
       (map + up (drop-right bot 1))))

(define (pro67 lst)
  (let ([lst (reverse lst)])
    (let loop ([bot (car lst)] [rest (cdr lst)])
      (if (null? rest)
	  (car bot)
	  (loop (max-path bot (car rest)) (cdr rest))))))

(pro67 triangle)
