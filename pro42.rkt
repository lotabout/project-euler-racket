#lang racket

(define (triangle-val? n)
  (let ([x (integer-sqrt (* n 2))])
    (= (* x (+ x 1)) (* n 2))))

(define (name->score name)
  (apply + (map (lambda (x) (- (char->integer x) 64)) (string->list name))))

(define names
  (map (lambda (x) (string-trim x "\""))
       (string-split (file->string "pro42.txt") ",")))


(define (pro42)
  (length
   (filter triangle-val?
	   (map name->score names))))


(pro42)
