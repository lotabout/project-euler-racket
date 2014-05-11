#lang racket

(define names
  (sort
   (map (lambda (x) (string-trim x "\""))
	(string-split (file->string "pro22.txt") ","))
   string<?))

(define (name->score name)
  (apply + (map (lambda (x) (- (char->integer x) 64)) (string->list name))))

(define (pro22)
  (define name-scores
    (map name->score names))
  (let loop ([lst name-scores] [weight 1] [total 0])
    (if (null? lst)
	total
	(loop (cdr lst) (add1 weight) (+ total (* weight (car lst)))))))

(pro22)
