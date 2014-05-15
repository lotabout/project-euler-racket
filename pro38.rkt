#lang racket

(require (only-in "common.rkt"
		  digits->num
		  num->digits))

(define (contains9? . rest)
  (define lst (sort (apply append (map num->digits rest)) <))
  (equal? lst '(1 2 3 4 5 6 7 8 9)))

(define (pandigital? num n)
  (apply contains9? (map (lambda (x) (* num x)) (range 1 (add1 n)))))

(define (num-of-digits n)
  (let loop ([cur 0] [n n])
    (if (= n 0)
	cur
	(loop (add1 cur) (quotient n 10)))))

(define (pandigital-multiple? num)
  (define len (quotient 9 (num-of-digits num)))
  (for/or ([i (in-range 2 (add1 len))])
    (pandigital? num i)))

(define (get-pandigital num n)
  (digits->num
   (apply append
	  (for/list ([i (in-range 1 (add1 n))])
	    (num->digits (* num i))))))

(define (get-pan-multiple num)
  (define len (quotient 9 (num-of-digits num)))
  (let loop ([i len])
    (cond
     [(< i 1) #f]			; not gonna happen
     [(pandigital? num i) (get-pandigital num i)]
     [else (loop (sub1 i))])))

(define (pro38)
  (let loop ([cur 9999])
    (if (pandigital-multiple? cur)
	(get-pan-multiple cur)
	(loop (sub1 cur)))))

(pro38)
