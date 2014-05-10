#lang racket

(require "common.rkt") ; for `num->digits`

(define (pro16)
  (apply + (num->digits (expt 2 1000))))

(pro16)
