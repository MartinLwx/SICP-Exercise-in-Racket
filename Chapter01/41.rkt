#lang racket

(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x)
  (+ x 1))

;; (double double) will apply 4 times
;; (double (double double)) will apply 8 times
;; ((double (double double)) inc) will apply 16 times of inc
;; i.e. we wil inc 5 16 times and get 16 + 5 = 21
(((double (double double)) inc) 5)   ;; 21
