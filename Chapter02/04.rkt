#lang racket

;; cons returns a procedure accept an procedure argument
;; that will be applied to x and y
(define (cons x y)
  (lambda (m) (m x y)))

;; car accepts a procedure which
(define (car z)
  (z (lambda (p q) p)))

;; by substituion:
;; (car (cons 1 2))
;; ((cons x y) (lambda (p q) p))
;; ((lambda (p q) p) 1 2)
;; 1
(car (cons 1 2))

;; we can mimic the car function
(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons 1 2))
