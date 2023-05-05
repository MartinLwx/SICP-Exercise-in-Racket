#lang racket

(define (improve guess x)
  (/ (+ (/ x
           (* guess guess))
        (* 2 guess))
     3))

;; if the change is < 0.01%, we think it's a good guess :)
(define (good-enough? guess last-guess)
  (< (abs (/ (- guess last-guess)
             last-guess))
     0.0001))

;; use let expression to remember the improved guess
(define (cube-iter guess x)
  (let ([improved-guess (improve guess x)])
    (if (good-enough? improved-guess guess)
        improved-guess
        (cube-iter improved-guess x))))

;; guess the cube root of any number is 1.0
(define (cbrt x)
  (cube-iter 1.0 x))

;; tests
(cbrt 0)
(cbrt 3141592631415926)
(cbrt -0.0000031415926)
