#lang racket

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; if the change is < 0.01%, we think it's a good guess :)
(define (another-good-enough? guess last-guess)
  (< (abs (/ (- guess last-guess)
             last-guess))
     0.0001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

;; use let expression to remember the improved guess
(define (another-sqrt-iter guess x)
  (let ([improved-guess (improve guess x)])
    (if (another-good-enough? improved-guess guess)
        improved-guess
        (another-sqrt-iter improved-guess x))))

;; guess the square root of any number is 1.0
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (another-sqrt x)
  (another-sqrt-iter 1.0 x))

;; small numbers: the problem is we can easily predict a inaccurate number
;; such that (< (abs (- (square bad-guess) x)) 0.001))
(sqrt 0.0001)            ;; the answer should be 0.01 but we got 0.03230844833048122, an inaccurate result
(another-sqrt 0.0001)

;; large numbers: the problem is that we will never get a better prediction
;; such that (< (abs (- (square bad-guess) x)) 0.001)) because the "gap" between
;; two consecutive float numbers will get larger and larger when the float numbers are increasing
;; So the (improve guess x) won't get a better guess because of the round error and we are struck in infinite loop
;; see: https://www.exploringbinary.com/the-spacing-of-binary-floating-point-numbers/

;; (sqrt 12345678912345678) ;; for large numbers, the sqrt function will keep running, uncomment this line to see this
(another-sqrt 12345678912345678)
