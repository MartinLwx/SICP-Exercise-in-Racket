#lang racket

;; in racket, we can use [] or ()
(define (sum-two-larger a b c)
  (let ([square (lambda (x) (* x x))])   ;; define square function
    (cond ((and (>= a b) (>= c b)) (+ (square a) (square c)))
          ((and (>= a c) (>= b c)) (+ (square a) (square b)))
          ((and (>= b a) (>= c a)) (+ (square b) (square c))))))

(sum-two-larger 1 2 3)
(sum-two-larger 2 1 3)
(sum-two-larger 3 2 1)
