#lang racket

(define (square x)
  (* x x))

;; a guess is improved by averaging it with the quotient of the radicand and the old guess
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; WARN: the new-if will cause the OOM(out-of-memory) error
;; EXPLANATION: Lisp uses applicative-order evaluation
;; , which means we will evaluate all arguments and then apply the function.
;; In this example, because the alternative `(sqrt-iter ...)` is a recursive call
;; , we will keep evaluating it forever and finally get OOM.

;; the reason why if expression is a special form is that
;; the evaluation rule is different than function application.
;; We will never evalute both the consequence and alternative parts
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)       ;; change new-if to the built-in if special form to make this work
          guess
          (sqrt-iter (improve guess x)
                     x)))

;; guess the square root of any number is 1.0
(define (sqrt x)
  (sqrt-iter 1.0 x))
