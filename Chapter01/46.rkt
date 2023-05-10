#lang racket

(define (average x y)
  (/ (+ x y)
     2))

(define (square x)
  (* x x))

;; this procedure returns a procedure
;; that takes a guess as argument and keeps improving the guess
;; until it is good enough
;; Arg:
;;   test: test if a guess is good enough
;;   improve: how to improve a guess
(define (iterative-improve test improve)
  (define (helper guess)
    (if (test guess)
        guess
        (helper (improve guess))))
  helper)

(define (fixed-point f first-guess)
  (define (close-enough? v)
    (let ([next (f v)])
      (< (abs (- v next)) 0.00001)))
  ((iterative-improve close-enough? f) first-guess))

(define (sqrt x)
  (define (good-enough? v)
    (< (abs (- (square v) x)) 0.001))
  (define (improve guess)
      (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0)) 

(sqrt 2)
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)  ;; taken from Exercise 1.35, the answer should be roughly 1.6180327868852458
