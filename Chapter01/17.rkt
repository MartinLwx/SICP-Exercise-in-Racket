#lang racket

(define (double x)
  (+ x x))

;; x should be an even integer
(define (halve x)
  (/ x 2))

(define (even? x)
  (= (remainder x 2) 0))

;; invariant: a * b won't change
;; when b is even: (a * 2) * (b / 2)
;; when b is odd: a * (b - 1) + a
(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (* (double a) (halve b)))
        (else (+ (* a (- b 1))
                 a))))

(require racket/trace)
(trace *)
(* 3 4)
(* 4 5)
