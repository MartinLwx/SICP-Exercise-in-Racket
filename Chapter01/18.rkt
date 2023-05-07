#lang racket

(define (double x)
  (+ x x))

;; x should be an even integer
(define (halve x)
  (/ x 2))

(define (even? x)
  (= (remainder x 2) 0))

;; invariant: a * b + c won't change, c starts from 0
;; when a or b is 0: return c
;; when b is even: (a * 2) * (b / 2) + c
;; when b is odd: (a + (b - 1) * a) + c = (b - 1) * a + (c + a)
;; the termination condition: both a and b are equal to 1
(define (fast-iter-* a b)
  (fast-iter-*-helper a b 0))

(define (fast-iter-*-helper a b c)
  (cond ((or (= a 0) (= b 0)) c)
        ((even? b) (fast-iter-*-helper (double a)
                                       (halve b)
                                       c))
        (else (fast-iter-*-helper (- b 1)
                                  a
                                  (+ c a)))))

(require racket/trace)
(trace fast-iter-*-helper)
(fast-iter-* 56 19)
