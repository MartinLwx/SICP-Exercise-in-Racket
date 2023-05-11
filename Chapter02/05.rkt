#lang racket

;; represent pairs of nonnegative integers using only
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car p)
  (define (helper t)
    (if (not (= 0 (remainder t 2)))
        0
        (+ 1
           (helper (quotient t 2)))))
  (helper p))

(define (cdr p)
  (define (helper t)
    (if (not (= 0 (remainder t 3)))
        0
        (+ 1
           (helper (quotient t 3)))))
  (helper p))

;; tests
(define p (cons 3 2))
(display p)       ;; verify p is 72 = 2^3 * 3^2
(newline)
(car p)
(cdr p)
