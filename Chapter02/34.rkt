#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; Horner's rule: you may regard the (a_n * x + a_{n-1}) as "higher-iterm"
;; then we always multipy it with x then add "this-coeff"
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;; tests
;; 1 + 6 + 40 + 32 = 79
(horner-eval 2 (list 1 3 0 5 0 1))
