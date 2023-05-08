#lang racket

;; recalll the applicative-order evaluation
;; that says: we evaluate the arguments and then apply

;; the problem is we will evalute (expmod base (/ exp 2) m) twice
;; even though they are equal.

;; the original s-expression is (square (expmod base (/ exp 2) m))
;; , and we can see that we will only it once

;; by the master theorem: T(n) = 2T(n / 2) + O(1)
;; n^{log_2 2} >> O(1), so the time complexity is O(n)
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
