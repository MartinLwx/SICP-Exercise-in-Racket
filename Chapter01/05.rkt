#lang racket

(define (p) (p))        ;; a dead loop function

(define (test x y)
  (if (= x 0)
      0
      y))

;; in applicative-order evaluation
;;   we will evaluate arguments then apply funciton
;;   when evaluate (p), we will stuck in the dead loop
;; in normal-order evaluation
;;   we will fully expand this then reduce
;;   because (= 0 0) evaluates to #t, (p) will not be evaluated
(test 0 (p))
