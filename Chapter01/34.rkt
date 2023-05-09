#lang racket

(define (square x)
  (* x x))

(define (f g)
  (g 2))

(f square)                          ;; 4

(f (lambda (z) (* z (+ z 1))))      ;; 6 = 2 * 3

(f f)

;; racket use applicative-order evaluation - evaluate all arguments then apply
;; so we first evaluate the f argument. i.e. evalute (f 2)
;; the (f 2) is a function application, again, we evalute (f 2) and we get (2 2)
;; but 2 is not a procedure and (2 2) will be meaningless, so we get a error
