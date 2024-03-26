#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))


;; A similar procedure like element-of-set?
(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(= x (car set)) set]
        [(< x (car set)) (cons x set)]
        [else (cons (car set) (adjoin-set x (cdr set)))]))


;; Tests
(define set1 '(1 2 3 5))
(adjoin-set 0 set1)
(adjoin-set 2 set1)
(adjoin-set 4 set1)
(adjoin-set 6 set1)
