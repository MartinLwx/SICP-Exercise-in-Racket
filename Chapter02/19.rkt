#lang racket

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? l)
  (null? l))

(define (first-denomination l)
  (car l))

(define (except-first-denomination l)
  (cdr l))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define us-coins-rev (list 1 5 10 25 50))

(cc 100 us-coins)

;; the order do not affect the answer
;; its result has nothing to do with the order of the coins we try
;; , and we will always try the all combinations
(cc 100 us-coins-rev)
