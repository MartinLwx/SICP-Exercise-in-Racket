#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (product term a next b)
  (if (> a b)
      1             ;; for product, we use 1 instead of 0
      (* (term a)   ;; not we change + to *
         (product term (next a) next b))))

;; combiner: a procedure with 2 arguments
;; null-value: specify what b ase value to use when the terms run out
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (identity x) x)
(define (inc x) (+ 1 x))

(product        identity 1 inc 10)              ;; 3628800
(accumulate * 1 identity 1 inc 10)              ;; 3628800

(sum            identity 1 inc 10)              ;; 55
(accumulate + 0 identity 1 inc 10)              ;; 55
