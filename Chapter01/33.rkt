#lang racket

;; combine only those terms derived from values in the range
;; that satisfy a specified condition
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (filtered-accumulate filter combiner null-value term (next a) next b))
          (filtered-accumulate filter combiner null-value term (next a) next b))))

;; ---------------------------------------------------------------------
;; a. the sum of the squares of the prime numbers in the interval a to b
;; ---------------------------------------------------------------------
(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (inc x) (+ 1 x))

(define (sum-square-prime-between-a-b a b)
  (filtered-accumulate prime? + 0 square a inc b))

(sum-square-prime-between-a-b 2 10)
;; 87 = 2 * 2 + 3 * 3 + 5 * 5 + 7 * 7

;; ----------------------------------------------------------------
;; a. the product of all positive integers i < n s.t. GCD(i, n) = 1
;; ----------------------------------------------------------------

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (identity x) x)

(define (product-integers-gcd-one n)
  (letrec ([fix-gcd (lambda (x) (eq? (gcd x n) 1))])
    (filtered-accumulate fix-gcd * 1 identity 1 inc (- n 1))))  ;; the a and b are both inclusive so we use (n - 1) here

(product-integers-gcd-one 10)
;; 189 = 1 * 3 * 7 * 9
