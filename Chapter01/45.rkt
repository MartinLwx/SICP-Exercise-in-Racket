#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x)
  (* x x))

;; repeat f n times where n >= 1
(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ((compose f (repeated f (- n 1))) x))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y)
     2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (n-average-damp f n)
  ((repeated average-damp n) f))

;; get the nth root of x which will apply average-damp k times
(define (nth-root x n k)
  (fixed-point (n-average-damp (lambda (y) (/ x (expt y (- n 1)))) k)
               1.0))

;; (nth-root 30 4 1)     ;; apply average-damp won't work
(nth-root 30 4 2)        ;; 2.340347319320716

;; now we can do some experiments....
