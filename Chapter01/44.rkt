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

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-fold-smoothed-f f n)
    ((repeated smooth n) f))

(square 2.3)                         ;; 5.289999999999999
((smooth square) 2.3)                ;; 5.290000000066666
((n-fold-smoothed-f square 10) 2.3)  ;; 5.290000000666666
