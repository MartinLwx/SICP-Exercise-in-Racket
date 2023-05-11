#lang racket

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cdr l))

(define (midpoint-segment l)
  (let* ([p1 (start-segment l)]
         [p2 (end-segment l)]
         [new-x (/ (+ (x-point p1) (x-point p2))
                   2)]
         [new-y (/ (+ (y-point p1) (y-point p2))
                   2)])
    (make-point new-x new-y)))

;; tests
(define p1 (make-point 2 3))
(define p2 (make-point 10 11))
(define line-segment (make-segment p1 p2))
(define mid (midpoint-segment line-segment))
(print-point mid)    ;; (6, 7)
