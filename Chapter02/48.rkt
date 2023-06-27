#lang racket

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

;; well, let's define a general procedure that will apply a procedure elemeng-wise
;; and finally return a new vector
(define (general-vect v1 v2 f)
  (make-vect (f (xcor-vect v1) (xcor-vect v2))
             (f (ycor-vect v1) (ycor-vect v2))))

(define (add-vect v1 v2)
  (general-vect v1 v2 +))

(define (sub-vect v1 v2)
  (general-vect v1 v2 -))

(define (scale-vect v s)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; Exercise 2.48
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

;; tests
(define v1 (make-vect 1 2))
(define v2 (make-vect 5 6))
(define foo (make-segment v1 v2))
(start-segment foo)
(end-segment foo)
