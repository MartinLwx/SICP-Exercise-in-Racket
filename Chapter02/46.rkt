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

;; tests
(define v1 (make-vect 1 2))
(define v2 (make-vect 5 6))
(add-vect v1 v2)
(sub-vect v1 v2)
(scale-vect v1 20)
