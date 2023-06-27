#lang racket

;; vec utils
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

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

;; frame utils
(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame-1 frame)
  (car frame))

(define (edge1-frame-1 frame)
  (cadr frame))

(define (edge2-frame-1 frame)
  (caddr frame))


(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-2 frame)
  (car frame))

(define (edge1-frame-2 frame)
  (cadr frame))

(define (edge2-frame-2 frame)
  (cddr frame))

;; tests
(define foo (make-frame-1 (make-vect 1 2)
                          (make-vect 5 6)
                          (make-vect 10 23)))
(origin-frame-1 foo)
(edge1-frame-1 foo)
(edge2-frame-1 foo)
(define bar (make-frame-2 (make-vect 1 2)
                          (make-vect 5 6)
                          (make-vect 10 23)))
(origin-frame-2 bar)
(edge1-frame-2 bar)
(edge2-frame-2 bar)
