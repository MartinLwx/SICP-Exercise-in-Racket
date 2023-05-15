#lang racket

(define nil '())

(define (square x)
  (* x x))

;; first definition
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

;; second definition
(define (another-square-list items)
  (map square items))

;; tests
(square-list (list 1 2 3 4))
(another-square-list (list 1 2 3 4))
