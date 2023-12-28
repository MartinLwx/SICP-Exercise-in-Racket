#lang racket

(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        (else (and (eq? (car l1) (car l2))
                   (equal? (cdr l1) (cdr l2))))))


;; tests
(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a) list))
