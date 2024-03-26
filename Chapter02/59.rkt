#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


(define (union-set set1 set2)
  (cond [(and (null? set1) (null? set2)) '()] ;; Q1: Both of them are empty?
        ;; Q2: Does one of them is empty?
        [(null? set1) set2]
        [(null? set2) set1]
        ;; Recursive decomposition just like intersection-set :)
        [(element-of-set? (car set1) set2) (union-set (cdr set1) set2)]
        [else (cons (car set1) (union-set (cdr set1) set2))]))

;; Tests
(define set1 '(1 2 3 5))
(define set2 '(2 3 5 6))
(union-set set1 set2)   ;; expect: '(1 2 3 5 6)
