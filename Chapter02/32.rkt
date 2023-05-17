#lang racket

;; explanation: the rest is all subsets WITHOUT (car s),
;; to form a bigger subset, we can choose to pick or not pick (car s)
;; then all subsets of s are 1. subsets of (cdr s) without (car s)
;;                           2. subsets of (cdr s) plus (car s)
;; this explains why we pass a lambda function to add (car s) to all sets in rest
(define (subsets s)
  (if (null? s)
      (list '())
      (let ([rest (subsets (cdr s))])
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; tests
(subsets '(1 2 3))
