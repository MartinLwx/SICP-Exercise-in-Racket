#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))


(define (union-set set1 set2)
  (cond [(and (null? set1) (null? set2) '())]
        [(null? set1) set2]
        [(null? set2) set1]
        ;; We know one truth: the smaller element must be in the answer
        ;; So a simple approach is always add the smaller element :)
        [else (let ([x1 (car set1)]
                    [x2 (car set2)])
                (cond [(= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2)))]
                      [(> x1 x2) (cons x2 (union-set set1 (cdr set2)))]
                      [(< x1 x2) (cons x1 (union-set (cdr set1) set2))]))]))


;; Tests
(define set1 '(1 2 3 5))
(define set2 '(2 3 6))
(union-set set1 set2)
