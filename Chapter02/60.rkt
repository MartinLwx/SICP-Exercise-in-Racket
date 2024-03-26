#lang racket

;; The element-of-set? does not need to change
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


;; The "set" allows duplicates, so we can just add this element
;; Time complexity: O(1)
(define (adjoin-set x set)
  (cons x set))


;; The "set" allows duplicates, so we can just merge them
;; Time complexity: O(n)
(define (union-set set1 set2)
  (append set1 set2))


;; The intersection-set does not need to change
;; Time complexity: O(n^2) still
(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) '()]
        [(element-of-set? (car set1) set2) (cons (car set1)
                                                 (intersection-set (cdr set1) set2))]
        [else (intersection-set (cdr set1) set2)]))

;; Tests
(define set1 '(1 1 2 2 3 4 5 5))
(define set2 '(2 2 3 3 3))
(element-of-set? 1 set1)
(adjoin-set 1 set1)
(union-set set1 set2)
(intersection-set set1 set2)   ;; expect: '(2 2 3)
