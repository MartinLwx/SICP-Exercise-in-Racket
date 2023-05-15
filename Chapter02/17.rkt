#lang racket

(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

;; tests
(last-pair (list 23 72 149 34))
