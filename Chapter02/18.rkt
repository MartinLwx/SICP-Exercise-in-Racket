#lang racket

(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list (car l)))))

;; tests
(reverse (list 23 72 149 34))
(reverse '())
(reverse '(2))
