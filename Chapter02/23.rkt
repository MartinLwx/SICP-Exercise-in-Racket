#lang racket

;; apply the procedure f on l
(define (for-each f l)
  (cond ((null? l) '())
        (else (f (car l)) (for-each f (cdr l)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
