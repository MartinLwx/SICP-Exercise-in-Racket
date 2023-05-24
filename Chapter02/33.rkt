#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              '()             ;; initial
              sequence))

(define (append seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (length sequence)
  (accumulate +
              0
              sequence))

;; tests
(map (lambda (x) (* x x)) '(3 5 6 7))
(append '(2 4 1) '(1))
(length '(3 5 6 7))
