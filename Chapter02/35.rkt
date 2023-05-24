#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatten t)
  (cond ((null? t) '())
        ((not (pair? t)) (list t))
        (else (append (flatten (car t)) (flatten (cdr t))))))

;; to make count-leaves work, we can flatten the tree first
(define (count-leaves t)
  (accumulate (lambda (x y)
                (+ (length x) y))
              0
              (map flatten t)))

;; tests
(flatten '(1 (2 3) 4 (5 (3 (1 2) 5))))
(map flatten '(1 (2 3) 4 (5 (3 (1 2) 5))))
(count-leaves '(1 (2 3) 4 (5 (3 (1 2) 5))))
(count-leaves '())
