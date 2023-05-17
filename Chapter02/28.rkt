#lang racket

(define (fringe tree)
  (cond ((null? tree) '())
        (else (let ([remains (fringe (cdr tree))])
                (if (pair? (car tree))
                    (append (fringe (car tree)) remains)
                    (append (list (car tree)) remains))))))

;; tests
(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))
(newline)
(fringe '(1 (2 3)))
(fringe '(1 5 (3 2) 2 (4 10)))
