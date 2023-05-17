#lang racket

(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list (car l)))))

;; invariants: the argument of deep-reverse are always a list
(define (deep-reverse l)
  (cond ((null? l) '())     ;; base case
        (else (let ([remains (deep-reverse (cdr l))])
                (if (pair? (car l))
                    ;; the arguments of append procedure should be lists too
                    (append remains (list (deep-reverse (car l))))
                    (append remains (list (car l))))))))

;; tests
(define x (list (list 1 2) (list 3 4)))
(reverse x)
(deep-reverse x)
(newline)
(deep-reverse '(2 3))
(deep-reverse '((2 3)))
(deep-reverse '((2 3) 1))
(deep-reverse '(5 (2 3) 1))
(deep-reverse '((4 2) (2 3) 1))
(deep-reverse '(5 (2 3) (5 2)))
