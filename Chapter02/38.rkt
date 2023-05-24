#lang racket

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; let's say seq = (a b c d)
;; fold-right: (op (op (op (d init) c) b) a)
;; fold-left:  (op (op (op (init a) b) c) d)
;; if they are equal, the op should be commutativity and associative

;; tests
(fold-right / 1 (list 1 2 3))        ;; 3/1 -> 2/3 -> 1/(2/3) = 3/2
(fold-left / 1 (list 1 2 3))         ;; 1/1 -> 1/2 -> (1/2)/3 = 1/6
(fold-right list '() (list 1 2 3))   ;; '(1 (2 (3 ())))
(fold-left list '() (list 1 2 3))    ;; '(((() 1) 2) 3)
