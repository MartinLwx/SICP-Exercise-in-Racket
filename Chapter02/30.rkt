#lang racket

(define (square x)
  (* x x))

;; directly
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

;; use map
(define (square-tree-map tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree-map subtree)
             (square subtree)))
       tree))

;; tests
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))
