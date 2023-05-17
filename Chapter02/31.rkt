#lang racket

(define (square x)
  (* x x))

;; abstract procedure
(define (tree-map f tree)
  (define (helper subtree)
    (if (not (pair? subtree))
        (f subtree)
        (map helper subtree)))
  (helper tree))

;; tests
(define tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define (square-tree tree) (tree-map square tree))
(square-tree tree)
(tree-map (lambda (x) (+ x 1)) tree) 
