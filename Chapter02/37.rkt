#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (v)
           (matrix-*-vector cols v))
         m)))

;; tests
(define v '(1 2))
(define w '(5 6))
(define matrix '((1 2) (4 5) (6 7)))

(map * v w)
(transpose matrix)
(matrix-*-vector matrix v)
(matrix-*-matrix matrix (transpose matrix))
