#lang racket

;; compute pascal[row][col], both row and col should >= 1
(define (pascal row col)
  (cond ((or (< row 1) (< col 1) (> col row)) 0)      ;; out of boundary
        ((= 1 col) 1)                                 ;; the 1st column
        ((= row col)  1)                              ;; the edge of the triangle
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))
