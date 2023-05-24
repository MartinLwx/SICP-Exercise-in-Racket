#lang racket

;; fold utils
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

;; pair utils
(define (enumerate-interval low high)
   (if (> low high)
       '()
       (cons low (enumerate-interval (+ low 1) high))))

;; let's just try to put the queen at (row col)
(define (adjoin-position row col rest-of-queens)
  (cons (list row col) rest-of-queens))

;; an empty of positions
(define empty-board '())

;; basically, we need to check row, col, diagonal
;; the form of positions is [(x1 y1) (x2 y2) ... (xi yi)]
(define (safe? k positions)
  (let ([row (map car positions)]      ;; row, all numbers should be distinct
        [col (map cdr positions)]      ;; col, all numbers should be distinct
        [diag1 (map (lambda (p) (- (car p) (cadr p)))
                    positions)]
        [diag2 (map (lambda (p) (+ (car p) (cadr p)))
                    positions)])
        (and (not (check-duplicates row))
             (not (check-duplicates col))
             (not (check-duplicates diag1))
             (not (check-duplicates diag2)))))

;; the form of return value will be  ([(x1 y1) (x2 y2) ... (xi yi)]
;;                                    [ ...                       ]
;;                                    ....)
;; each [...] is a legal solution to place eight queens
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 ;; generate (1 2 3 4 5 ... board-size)
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; tests
(queens 1)     ;; length: 1
(queens 2)     ;; length: 0
(queens 3)     ;; length: 0
(queens 4)     ;; length: 2
