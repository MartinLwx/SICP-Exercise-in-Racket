#lang racket

;; install the required package so that we can test our code
;; $ raco pkg install sicp      # this will install the sicp package for racket
(require sicp-pict)             ;; see: https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html

;; I will use einstein rather than wave
(define wave einstein)

;; utils
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ([up (up-split painter (- n 1))]
            [right (right-split painter (- n 1))])
        (let ([top-left (beside up up)]
              [bottom-right (below right right)]
              [corner (corner-split painter (- n 1))])
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; speicify how we want to transform copies
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ([top (beside (tl painter) (tr painter))]
          [bottom (beside (bl painter) (br painter))])
      (below bottom top))))

(define (flipped-pairs painter)
  (let ([combine4 (square-of-four identity flip-vert
                                  identity flip-vert)])
    (combine4 painter)))

(define (square-limit painter n)
  (let ([combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)])
    (combine4 (corner-split painter n))))

;; Exercise 2.45
(define (split f1 f2)
  (define (helper painter n)
    (if (= n 0)
        painter
        (let ([smaller (helper painter (- n 1))])
          (f1 painter (f2 smaller smaller)))))
  helper)

(define right-split (split beside below))
(define up-split (split below beside))

;; tests
(paint (right-split einstein 1))
(paint ((split below beside) einstein 1))
