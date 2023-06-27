#lang racket

;; install the required package so that we can test our code
;; $ raco pkg install sicp      # this will install the sicp package for racket
(require sicp-pict)             ;; see: https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html

;; I will use einstein rather than wave
(define wave einstein)

;; utils
(define (flipped-pairs painter)
  (let ([painter2 (beside painter (flip-vert painter))])
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (right-split painter (- n 1))])
        (beside painter (below smaller smaller)))))

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

;; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (up-split painter (- n 1))])
        (below painter (beside smaller smaller)))))

;; tests
(paint (up-split einstein 1))
(paint (up-split einstein 2)) 
