#lang racket

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

;; the illustration:
;;   (new-plus 4 5)
;;   (inc (new-plus 3 5))
;;   (inc (inc (new-plus 2 5)))
;;   (inc (inc (inc (new-plus 1 5))))
;;   (inc (inc (inc (inc (new-plus 0 5)))))
;;   (inc (inc (inc (inc 5))))
;;   (inc (inc (inc 6)))
;;   (inc (inc 7))
;;   (inc 8)
;;   (9)
;; so it's recursive
(define (new-plus a b)           ;; Note: I rename the first + to new-plus
  (if (= a 0)
      b
      (inc (new-plus (dec a) b))))

;; the illustration:
;;   (new-plus-2 4 5)
;;   (new-plus-2 3 6)
;;   (new-plus-2 2 7)
;;   (new-plus-2 1 8)
;;   (new-plus-2 0 9)
;;   9
;; so it's iterative
(define (new-plus-2 a b)         ;; Note: I rename the second + to new-plus
  (if (= a 0)
      b
      (+ (dec a) (inc b))))
