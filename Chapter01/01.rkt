#lang racket

10        ;; 10

(+ 5 3 4) ;; 12

(- 9 1)   ;; 8

(/ 6 2)   ;; 3

(+ (* 2 4)
   (- 4 6))        ;; 6

(define a 3)       ;; empty output, a = 3
(define b (+ a 1)) ;; empty output, b = 4
(+ a b (* a b))    ;; 19
(= a b)            ;; #f - 3 != 4

(if
  (and 
    (> b a) 
    (< b (* a b))) 
  b 
  a)  ;; 4, <predicate> is #t

(cond
  ((= a 4) 6)              ;; #f
  ((= b 4) (+ 6 7 a))      ;; #t, 16
  (else 25))

(+ 2
   (if (> b a)
     b 
     a))     ;; 6, (+ 2 4)

;; 16, (* 4 4) -> 16
(* (cond
     ((> a b) a)       ;; #f
     ((< a b) b)       ;; #t, 4
     (else -1))
   (+ a 1))                ;; 4
