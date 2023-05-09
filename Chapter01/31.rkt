#lang racket

(define (product term a next b)
  (if (> a b)
      1             ;; for product, we use 1 instead of 0
      (* (term a)   ;; not we change + to *
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result
                          (term a)))))
  (iter a 1))

(define (factorial n)
  (product (lambda (x) x)
           1
           (lambda (x) (+ 1 x))
           n))

(define (square x)
  (* x x))

;; compute the pi based on the formula(n should be an even number):
;; pi / 4 = (2 * 4 * 4 * 6 * 6 * 8...* n) / (3 * 3 * 5 * 5 * 7 * ... * (n - 1) * (n - 1))
;;        = (2 * n * (4 * 6 * 8 ...)(4 * 6 * 8 ...)) / ((3 * 5 * 7 * ...)(3 * 5 * 7 * ...))
(define (compute-pi n)
  (let ([nominator (* 2
                      n
                      (square (product (lambda (x) x)
                                    4
                                    (lambda (x) (+ 2 x))
                                    (- n 2))))]
        [denominator (square (product (lambda (x) x)
                                      3
                                      (lambda (x) (+ 2 x))
                                      (- n 1)))])
    (/ (* 4.0
          nominator)
       denominator)))


(product-iter (lambda (x) x) 1 (lambda (x) (+ 1 x)) 10)  ;; 3628800
(product (lambda (x) x) 1 (lambda (x) (+ 1 x)) 10)       ;; 3628800
(factorial 10)                                           ;; 3628800

;; it won't work for big n though
(compute-pi 50)    ;; 3.17316407023953
(compute-pi 100)   ;; 3.157339689217565
(compute-pi 150)   ;; 3.1520820239779614
