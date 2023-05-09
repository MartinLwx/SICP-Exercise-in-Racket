#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f
          (+ a (/ dx 2.0))
          add-dx
          b)
     dx))

(define (cube x) (* x x x))

;;--------------------------------

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result
                          (term a)))))
  (iter a 0))

(define (integral-iter f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum-iter f
          (+ a (/ dx 2.0))
          add-dx
          b)
     dx))

(integral cube 0 1 0.001)      ;; 0.249999875000001
(integral-iter cube 0 1 0.001) ;; 0.24999987500000073
