#lang racket

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y)
  (/ (+ x y)
     2.0))

(define (search f neg-point pos-point)
  (let ([midpoint (average neg-point pos-point)])
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ([test-value (f midpoint)])
          (cond ((positive? test-value) (search f neg-point midpoint))
                ((negative? test-value) (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ([a-value (f a)]
        [b-value (f b)])
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

;; ---------------------------------------------------------

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; ---------------------------------------------------------
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)  ;; 1.6180327868852458
