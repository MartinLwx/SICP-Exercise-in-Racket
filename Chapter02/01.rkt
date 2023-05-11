#lang racket

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; let's design a better make-rat
;; (define (make-rat n d) (cons n d))

(define (make-rat n d)
  (let* ([rat (cond ((and (>= n 0) (> d 0)) (cons n d))                ;; both positive
                    ((and (<= n 0) (< d 0)) (cons (abs n) (abs d)))    ;; both negative
                    (else (cons (- (abs n)) (abs d))))]                ;; now we can assert that numerator or denominator is negative
         [g (gcd (numer rat) (denom rat))])                            ;; the let* enable us to use rat here              
    (cons (/ (numer rat) g) (/ (denom rat) g))))

;; tests
(print-rat (make-rat 4 6))
(print-rat (make-rat -4 -6))
(print-rat (make-rat -4 6))
(print-rat (make-rat 4 -6))
