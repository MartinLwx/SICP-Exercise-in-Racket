#lang racket

;; Interval Arithmetic

(define (add-interval x y)
   (make-interval (+ (lower-bound x) (lower-bound y))
                  (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; check if y spans zero
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (<= 0 (upper-bound y)))
      (error "y spans zero")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define (upper-bound p)
  (cdr p))

(define (lower-bound p)
  (car p))

(define (width p)
  (/ (- (upper-bound p) (lower-bound p))
     2))

;; tests
(define q (make-interval 3 5))
(define p (make-interval -2 4))
(div-interval q p)
