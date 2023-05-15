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

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; define a constructor make-center-percent
;; p in 0~100
(define (make-center-percent c p)
  (let ([p (/ p 100)])
    (make-interval (* c (- 1 p)) (* c (+ 1 p)))))

;; the center may be zero
(define (percent i)
  (if (= (center i) 0)
      0
      (* 100 (/ (- (upper-bound i) (center i))
                (center i)))))

;; tests
(define x (make-center-percent 99 12.3))
(percent x)
