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

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(define (show-interval A)
  (newline)
  (display (lower-bound A))
  (display "*****")
  (display (upper-bound A)))

;; tests
(define A (make-interval 2 4))
(define B (make-interval 2 4))
(show-interval (par1 A A))
(show-interval (par2 A B))
