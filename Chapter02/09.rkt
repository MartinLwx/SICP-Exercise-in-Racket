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

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (upper-bound p)
  (cdr p))

(define (lower-bound p)
  (car p))

(define (width p)
  (/ (- (upper-bound p) (lower-bound p))
     2))

;; tests
(define p (make-interval 2 4))
(define q (make-interval 3 5))
(define p-add-q (add-interval p q))
(define p-sub-q (sub-interval p q))
(define p-mul-q (mul-interval p q))
(define p-div-q (div-interval p q))
(width p)          ;; 1
(width q)          ;; 1

;; for addition -> [x1,x2] + [y1,y2] = [x1+y1,x2+y2]
;; width of [x1, x2] -> (x2 - x1) / 2
;; width of [y1, y2] -> (y2 - y1) / 2
;; width of [x1+y1, x2+y2] -> (y2 - y1 + x2 - x1) / 2
(width p-add-q)    ;; 2 = 1 + 1

;; for substraction -> [x1,x2] - [y1,y2] = [x1-y2,x2-y1]
;; width of [x1, x2] -> (x2 - x1) / 2
;; width of [y1, y2] -> (y2 - y1) / 2
;; width of [x1-y2,x2-y1] -> (x2 - x1 + y2 - y1) / 2
(width p-sub-q)    ;; 2 = 1 + 1
(width p-mul-q)    ;; 7
(width p-div-q)    ;; 0.466666666666
