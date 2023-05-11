#lang racket

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cdr l))

(define (midpoint-segment l)
  (let* ([p1 (start-segment l)]
         [p2 (end-segment l)]
         [new-x (/ (+ (x-point p1) (x-point p2))
                   2)]
         [new-y (/ (+ (y-point p1) (y-point p2))
                   2)])
    (make-point new-x new-y)))

;; let's try to define a rectangle
;; the 1st try: a rectangle is define as the top-left point and the bottom-right point
;; for simplicity, I only consider rectangles that have sides parallel to x-axis or y-axis. 

;; the low-level abstraction
(define (make-rectangle p1 p2)
  (cons p1 p2))

(define (point-one some_rectangle)
  (car some_rectangle))

(define (point-two some_rectangle)
  (cdr some_rectangle))

;; the mid-level abstraction
(define (width some_rectangle)
  (abs (- (x-point (point-one some_rectangle))
          (x-point (point-two some_rectangle)))))

(define (height some_rectangle)
  (abs (- (y-point (point-one some_rectangle))
          (y-point (point-two some_rectangle)))))

;; the high-level abstraction
(define (perimeter some_rectangle)
  (let ([w (width some_rectangle)]
        [h (height some_rectangle)])
    (+ w w h h )))
  
(define (area some_rectangle)
  (let ([w (width some_rectangle)]
        [h (height some_rectangle)])
    (* w h)))

;; another representation for rectagnel: top-left point + width + height
;; I comment with these with semicolons
;; (define (make-rectangle p w h)
;;   (list p w h))
;; 
;; (define (width some_rectangle)
;;   (car (cdr some_rectangle)))
;; 
;; (define (height some_rectangle)
;;   (car (cdr (cdr some_rectangle))))

;; tests
(define p1 (make-point 2 3))
(define p2 (make-point 9 6))
(define rectangle (make-rectangle p1 p2))    ;; representation1
;; (define rectangle (make-rectangle p1 7 3))   ;; representation2
(perimeter rectangle)                        ;; perimeter: (7 + 3) * 2
(area rectangle)                             ;; area: 7 * 3
