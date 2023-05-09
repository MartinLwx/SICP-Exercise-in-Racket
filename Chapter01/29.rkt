#lang racket

;; sum(term(i)) for i a ~ b, advance a using by the next procedure
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f
          (+ a (/ dx 2.0))     ;; start from a + 1/2 * dx
          add-dx
          b)
     dx))

(define (cube x) (* x x x))

;; the Simpson's Rule
;; y0 + 4y1 + 2y2 + 4y3 + 2y4 + ... + 2y_{n-2} + 4y_{n-1} + yn
;; y0 + yn + 4(y1 + y3 + ...y_{n-1}) + 2(y_2 + y4 + y_{n-2})
(define (simpson f a b n)
  (let* ([h (/ (- b a) n)]
         [factor (/ h 3.0)]
         [next (lambda (x) (+ x (* 2 h)))])
    (* factor
       (+ (f a)
          (f (+ a (* n h)))
          (* 4
             (sum f
                  (+ a h)                ;; start from y_1. i.e. a + h
                  next
                  (+ a (* (- n 1) h))))  ;; end in y_{n-1}. i.e. a + (n - 1) * h
          (* 2
             (sum f
                  (+ a h h)                  ;; start from y_2. i.e. a + 2h
                  next
                  (+ a (* (- n 2) h))))))))  ;; end in y_{n-2}. i.e. a + (n - 2) * h

(integral cube 0 1 0.001) ;; 0.249999875000001
(simpson cube 0 1 100)    ;; 0.25
(simpson cube 0 1 1000)   ;; 0.25
