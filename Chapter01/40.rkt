#lang racket

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

;; new-guess = f(old-guess)
;; the procedure will stop if the |new-guess - old-guess| <= tolerance
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

;; x' = x - g(x) / g'(x)
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a
          (* x x))
       (* b x)
       c)))

(newtons-method (cubic 2 3 4) 1)      ;; -1.6506291914330982
((cubic 2 3 4) -1.6506291914330982)   ;; 2.8752999980952154e-11
