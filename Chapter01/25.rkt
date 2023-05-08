#lang racket

(define (square x)
  (* x x))

;; when n is even: (b^{n/2})^2
;; when n is odd: b * b^{n-1}
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; the corrent one
;; when exp is even: (base^{exp/2})^2 % m
;; when exp is odd: (base * base^{exp-1}) % m
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

;; let's do substitution
;; when exp is even: keep computing (base^{exp/2})^2 and finally % m
;; when exp is odd: keep computing (base * base^{n-1}) and finally % m
;; it SEEMS both expmod and wrong-expmod get the same result and show no diffferences
;; but the woring-expmod procedure will evaluate (fast-expt base exp) first and get a super big value then we % m finally
;; try a big base and exp like (expmod 99999 99999999 32) vs (wrong-expmod 99999 99999999 32)
;; you will and the latter one use more memory and run much slower
(define (wrong-expmod base exp m)
  (remainder (fast-expt base exp) m))

;; randomly select a(a < n) and do fermat-test 
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; execute fermat-test n times
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
