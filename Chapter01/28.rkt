#lang racket

(define (square x)
  (* x x))

;; return x^2 % m iff:
;; 1. x^2 % m == 1
;; 2. x == 1 or x == (m - 1)
;; else return 0
(define (check x m)
  (let ([tmp (remainder (square x) m)])
    (if (and (= tmp 1)
             (not (= x 1))
             (not (= x (- m 1))))
        0
        tmp)))

;; compute base^exp % m + check
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (check (expmod base (/ exp 2) m)
                            m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))


(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))    ;; a^p % p == a % p --> a^{p-1} % p == 1 % p
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

;; proof
(fast-prime? 17 100)    ;; #t
(fast-prime? 561 100)   ;; #f
(fast-prime? 1105 100)  ;; #f
