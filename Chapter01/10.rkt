#lang racket

;; the Ackermann's function
;; 1. A(x, 0) = 0
;; 2. A(0, y) = 2y  where y != 0
;; 3. A(x, 1) = 2   where x != 0
;; 4. A(x, y) = A(x - 1, A(x, y - 1))
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)      ;; 1024
(A 2 4)       ;; 65536
(A 3 3)       ;; 65536

(define (f n) (A 0 n))     ;; (f n) computes 2n by rule 2
(define (g n) (A 1 n))     ;; (g n) computes 2^n
                           ;;   (A 1 n)
                           ;; = (A 0 (A 1 n - 1))  by rule 4
                           ;; = (* 2 (A 1 n - 1))  by rule 2
                           ;; ...repeat...
                           ;; = (* 2 (* 2 ...  (A 1 1)) by rule 3, and (A 1 1) is equal to 2
                           ;; = 2^n

(define (h n) (A 2 n))     ;; (h n) computes 2^2^2...(repeat n times)
                           ;;   (A 2 n)
                           ;; = (A 1 (A 2 n - 1))  by rule 4
                           ;; = 2^{(A 2 n - 1)}    by (g n)
                           ;; = 2^{2^(A 2 n - 2)}
                           ;; = 2^2^2...(repeat n times)

(define (k n) (* 5 n n))   ;; (k n) computes 5n^2
