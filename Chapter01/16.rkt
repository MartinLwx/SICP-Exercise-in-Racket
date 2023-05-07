#lang racket

(define (even? n)
  (= (remainder n 2) 0))

;; compute b^n
(define (iter-fast-expt b n)
  (iter-fast-expt-helper b n 1))

;; invariant: a * b ^ n won't change
;; when n is even --> $ab^n = a(b^2)^{n/2}$. i.e. $a'=a,b'=b^2,n'=n/2$
;; when n is odd  ---> $ab^n=ab*b^{n-1}=ab*(b^2)^{(n-1)/2}$. i.e. $a'=ab,b'=b^2,n'=(n-1)/2$
(define (iter-fast-expt-helper b n a)
  (cond ((= n 0) a)
        ((even? n) (iter-fast-expt-helper (* b b)
                                          (/ n 2)
                                          a))
        (else (iter-fast-expt-helper (* b b)
                                     (/ (- n 1) 2)
                                     (* b a)))))
(require racket/trace)
(trace iter-fast-expt-helper)
(iter-fast-expt 2 4)
(iter-fast-expt 2 7)
(iter-fast-expt 2 10)
