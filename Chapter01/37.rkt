#lang racket

;; n and d are procedures of one argument that return the Ni
;; and Di of the terms of the continued fraction
(define (cont-frac n d k)
  (define (cont-frac-helper n d i)
    (cond ((= i k) (/ (n i)
                      (d i)))
          (else (/ (n i)
                   (+ (d i)
                      (cont-frac-helper n d (+ i 1)))))))
  (cont-frac-helper n d 1))

;; let's also define a iterative process
(define (cont-frac-iter n d k)
  (define (iter count val)
    (cond ((= count 0) val)
          (else (iter (- count 1)
                      (/ (n (- count 1))
                         (+ (d (- count 1))
                            val))))))
  (iter k 0))

;; let's also define a helper function to compute golden ratio
;; in k-term finite continued fraction
(define (golden-ratio k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

(define (golden-ratio-iter k)
  (cont-frac-iter (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  k))

(golden-ratio 1)                    ;; 1.0
(golden-ratio-iter 1)               ;; 1.0
(golden-ratio 10)                   ;; 0.6179775280898876
(golden-ratio-iter 10)              ;; 0.6179775280898876
(golden-ratio 11)                   ;; 0.6180555555555556
(golden-ratio-iter 11)              ;; 0.6180555555555556
(golden-ratio 15)                   ;; 0.6180344478216819
(golden-ratio-iter 15)              ;; 0.6180344478216819
(golden-ratio 20)                   ;; 0.6180339850173578
(golden-ratio-iter 20)              ;; 0.6180339850173578
