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

;; we can see the pattern: (1, i, 1), i starts from 2 and increase by 2 each time
(define (euler-expansion k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i)
               (let ([quotient-res (quotient i 3)]
                     [remainder-res (remainder i 3)])
                 (if (or (= remainder-res 0) (= remainder-res 2))
                     1
                     (* 2 quotient-res))))
             k))

(euler-expansion 10)        ;; 1.718279569892473
(euler-expansion 11)        ;; 1.7182835820895521
(euler-expansion 30)        ;; 1.7182818284590455
