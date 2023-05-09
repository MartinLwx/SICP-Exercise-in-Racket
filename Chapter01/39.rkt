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

;; the pattern:
;; Di: 1 3 5 ...
;; Ni: x, x^2, x^2 ...
;; note that in the denominator, we use "-" instead of "+", we can negate Ni
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- 0 (* x x))))
             (lambda (i) (- (* 2.0 i) 1))
             k))

(tan-cf 45 10)       ;; -2.7977843227854704
(tan-cf 45 50)       ;; 1.5755031274991265
(tan-cf 45 100)      ;; 1.6197751905438595

;; compare to the real answer
(tan 45)             ;; 1.6197751905438615
