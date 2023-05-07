#lang racket

;; recursive process
(define (recursive-f n)
  (cond ((< n 3) n)
        (else (+ (recursive-f (- n 1))
                 (* 2
                    (recursive-f (- n 2)))
                 (* 3
                    (recursive-f (- n 3)))))))


(recursive-f 3)
(recursive-f 4)
(recursive-f 5)

;; iterative process
(define (iterative-f n)
  (cond ((< n 3) n)
        (else (iterative-f-helper 2 1 0 3 n))))

;; x - f(n - 1)
;; y - f(n - 2)
;; z - f(n - 3)
(define (iterative-f-helper x y z counter max-count)
  (if (> counter max-count)
      x
      (iterative-f-helper (+ x (* 2 y) (* 3 z))   ;; the new "x" is the computed value
                          x                       ;; the new "y" is the origal "x"
                          y                       ;; the new "z" is the original "y"
                          (+ 1 counter)
                          max-count)))

;; use racket's trace to see the iterative process
;; (require racket/trace)
;; (trace iterative-f-helper)
(iterative-f 3)
(iterative-f 4)
(iterative-f 5)
