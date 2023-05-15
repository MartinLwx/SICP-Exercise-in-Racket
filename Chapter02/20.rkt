#lang racket


;; we can use the built-in filter procedure
(define (same-parity first . remains)
  (if (odd? first)
      (cons first (filter odd? remains))
      (cons first (filter even? remains))))

;; tests
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
