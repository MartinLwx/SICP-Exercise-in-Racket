#lang racket

;; fold utils
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

;; prime utils
(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
   (prime? (+ (car pair) (cadr pair))))

;; pair utils
(define (enumerate-interval low high)
   (if (> low high)
       '()
       (cons low (enumerate-interval (+ low 1) high))))

(define (make-pair-sum pair)
   (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i) (unique-pairs i))
                (enumerate-interval 1 n)))))

(define (unique-pairs n)
  (map (lambda (j) (list n j))
       (enumerate-interval 1 (- n 1))))

;; tests
(define n 5)  
(unique-pairs n)
(prime-sum-pairs n)
