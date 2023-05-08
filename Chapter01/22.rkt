#lang sicp   ;; we need to use (runtime)

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

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; find the prime in range [L, R] for odd integers
;; , return the smallest n primes (I add this argument for this procedure)
(define (search-for-primes l r n)
  (cond ((= n 0) (newline))      ;; just print a new line and do nothing :)
        ((= l r) (newline))
        ((and (odd? l) (prime? l)) (timed-prime-test l)
                                   (search-for-primes (+ 1 l) r (- n 1)))
        (else (search-for-primes (+ 1 l )
                                 r
                                 n))))

;; sqrt(10) is roughly 3.1622
(search-for-primes 1000 10000 3)
;; 1009 *** 5
;; 1013 *** 3
;; 1019 *** 3
(search-for-primes 10000 100000 3)
;; 10007 *** 8
;; 10009 *** 8
;; 10037 *** 6
(search-for-primes 100000 1000000 3)
;; 100003 *** 16
;; 100019 *** 17
;; 100043 *** 17
(search-for-primes 1000000 10000000 3)  
;; 1000003 *** 53
;; 1000033 *** 53
;; 1000037 *** 54
