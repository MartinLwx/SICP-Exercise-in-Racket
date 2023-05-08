#lang sicp

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

;; randomly select a(a < n) and do fermat-test 
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; execute fermat-test n times
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

;; the problem description doesn't say how many times should we try
(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(timed-prime-test 1009)
;; old: 1009 *** 5
;; new: 1009 *** 20
(timed-prime-test 1013)
;; old: 1013 *** 3
;; new: 1013 *** 16
(timed-prime-test 1019)
;; old: 1019 *** 3
;; new: 1019 *** 17
(timed-prime-test 10007)
;; old: 10007 *** 8
;; new: 10007 *** 20
(timed-prime-test 10009)
;; old: 10009 *** 8
;; new: 10009 *** 20
(timed-prime-test 10037)
;; old: 10037 *** 6
;; new: 10037 *** 22
(timed-prime-test 100003)
;; old: 100003 *** 16
;; new: 100003 *** 23
(timed-prime-test 100019)
;; old: 100019 *** 17
;; new: 100019 *** 25
(timed-prime-test 100043)
;; old: 100043 *** 17
;; new: 100043 *** 25
(timed-prime-test 1000003)
;; old: 1000003 *** 53
;; new: 1000003 *** 26
(timed-prime-test 1000033)
;; old: 1000033 *** 53
;; new: 1000033 *** 27
(timed-prime-test 1000037)
;; old: 1000037 *** 54
;; new: 1000037 *** 28

;; when the input prime number is small, prime? is faster
