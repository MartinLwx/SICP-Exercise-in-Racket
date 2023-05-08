#lang sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

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

;; the next procedure returns 3 if its input is equal to 2
;; , and otherwise returns its input plus 2
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(timed-prime-test 1009)
;; old: 1009 *** 5
;; new: 1009 *** 5
(timed-prime-test 1013)
;; old: 1013 *** 3
;; new: 1013 *** 3
(timed-prime-test 1019)
;; old: 1019 *** 3
;; new: 1019 *** 2
(timed-prime-test 10007)
;; old: 10007 *** 8
;; new: 10007 *** 7
(timed-prime-test 10009)
;; old: 10009 *** 8
;; new: 10009 *** 6
(timed-prime-test 10037)
;; old: 10037 *** 6
;; new: 10037 *** 6
(timed-prime-test 100003)
;; old: 100003 *** 16
;; new: 100003 *** 14
(timed-prime-test 100019)
;; old: 100019 *** 17
;; new: 100019 *** 17
(timed-prime-test 100043)
;; old: 100043 *** 17
;; new: 100043 *** 15
(timed-prime-test 1000003)
;; old: 1000003 *** 53
;; new: 1000003 *** 45
(timed-prime-test 1000033)
;; old: 1000033 *** 53
;; new: 1000033 *** 40
(timed-prime-test 1000037)
;; old: 1000037 *** 54
;; new: 1000037 *** 42

;; apparently, the program does not run twice as fast
;; this may due to the extra if test in the next procedure
