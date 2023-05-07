#lang racket

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; let's draw (count-change 11)
;;                             (count-change 11)
;;                                    |
;;                                (cc 11 5)
;;                                /       \
;;                          (cc 11 4)    (cc -39 5)
;;                          /        \         \
;;                    (cc 11 3)    (cc -14 4)   0
;;                   /         \             \
;;              (cc 11 2)       (cc 1 3)      0
;;               /       \          |   \
;;       (cc 11 1)       (cc 6 2)  (cc 1 2) (cc -9 3)
;;         /    \            | \
;;    (cc 11 0)(cc 10 1) (cc 6 1)(cc 1 2)           ;; well, let's stop. Draw this on paper with a pen will be better


;; the trace procedure will come in handy
(require racket/trace)
(trace cc)
(count-change 11)

;; the order of growth of the space should be O(n).
;; i.e. always use the first coin will give us the most deep path
;; the number of steps used by this process is O(n^5).
;; see the proof in https://billthelizard.blogspot.com/2009/12/sicp-exercise-114-counting-change.html
