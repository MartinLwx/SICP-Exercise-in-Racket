#lang racket

;; 1 mobile consist of 2 branches
(define (make-mobile left right)
  (list left right))

;; 1 branch = length + a number(weight) or another mobile
(define (make-branch length structure)
  (list length structure))

;; a
;; m - mobile
;; b - branch
(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cadr b))

;; b
(define (total-weight m)
  (cond ((null? m) 0)
        ((not (pair? m)) m)
        (else (let* ([lbranch (left-branch m)]
                     [rbranch (right-branch m)]
                     [lmobile (branch-structure lbranch)]
                     [rmobile (branch-structure rbranch)]
                     [lweight (total-weight lmobile)]
                     [rweight (total-weight rmobile)])
                (+ lweight rweight)))))

;; c
(define (is-balanced m)
  (cond ((null? m) #t)
        ((not (pair? m)) #t)
        (else (let* ([lbranch (left-branch m)]
                     [rbranch (right-branch m)]
                     [llength (branch-length lbranch)]
                     [rlength (branch-length rbranch)]
                     [lmobile (branch-structure lbranch)]
                     [rmobile (branch-structure rbranch)]
                     [lweight (total-weight lmobile)]
                     [rweight (total-weight rmobile)])
                (and (is-balanced lmobile) (is-balanced rmobile) (= (* llength lweight)
                                                                    (* rlength rweight)))))))

;; d
;; if we change the representation of mobiles, we only need to change the selectors

;; tests
;;          top
;;      (1)/  \(3)
;;        c   32
;;    (1)/
;;      m
;; (3) /  \ (2)
;;    2    3
(define a (make-branch 3 2))
(define b (make-branch 2 3))
(define m (make-mobile a b))
(define c (make-branch 1 m))
(define d (make-branch 3 32))
(define top (make-mobile c d))

(branch-structure (left-branch m))
(branch-structure (right-branch m))

(newline)

(total-weight m)
(total-weight top)
;; (require racket/trace)
;; (trace is-balanced)
;; (trace total-weight)
(is-balanced m)
(is-balanced top) 
