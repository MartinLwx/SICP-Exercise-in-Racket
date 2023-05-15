#lang racket

(define nil '())

(define (square x)
  (* x x))

;; it will produce the answer list in reverse order
;; because we always cons "the square of the first element" to the previously calculated result
;; , and we process the list from the beginning
;; e.g. previously calculated result: 1, "the square of the first element": 4, and we get a '(4 1)
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))


(define (wrong-square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer                      ;; answer is a list 
                    (square (car things))))))   ;; (square ...) is a number(primitive)
  (iter items nil))                             ;; that's why we get '((((() . 1) . 4) . 9) . 16)

;; tests
(square-list (list 1 2 3 4))             ;; '(16 9 4 1)
(wrong-square-list (list 1 2 3 4))       ;; '((((() . 1) . 4) . 9) . 16)
