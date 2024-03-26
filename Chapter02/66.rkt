#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


;; For tests
(define (list->tree elements)
  (car (partial-tree elements (length elements))))


;; For tests
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ([left-size (quotient (- n 1) 2)])
        (let ([left-result (partial-tree elts left-size)])
          (let ([left-tree (car left-result)]
                [non-left-elts (cdr left-result)]
                [right-size (- n (+ left-size 1))])
            (let ([this-entry (car non-left-elts)]
                  [right-result (partial-tree (cdr non-left-elts)
                                              right-size)])
              (let ([right-tree (car right-result)]
                    [remaining-elts (cdr right-result)])
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))


;; Assumption: the `set-of-records` is structured as a binary tree
(define (key x) x)  ;; To make this easier, we assume key = value
(define (lookup given-key set-of-records)
  (cond [(null? set-of-records) false]
        [(= given-key (key (entry set-of-records)))
         (entry set-of-records)]
        [(< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records))]
        [(> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records))]))
;; The solution is similar to element-of-set?


;; Tests
(define t1 '(1 2 3 4 5 6 8))
(lookup 3 (list->tree t1))
(lookup 7 (list->tree t1))
