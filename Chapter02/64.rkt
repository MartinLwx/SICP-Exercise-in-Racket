#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))


;; Return: a pair
;; - car: constructed tree
;; - cdr: the list of elements not included in the tree
;; Note that the elts is a ordered list
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ([left-size (quotient (- n 1) 2)])
        (let ([left-result (partial-tree elts left-size)])    ;; use the first `left-size` elements to create the left subtree
          (let ([left-tree (car left-result)]
                [non-left-elts (cdr left-result)]
                [right-size (- n (+ left-size 1))])           ;; calculate the corresponding the right subtree's size
            (let ([this-entry (car non-left-elts)]            ;; the smallest value is the root
                  [right-result (partial-tree (cdr non-left-elts)   ;; create the right subtree
                                              right-size)])
              (let ([right-tree (car right-result)]
                    [remaining-elts (cdr right-result)])
                (cons (make-tree this-entry left-tree right-tree)   ;; make a tree
                      remaining-elts))))))))

;; a. The partial-tree procedure will split elts into two halves
;;    , which are used to create the left subtree and the right subtree.
;; (list->tree '(1 3 5 7 9 11))
;;             5
;;           1   9
;;            3 7 11


;; b. Let's use the Master Theorem again :)
;;    T(n) = 2 * T(n / 2) + O(1)  ==> T(n) = O(n)
