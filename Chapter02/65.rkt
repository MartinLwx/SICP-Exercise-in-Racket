#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


;; tree -> ordered list
;; O(n)
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;; ordered list -> tree
;; O(n)
(define (list->tree elements)
  (car (partial-tree elements (length elements))))


;; Return: a pair
;; - car: constructed tree
;; - cdr: the list of elements not included in the tree
;; Note that the elts is a ordered list
;; O(n)
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

;; The function is taken from 2.62
;; It will union two sets (as ordered list)
(define (union-dedup ord1 ord2)
  (cond [(and (null? ord1) (null? ord2) '())]
        [(null? ord1) ord2]
        [(null? ord2) ord1]
        [else (let ([x1 (car ord1)]
                    [x2 (car ord2)])
                (cond [(= x1 x2) (cons x1 (union-dedup (cdr ord1) (cdr ord2)))]
                      [(> x1 x2) (cons x2 (union-dedup ord1 (cdr ord2)))]
                      [(< x1 x2) (cons x1 (union-dedup (cdr ord1) ord2))]))]))


;; It will compute the intersection of two sets (as ordered list)
(define (intersection-dedup ord1 ord2)
  (cond [(and (null? ord1) (null? ord2) '())]
        [(null? ord1) '()]
        [(null? ord2) '()]
        [else (let ([x1 (car ord1)]
                    [x2 (car ord2)])
                (cond [(= x1 x2) (cons x1 (intersection-dedup (cdr ord1) (cdr ord2)))]
                      [(> x1 x2) (intersection-dedup ord1 (cdr ord2))]
                      [(< x1 x2) (intersection-dedup (cdr ord1) ord2)]))]))


(define (union-set t1 t2)
  (let* ([nt1 (tree->list-2 t1)]         ;; O(n)
         [nt2 (tree->list-2 t2)]         ;; O(n)
         [merged (union-dedup nt1 nt2)]) ;; O(n)
    (list->tree merged)))                ;; O(n)


(define (intersection-set t1 t2)
  (let* ([nt1 (tree->list-2 t1)]                ;; O(n)
         [nt2 (tree->list-2 t2)]                ;; O(n)
         [merged (intersection-dedup nt1 nt2)]) ;; O(n)
    (list->tree merged)))                       ;; O(n)


;; Tests
(define t1 '(1 2 3 4 5 6 8))
(define t2 '(0 2 4 6 7))
(tree->list-2 (union-set (list->tree t1)
                         (list->tree t2)))
(tree->list-2 (intersection-set (list->tree t1)
                                (list->tree t2)))
