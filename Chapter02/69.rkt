#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))


(define (leaf? object)
  (eq? (car object) 'leaf))


(define (symbol-leaf x) (cadr x))


(define (weight-leaf x) (caddr x))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define (left-branch tree) (car tree))


(define (right-branch tree) (cadr tree))


(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))


(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (choose-branch bit branch)
  (cond [(= bit 0) (left-branch branch)]
        [(= bit 1) (right-branch branch)]
        [else (error "bad bit -- CHOOSE-BRANCH" bit)]))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ([next-branch
               (choose-branch (car bits) current-branch)])
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))  ;; Return to the root node
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))


(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(< (weight x) (weight (car set))) (cons x set)]
        [else (cons (car set)
                    (adjoin-set x (cdr set)))]))


;; This function constructs an initial ordered set of leaves
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let [(pair (car pairs))]
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


;; Returns: a Huffman tree
(define (successive-merge leafs)
  (cond [(null? leafs) '()]                ;; no leaf
        [(null? (cdr leafs)) (car leafs)]  ;; one leaf
        [else (let* ([first (car leafs)]
                     [second (cadr leafs)]
                     [merge-tree (make-code-tree first second)])
                (successive-merge (adjoin-set merge-tree
                                              (cddr leafs))))]))

  
;; Tests
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))


(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define pairs '(('A 4) ('B 2) ('D 1) ('C 1)))
(generate-huffman-tree pairs)
