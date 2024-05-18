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


;; Produces the list of bits that gives the encoded message
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


;; Returns the list of bits that encoded a given symbol
;; according to a given tree
;; Note: symbol is not null
;; Solution: Visit all node in this tree and generate a set.
;;           Each element in this set is (symbol the-corresponding-path)
(define (encode-symbol symbol tree)
  (define (search current-node path)   ;; Returns a set (in list form)
    (if (leaf? current-node)
        (list (list (symbol-leaf current-node) path))
        (append (search (left-branch current-node) (append path '(0)))
                (search (right-branch current-node) (append path '(1))))))
  (let* ([set (search tree '())]
         [ans (assoc symbol set)])
    (if (not ans)
        (error "Symbol is not in this Huffman tree")
        (cadr ans))))


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
(define pairs-5 '((A 1) (B 2) (C 4) (D 8) (E 16)))
(define n-5 (generate-huffman-tree pairs-5))
(encode '(E) n-5)
(encode '(A) n-5)

(define pairs-10 '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)))
(define n-10 (generate-huffman-tree pairs-10))
(encode '(J) n-10)
(encode '(A) n-10)


;; For a general n, the most frequent symbol needs 1 bit.
;; And the least frequent symbol needs (n - 1) bits
