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


;; Tests
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))


(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)
(encode (decode sample-message sample-tree) sample-tree)
