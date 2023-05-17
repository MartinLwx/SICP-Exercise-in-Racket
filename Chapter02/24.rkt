#lang racket

;; (1 (2 (3 4)))
;; the interpretation of this as a tree:
;;     (1 (2 (3 4)))
;;     /    |
;;    1   (2 (3 4))
;;         |   \
;;         2  (3 4)
;;            /  \
;;           3   4
(list 1 (list 2 (list 3 4)))
