#lang racket

;; install the required package so that we can test our code
;; $ raco pkg install sicp      # this will install the sicp package for racket
(require sicp-pict)             ;; see: https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html

;; note that the sicp-pict have built-in make-vect/make-segment/... procedure
;; to avoid conflicts, we may just use the built-in procedures.

(define (rorate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rorate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 0.0)))

(define (rorate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (beside painter1 painter2)
  (let* ([split-point (make-vect 0.5 0.0)]
         [paint-left (transform-painter painter1
                                        (make-vect 0.0 0.0)
                                        split-point
                                        (make-vect 0.0 1.0))]
         [paint-right (transform-painter painter2
                                         split-point
                                         (make-vect 1.0 0.0)
                                         (make-vect 0.5 1.0))])
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))

(define (below painter1 painter2)
  (let* ([split-point (make-vect 0.0 0.5)]
         [paint-left (transform-painter painter1
                                        split-point
                                        (make-vect 1.0 0.5)
                                        (make-vect 0.0 1.0))]
         [paint-right (transform-painter painter2
                                         (make-vect 0.0 0.0)
                                         (make-vect 1.0 0.0)
                                         split-point)])
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))


;; tests
(define foo (make-segment (make-vect 1 2) (make-vect 5 6)))
(define bar (make-segment (make-vect 1 2) (make-vect 5 6)))
(define o (make-vect 0 0))
(define e1 (make-vect 0 1))
(define e2 (make-vect 1 0))
(define frame (make-frame o e1 e2))

(paint (below einstein einstein))        ;; write a procedure
(paint (rorate90 (beside                 ;; by suitable rotation operations
                  (rotate270 einstein)
                  (rorate270 einstein))))
