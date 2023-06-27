#lang racket

;; install the required package so that we can test our code
;; $ raco pkg install sicp      # this will install the sicp package for racket
(require sicp-pict)             ;; see: https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html

;; note that the sicp-pict have built-in make-vect/make-segment/... procedure
;; to avoid conflicts, we may just use the built-in procedures.

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ([m (frame-coord-map frame)])
      (let ([new-origin (m origin)])
        (painter (make-frame new-origin
                             (vector-sub (m corner1) new-origin)
                             (vector-sub (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ;; new origin
                     (make-vect 1.0 1.0)   ;; new end of edge1
                     (make-vect 0.0 0.0))) ;; new end of edge2

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

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


;; tests
(define foo (make-segment (make-vect 1 2) (make-vect 5 6)))
(define bar (make-segment (make-vect 1 2) (make-vect 5 6)))
(define o (make-vect 0 0))
(define e1 (make-vect 0 1))
(define e2 (make-vect 1 0))
(define frame (make-frame o e1 e2))

(paint (flip-horiz einstein))
(paint einstein)
(paint (rotate180 einstein))
(paint (rotate270 einstein))
