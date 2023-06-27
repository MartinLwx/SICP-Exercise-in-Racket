#lang racket

;; install the required package so that we can test our code
;; $ raco pkg install sicp      # this will install the sicp package for racket
(require sicp-pict)             ;; see: https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html

;; note that the sicp-pict have built-in make-vect/make-segment/... procedure
;; to avoid conflicts, we may just use the built-in procedures.


;; Exercise 2.49
;; a. draws the outline of the designated frame
(define (outline some_frame)
  (let* ([bottom-left (frame-origin frame)]
         [top-left (frame-edge1 frame)]
         [bottom-right (frame-edge2 frame)]
         [top-right (make-vect (- (vector-xcor bottom-right)
                                  (vector-xcor bottom-left))
                               (- (vector-ycor top-left)
                                  (vector-ycor bottom-left)))])
   (segments->painter 
    (list 
     (make-segment bottom-left top-left)
     (make-segment bottom-left bottom-right)
     (make-segment top-left top-right)
     (make-segment bottom-right top-right)))))

;; b. draws an "X" by connecting opposite corners of the frame
(define (X-painter frame)
   (let* ([bottom-left (frame-origin frame)]
          [top-left (frame-edge1 frame)]
          [bottom-right (frame-edge2 frame)]
          [top-right (make-vect (- (vector-xcor bottom-right)
                                  (vector-xcor bottom-left))
                               (- (vector-ycor top-left)
                                  (vector-ycor bottom-left)))])
     (segments->painter
      (list (make-segment bottom-left top-right)
            (make-segment bottom-right top-left)))))

;; c. draws a diamond shape by connecting the midpoints of the sides of the frame
(define (diamond frame)
   (let* ([bottom-left (frame-origin frame)]
          [top-left (frame-edge1 frame)]
          [bottom-right (frame-edge2 frame)]
          [top-right (make-vect (- (vector-xcor bottom-right)
                                  (vector-xcor bottom-left))
                               (- (vector-ycor top-left)
                                  (vector-ycor bottom-left)))]
          [mid-left   (vector-scale 0.5 (vector-add bottom-left top-left))]
          [mid-top    (vector-scale 0.5 (vector-add top-left top-right))]
          [mid-right  (vector-scale 0.5 (vector-add bottom-right top-right))]
          [mid-bottom (vector-scale 0.5 (vector-add bottom-left bottom-right))])
     (segments->painter
      (list (make-segment mid-left mid-top)
            (make-segment mid-top mid-right)
            (make-segment mid-right mid-bottom)
            (make-segment mid-bottom mid-left)))))

;; d. the wave painter
;; It's tedious :(. Let's forget about this

;; tests
(define foo (make-segment (make-vect 1 2) (make-vect 5 6)))
(define bar (make-segment (make-vect 1 2) (make-vect 5 6)))
(define o (make-vect 0 0))
(define e1 (make-vect 0 1))
(define e2 (make-vect 1 0))
(define frame (make-frame o e1 e2))
((frame-coord-map frame) (segment-start foo))  ;; (1 2) -> (0 0) + (1 * (1 1)) + (2 * (1 -1)) -> (3 -1)

(paint (outline frame))
(paint (X-painter frame))
(paint (diamond frame))
